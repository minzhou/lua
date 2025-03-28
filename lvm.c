/*
** $Id: lvm.c,v 2.268.1.1 2017/04/19 17:39:34 roberto Exp $
** Lua 虚拟机实现文件
** 作者: R. Ierusalimschy, L. H. de Figueiredo, W. Celes
** 创建日期: 2018年
*/

#define lvm_c
#define LUA_CORE

#include "lprefix.h"

#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "ltm.h"
#include "lvm.h"

/* 表元方法链的最大长度限制，用于避免循环 */
#define MAXTAGLOOP	2000

/*
** 'l_intfitsf' 检查给定的整数是否可以精确转换为浮点数
** 用于比较操作。如果所有整数都能精确表示为浮点数，则未定义
*/
#if !defined(l_intfitsf)

/* 浮点数尾数位数 */
#define NBM		(l_mathlim(MANT_DIG))

/*
** 检查某些整数是否可能无法精确表示为浮点数
** 即检查 (maxinteger >> NBM) > 0 (这意味着 (1 << NBM) <= maxinteger)
** (移位操作分步进行以避免超过整数大小)
** 最坏情况下，NBM == 113 (long double) 且 sizeof(integer) == 32
*/
#if ((((LUA_MAXINTEGER >> (NBM / 4)) >> (NBM / 4)) >> (NBM / 4)) \
	>> (NBM - (3 * (NBM / 4))))  >  0

#define l_intfitsf(i)  \
  (-((lua_Integer)1 << NBM) <= (i) && (i) <= ((lua_Integer)1 << NBM))

#endif

#endif

/*
** 尝试将值转换为浮点数
** 浮点数情况已由 'tonumber' 宏处理
*/
int luaV_tonumber_ (const TValue *obj, lua_Number *n) {
  TValue v;
  if (ttisinteger(obj)) {  /* 是整数? */
    *n = cast_num(ivalue(obj));  /* 直接转换 */
    return 1;
  }
  else if (cvt2num(obj) &&  /* 字符串可转换为数字? */
            luaO_str2num(svalue(obj), &v) == vslen(obj) + 1) {
    *n = nvalue(&v);  /* 转换 'luaO_str2num' 的结果为浮点数 */
    return 1;
  }
  else
    return 0;  /* 转换失败 */
}

/*
** 尝试将值转换为整数，根据 'mode' 进行舍入:
** mode == 0: 只接受整数值
** mode == 1: 向下取整
** mode == 2: 向上取整
*/
int luaV_tointeger (const TValue *obj, lua_Integer *p, int mode) {
  TValue v;
 again:
  if (ttisfloat(obj)) {  /* 是浮点数? */
    lua_Number n = fltvalue(obj);
    lua_Number f = l_floor(n);
    if (n != f) {  /* 不是整数值? */
      if (mode == 0) return 0;  /* 如果模式要求整数值则失败 */
      else if (mode > 1)  /* 需要向上取整? */
        f += 1;  /* 将向下取整转换为向上取整 */
    }
    return lua_numbertointeger(f, p);
  }
  else if (ttisinteger(obj)) {  /* 是整数? */
    *p = ivalue(obj);
    return 1;
  }
  else if (cvt2num(obj) &&
            luaO_str2num(svalue(obj), &v) == vslen(obj) + 1) {
    obj = &v;
    goto again;  /* 转换 'luaO_str2num' 的结果为整数 */
  }
  return 0;  /* 转换失败 */
}

/*
** 尝试将 'for' 循环的限制值转换为整数，保持循环语义
** (以下说明假设步长为非负数；对于负步长类似)
** 如果限制值可以向下取整转换为整数，则使用该值
** 否则，检查是否可以转换为数字。如果数字太大，可以设置为 LUA_MAXINTEGER
** 如果数字太负，循环不应运行，因为任何初始整数值都大于限制值
** 所以将限制值设为 LUA_MININTEGER
** 'stopnow' 修正极端情况，当初始值为 LUA_MININTEGER 时
** 这种情况下 LUA_MININTEGER 限制值仍会运行循环一次
*/
static int forlimit (const TValue *obj, lua_Integer *p, lua_Integer step,
                     int *stopnow) {
  *stopnow = 0;  /* 通常允许循环运行 */
  if (!luaV_tointeger(obj, p, (step < 0 ? 2 : 1))) {  /* 不适合整数? */
    lua_Number n;  /* 尝试转换为浮点数 */
    if (!tonumber(obj, &n)) /* 不能转换为浮点数? */
      return 0;  /* 不是数字 */
    if (luai_numlt(0, n)) {  /* 如果为真，浮点数大于最大整数 */
      *p = LUA_MAXINTEGER;
      if (step < 0) *stopnow = 1;
    }
    else {  /* 浮点数小于最小整数 */
      *p = LUA_MININTEGER;
      if (step >= 0) *stopnow = 1;
    }
  }
  return 1;
}

/*
** 完成表访问 'val = t[key]'
** 如果 'slot' 为 NULL，'t' 不是表；否则，'slot' 指向 t[k] 条目(必须为 nil)
*/
void luaV_finishget (lua_State *L, const TValue *t, TValue *key, StkId val,
                      const TValue *slot) {
  int loop;  /* 计数器，避免无限循环 */
  const TValue *tm;  /* 元方法 */
  for (loop = 0; loop < MAXTAGLOOP; loop++) {
    if (slot == NULL) {  /* 't' 不是表? */
      lua_assert(!ttistable(t));
      tm = luaT_gettmbyobj(L, t, TM_INDEX);
      if (ttisnil(tm))
        luaG_typeerror(L, t, "index");  /* 没有元方法 */
      /* 否则尝试元方法 */
    }
    else {  /* 't' 是表 */
      lua_assert(ttisnil(slot));
      tm = fasttm(L, hvalue(t)->metatable, TM_INDEX);  /* 表的元方法 */
      if (tm == NULL) {  /* 没有元方法? */
        setnilvalue(val);  /* 结果为 nil */
        return;
      }
      /* 否则尝试元方法 */
    }
    if (ttisfunction(tm)) {  /* 元方法是函数? */
      luaT_callTM(L, tm, t, key, val, 1);  /* 调用它 */
      return;
    }
    t = tm;  /* 否则尝试访问 'tm[key]' */
    if (luaV_fastget(L,t,key,slot,luaH_get)) {  /* 快速路径? */
      setobj2s(L, val, slot);  /* 完成 */
      return;
    }
    /* 否则重复(尾调用 'luaV_finishget') */
  }
  luaG_runerror(L, "'__index' chain too long; possible loop");
}

/*
** 完成表赋值 't[key] = val'
** 如果 'slot' 为 NULL，'t' 不是表。否则，'slot' 指向条目 't[key]'
** 或指向 'luaO_nilobject' 如果不存在该条目
** (slot 处的值必须为 nil，否则 'luaV_fastset' 会完成工作)
*/
void luaV_finishset (lua_State *L, const TValue *t, TValue *key,
                     StkId val, const TValue *slot) {
  int loop;  /* 计数器，避免无限循环 */
  for (loop = 0; loop < MAXTAGLOOP; loop++) {
    const TValue *tm;  /* '__newindex' 元方法 */
    if (slot != NULL) {  /* 't' 是表? */
      Table *h = hvalue(t);  /* 保存 't' 表 */
      lua_assert(ttisnil(slot));  /* 旧值必须为 nil */
      tm = fasttm(L, h->metatable, TM_NEWINDEX);  /* 获取元方法 */
      if (tm == NULL) {  /* 没有元方法? */
        if (slot == luaO_nilobject)  /* 没有前一个条目? */
          slot = luaH_newkey(L, h, key);  /* 创建一个 */
        /* 没有元方法且(现在)存在给定键的条目 */
        setobj2t(L, cast(TValue *, slot), val);  /* 设置新值 */
        invalidateTMcache(h);
        luaC_barrierback(L, h, val);
        return;
      }
      /* 否则尝试元方法 */
    }
    else {  /* 不是表；检查元方法 */
      if (ttisnil(tm = luaT_gettmbyobj(L, t, TM_NEWINDEX)))
        luaG_typeerror(L, t, "index");
    }
    /* 尝试元方法 */
    if (ttisfunction(tm)) {
      luaT_callTM(L, tm, t, key, val, 0);
      return;
    }
    t = tm;  /* 否则在 'tm' 上重复赋值 */
    if (luaV_fastset(L, t, key, slot, luaH_get, val))
      return;  /* 完成 */
    /* 否则循环 */
  }
  luaG_runerror(L, "'__newindex' chain too long; possible loop");
}

/*
** 比较两个字符串 'ls' x 'rs'，如果 'ls' 小于/等于/大于 'rs'
** 则返回小于/等于/大于零的整数
** 代码有点复杂，因为它允许字符串中包含 '\0'
** 并且对每个字符串段使用 'strcoll' (以尊重区域设置)
*/
static int l_strcmp (const TString *ls, const TString *rs) {
  const char *l = getstr(ls);
  size_t ll = tsslen(ls);
  const char *r = getstr(rs);
  size_t lr = tsslen(rs);
  for (;;) {  /* 对每个段 */
    int temp = strcoll(l, r);
    if (temp != 0)  /* 不相等? */
      return temp;  /* 完成 */
    else {  /* 字符串在 '\0' 之前相等 */
      size_t len = strlen(l);  /* 两个字符串中第一个 '\0' 的索引 */
      if (len == lr)  /* 'rs' 结束? */
        return (len == ll) ? 0 : 1;  /* 检查 'ls' */
      else if (len == ll)  /* 'ls' 结束? */
        return -1;  /* 'ls' 小于 'rs' ('rs' 未结束) */
      /* 两个字符串都比 'len' 长；继续比较 '\0' 之后的部分 */
      len++;
      l += len; ll -= len; r += len; lr -= len;
    }
  }
}

/*
** 检查整数 'i' 是否小于浮点数 'f'
** 如果 'i' 可以精确表示为浮点数 ('l_intfitsf')，则作为浮点数比较
** 否则，如果 'f' 超出整数范围，结果是显而易见的
** 否则，将它们作为整数比较
** (当 'i' 没有浮点数表示时，要么 'f' 远离 'i'，要么 'f' 没有精度用于小数部分；
** 无论哪种情况，'f' 如何截断都无关紧要)
** 当 'f' 为 NaN 时，比较必须返回 false
*/
static int LTintfloat (lua_Integer i, lua_Number f) {
#if defined(l_intfitsf)
  if (!l_intfitsf(i)) {
    if (f >= -cast_num(LUA_MININTEGER))  /* -minint == maxint + 1 */
      return 1;  /* f >= maxint + 1 > i */
    else if (f > cast_num(LUA_MININTEGER))  /* minint < f <= maxint ? */
      return (i < cast(lua_Integer, f));  /* 作为整数比较 */
    else  /* f <= minint <= i (或 'f' 是 NaN)  -->  not(i < f) */
      return 0;
  }
#endif
  return luai_numlt(cast_num(i), f);  /* 作为浮点数比较 */
}

/*
** 检查整数 'i' 是否小于等于浮点数 'f'
** 参见前一个函数的注释
*/
static int LEintfloat (lua_Integer i, lua_Number f) {
#if defined(l_intfitsf)
  if (!l_intfitsf(i)) {
    if (f >= -cast_num(LUA_MININTEGER))  /* -minint == maxint + 1 */
      return 1;  /* f >= maxint + 1 > i */
    else if (f >= cast_num(LUA_MININTEGER))  /* minint <= f <= maxint ? */
      return (i <= cast(lua_Integer, f));  /* 作为整数比较 */
    else  /* f < minint <= i (或 'f' 是 NaN)  -->  not(i <= f) */
      return 0;
  }
#endif
  return luai_numle(cast_num(i), f);  /* 作为浮点数比较 */
}

/*
** 返回 'l < r'，用于数字
*/
static int LTnum (const TValue *l, const TValue *r) {
  if (ttisinteger(l)) {
    lua_Integer li = ivalue(l);
    if (ttisinteger(r))
      return li < ivalue(r);  /* 都是整数 */
    else  /* 'l' 是整数，'r' 是浮点数 */
      return LTintfloat(li, fltvalue(r));  /* l < r ? */
  }
  else {
    lua_Number lf = fltvalue(l);  /* 'l' 必须是浮点数 */
    if (ttisfloat(r))
      return luai_numlt(lf, fltvalue(r));  /* 都是浮点数 */
    else if (luai_numisnan(lf))  /* 'r' 是整数，'l' 是浮点数 */
      return 0;  /* NaN < i 总是 false */
    else  /* 没有 NaN，(l < r)  <-->  not(r <= l) */
      return !LEintfloat(ivalue(r), lf);  /* not (r <= l) ? */
  }
}

/*
** 返回 'l <= r'，用于数字
*/
static int LEnum (const TValue *l, const TValue *r) {
  if (ttisinteger(l)) {
    lua_Integer li = ivalue(l);
    if (ttisinteger(r))
      return li <= ivalue(r);  /* 都是整数 */
    else  /* 'l' 是整数，'r' 是浮点数 */
      return LEintfloat(li, fltvalue(r));  /* l <= r ? */
  }
  else {
    lua_Number lf = fltvalue(l);  /* 'l' 必须是浮点数 */
    if (ttisfloat(r))
      return luai_numle(lf, fltvalue(r));  /* 都是浮点数 */
    else if (luai_numisnan(lf))  /* 'r' 是整数，'l' 是浮点数 */
      return 0;  /*  NaN <= i 总是 false */
    else  /* 没有 NaN，(l <= r)  <-->  not(r < l) */
      return !LTintfloat(ivalue(r), lf);  /* not (r < l) ? */
  }
}

/*
** 主要小于操作；返回 'l < r'
*/
int luaV_lessthan (lua_State *L, const TValue *l, const TValue *r) {
  int res;
  if (ttisnumber(l) && ttisnumber(r))  /* 两个操作数都是数字? */
    return LTnum(l, r);
  else if (ttisstring(l) && ttisstring(r))  /* 都是字符串? */
    return l_strcmp(tsvalue(l), tsvalue(r)) < 0;
  else if ((res = luaT_callorderTM(L, l, r, TM_LT)) < 0)  /* 没有元方法? */
    luaG_ordererror(L, l, r);  /* 错误 */
  return res;
}

/*
** 主要小于等于操作；返回 'l <= r'
** 如果需要元方法且没有 '__le'，尝试 '__lt'，基于
** l <= r 当且仅当 !(r < l) (假设全序)
** 如果元方法在此替换期间产生，继续需要知道它(否定 r<l 的结果)；
** 调用状态中的 CIST_LEQ 位保持该信息
*/
int luaV_lessequal (lua_State *L, const TValue *l, const TValue *r) {
  int res;
  if (ttisnumber(l) && ttisnumber(r))  /* 两个操作数都是数字? */
    return LEnum(l, r);
  else if (ttisstring(l) && ttisstring(r))  /* 都是字符串? */
    return l_strcmp(tsvalue(l), tsvalue(r)) <= 0;
  else if ((res = luaT_callorderTM(L, l, r, TM_LE)) >= 0)  /* 尝试 'le' */
    return res;
  else {  /* 尝试 'lt': */
    L->ci->callstatus |= CIST_LEQ;  /* 标记它正在为 'le' 做 'lt' */
    res = luaT_callorderTM(L, r, l, TM_LT);
    L->ci->callstatus ^= CIST_LEQ;  /* 清除标记 */
    if (res < 0)
      luaG_ordererror(L, l, r);
    return !res;  /* 结果被否定 */
  }
}

/*
** Lua 值相等性的主要操作；返回 't1 == t2'
** L == NULL 表示原始相等性(无元方法)
*/
int luaV_equalobj (lua_State *L, const TValue *t1, const TValue *t2) {
  const TValue *tm;
  if (ttype(t1) != ttype(t2)) {  /* 不是相同的变体? */
    if (ttnov(t1) != ttnov(t2) || ttnov(t1) != LUA_TNUMBER)
      return 0;  /* 只有数字可以在不同变体下相等 */
    else {  /* 两个不同变体的数字 */
      lua_Integer i1, i2;  /* 作为整数比较 */
      return (tointeger(t1, &i1) && tointeger(t2, &i2) && i1 == i2);
    }
  }
  /* 值有相同的类型和变体 */
  switch (ttype(t1)) {
    case LUA_TNIL: return 1;
    case LUA_TNUMINT: return (ivalue(t1) == ivalue(t2));
    case LUA_TNUMFLT: return luai_numeq(fltvalue(t1), fltvalue(t2));
    case LUA_TBOOLEAN: return bvalue(t1) == bvalue(t2);  /* true 必须为 1 !! */
    case LUA_TLIGHTUSERDATA: return pvalue(t1) == pvalue(t2);
    case LUA_TLCF: return fvalue(t1) == fvalue(t2);
    case LUA_TSHRSTR: return eqshrstr(tsvalue(t1), tsvalue(t2));
    case LUA_TLNGSTR: return luaS_eqlngstr(tsvalue(t1), tsvalue(t2));
    case LUA_TUSERDATA: {
      if (uvalue(t1) == uvalue(t2)) return 1;
      else if (L == NULL) return 0;
      tm = fasttm(L, uvalue(t1)->metatable, TM_EQ);
      if (tm == NULL)
        tm = fasttm(L, uvalue(t2)->metatable, TM_EQ);
      break;  /* 将尝试元方法 */
    }
    case LUA_TTABLE: {
      if (hvalue(t1) == hvalue(t2)) return 1;
      else if (L == NULL) return 0;
      tm = fasttm(L, hvalue(t1)->metatable, TM_EQ);
      if (tm == NULL)
        tm = fasttm(L, hvalue(t2)->metatable, TM_EQ);
      break;  /* 将尝试元方法 */
    }
    default:
      return gcvalue(t1) == gcvalue(t2);
  }
  if (tm == NULL)  /* 没有元方法? */
    return 0;  /* 对象不同 */
  luaT_callTM(L, tm, t1, t2, L->top, 1);  /* 调用元方法 */
  return !l_isfalse(L->top);
}

/* 用于 'luaV_concat' 的宏，确保 'o' 处的元素是字符串 */
#define tostring(L,o)  \
	(ttisstring(o) || (cvt2str(o) && (luaO_tostring(L, o), 1)))

#define isemptystr(o)	(ttisshrstring(o) && tsvalue(o)->shrlen == 0)

/* 将栈中从 top - n 到 top - 1 的字符串复制到缓冲区 */
static void copy2buff (StkId top, int n, char *buff) {
  size_t tl = 0;  /* 已复制的大小 */
  do {
    size_t l = vslen(top - n);  /* 正在复制的字符串长度 */
    memcpy(buff + tl, svalue(top - n), l * sizeof(char));
    tl += l;
  } while (--n > 0);
}

/*
** 字符串连接的主要操作：连接栈中从 'L->top - total' 到 'L->top - 1' 的 'total' 个值
*/
void luaV_concat (lua_State *L, int total) {
  lua_assert(total >= 2);
  do {
    StkId top = L->top;
    int n = 2;  /* 本次处理的操作数数量(至少2个) */
    if (!(ttisstring(top-2) || cvt2str(top-2)) || !tostring(L, top-1))
      luaT_trybinTM(L, top-2, top-1, top-2, TM_CONCAT);
    else if (isemptystr(top - 1))  /* 第二个操作数是空字符串? */
      cast_void(tostring(L, top - 2));  /* 结果是第一个操作数 */
    else if (isemptystr(top - 2)) {  /* 第一个操作数是空字符串? */
      setobjs2s(L, top - 2, top - 1);  /* 结果是第二个操作数 */
    }
    else {
      /* 至少有两个非空字符串值；获取尽可能多的操作数 */
      size_t tl = vslen(top - 1);
      TString *ts;
      /* 收集总长度和字符串数量 */
      for (n = 1; n < total && tostring(L, top - n - 1); n++) {
        size_t l = vslen(top - n - 1);
        if (l >= (MAX_SIZE/sizeof(char)) - tl)
          luaG_runerror(L, "string length overflow");
        tl += l;
      }
      if (tl <= LUAI_MAXSHORTLEN) {  /* 结果是短字符串? */
        char buff[LUAI_MAXSHORTLEN];
        copy2buff(top, n, buff);  /* 复制字符串到缓冲区 */
        ts = luaS_newlstr(L, buff, tl);
      }
      else {  /* 长字符串；直接复制到最终结果 */
        ts = luaS_createlngstrobj(L, tl);
        copy2buff(top, n, getstr(ts));
      }
      setsvalue2s(L, top - n, ts);  /* 创建结果 */
    }
    total -= n-1;  /* 已连接 n 个字符串得到 1 个新字符串 */
    L->top -= n-1;  /* 弹出 n-1 个操作数 */
  } while (total > 1);  /* 重复直到只剩一个字符串 */
}

/*
** 主要操作 'ra' = #rb'
*/
void luaV_objlen (lua_State *L, StkId ra, const TValue *rb) {
  const TValue *tm;
  switch (ttype(rb)) {
    case LUA_TTABLE: {
      Table *h = hvalue(rb);
      tm = fasttm(L, h->metatable, TM_LEN);
      if (tm) break;  /* 有元方法? 跳出 switch 调用它 */
      setivalue(ra, luaH_getn(h));  /* 否则使用原始长度 */
      return;
    }
    case LUA_TSHRSTR: {
      setivalue(ra, tsvalue(rb)->shrlen);
      return;
    }
    case LUA_TLNGSTR: {
      setivalue(ra, tsvalue(rb)->u.lnglen);
      return;
    }
    default: {  /* 尝试元方法 */
      tm = luaT_gettmbyobj(L, rb, TM_LEN);
      if (ttisnil(tm))  /* 没有元方法? */
        luaG_typeerror(L, rb, "get length of");
      break;
    }
  }
  luaT_callTM(L, tm, rb, rb, ra, 1);
}

/*
** 整数除法；返回 'm // n'，即 floor(m/n)
** C 除法截断其结果(向零舍入)
** 'floor(q) == trunc(q)' 当 'q >= 0' 或 'q' 为整数时
** 否则 'floor(q) == trunc(q) - 1'
*/
lua_Integer luaV_div (lua_State *L, lua_Integer m, lua_Integer n) {
  if (l_castS2U(n) + 1u <= 1u) {  /* 特殊情况: -1 或 0 */
    if (n == 0)
      luaG_runerror(L, "attempt to divide by zero");
    return intop(-, 0, m);   /* n==-1; 避免 0x80000...//-1 溢出 */
  }
  else {
    lua_Integer q = m / n;  /* 执行 C 除法 */
    if ((m ^ n) < 0 && m % n != 0)  /* 'm/n' 会是负的非整数? */
      q -= 1;  /* 修正不同舍入的结果 */
    return q;
  }
}

/*
** 整数取模；返回 'm % n'
** (假设 C 的 '%' 对负操作数遵循 C99 行为。参见前一个关于 luaV_div 的注释)
*/
lua_Integer luaV_mod (lua_State *L, lua_Integer m, lua_Integer n) {
  if (l_castS2U(n) + 1u <= 1u) {  /* 特殊情况: -1 或 0 */
    if (n == 0)
      luaG_runerror(L, "attempt to perform 'n%%0'");
    return 0;   /* m % -1 == 0; 避免 0x80000...%-1 溢出 */
  }
  else {
    lua_Integer r = m % n;
    if (r != 0 && (m ^ n) < 0)  /* 'm/n' 会是负的非整数? */
      r += n;  /* 修正不同舍入的结果 */
    return r;
  }
}

/* 整数位数 */
#define NBITS	cast_int(sizeof(lua_Integer) * CHAR_BIT)

/*
** 左移操作。(右移只是否定 'y')
*/
lua_Integer luaV_shiftl (lua_Integer x, lua_Integer y) {
  if (y < 0) {  /* 右移? */
    if (y <= -NBITS) return 0;
    else return intop(>>, x, -y);
  }
  else {  /* 左移 */
    if (y >= NBITS) return 0;
    else return intop(<<, x, y);
  }
}

/*
** 检查原型 'p' 中的缓存闭包是否可以重用，即
** 是否存在一个具有相同上值的缓存闭包
** 新闭包需要这些上值
*/
static LClosure *getcached (Proto *p, UpVal **encup, StkId base) {
  LClosure *c = p->cache;
  if (c != NULL) {  /* 有缓存的闭包? */
    int nup = p->sizeupvalues;
    Upvaldesc *uv = p->upvalues;
    int i;
    for (i = 0; i < nup; i++) {  /* 检查是否有正确的上值 */
      TValue *v = uv[i].instack ? base + uv[i].idx : encup[uv[i].idx]->v;
      if (c->upvals[i]->v != v)
        return NULL;  /* 错误的上值；不能重用闭包 */
    }
  }
  return c;  /* 返回缓存的闭包(如果没有缓存闭包则为 NULL) */
}

/*
** 创建新的 Lua 闭包，将其压入栈，并初始化其上值
** 注意，如果原型已经是黑色的(这意味着 'cache' 已经被 GC 清除)
** 则不会缓存闭包
*/
static void pushclosure (lua_State *L, Proto *p, UpVal **encup, StkId base,
                         StkId ra) {
  int nup = p->sizeupvalues;
  Upvaldesc *uv = p->upvalues;
  int i;
  LClosure *ncl = luaF_newLclosure(L, nup);
  ncl->p = p;
  setclLvalue(L, ra, ncl);  /* 在栈中锚定新闭包 */
  for (i = 0; i < nup; i++) {  /* 填充其上值 */
    if (uv[i].instack)  /* 上值引用局部变量? */
      ncl->upvals[i] = luaF_findupval(L, base + uv[i].idx);
    else  /* 从封闭函数获取上值 */
      ncl->upvals[i] = encup[uv[i].idx];
    ncl->upvals[i]->refcount++;
    /* 新闭包是白色的，所以这里不需要屏障 */
  }
  if (!isblack(p))  /* 缓存不会破坏 GC 不变量? */
    p->cache = ncl;  /* 保存到缓存以供重用 */
}

/*
** 完成被 yield 中断的操作码的执行
*/
void luaV_finishOp (lua_State *L) {
  CallInfo *ci = L->ci;
  StkId base = ci->u.l.base;
  Instruction inst = *(ci->u.l.savedpc - 1);  /* 被中断的指令 */
  OpCode op = GET_OPCODE(inst);
  switch (op) {  /* 完成其执行 */
    case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV: case OP_IDIV:
    case OP_BAND: case OP_BOR: case OP_BXOR: case OP_SHL: case OP_SHR:
    case OP_MOD: case OP_POW:
    case OP_UNM: case OP_BNOT: case OP_LEN:
    case OP_GETTABUP: case OP_GETTABLE: case OP_SELF: {
      setobjs2s(L, base + GETARG_A(inst), --L->top);
      break;
    }
    case OP_LE: case OP_LT: case OP_EQ: {
      int res = !l_isfalse(L->top - 1);
      L->top--;
      if (ci->callstatus & CIST_LEQ) {  /* "<=" 使用 "<" 代替? */
        lua_assert(op == OP_LE);
        ci->callstatus ^= CIST_LEQ;  /* 清除标记 */
        res = !res;  /* 否定结果 */
      }
      lua_assert(GET_OPCODE(*ci->u.l.savedpc) == OP_JMP);
      if (res != GETARG_A(inst))  /* 条件失败? */
        ci->u.l.savedpc++;  /* 跳过跳转指令 */
      break;
    }
    case OP_CONCAT: {
      StkId top = L->top - 1;  /* 'luaT_trybinTM' 被调用时的 top */
      int b = GETARG_B(inst);      /* 第一个要连接的元素 */
      int total = cast_int(top - 1 - (base + b));  /* 还有待连接 */
      setobj2s(L, top - 2, top);  /* 将 TM 结果放在正确位置 */
      if (total > 1) {  /* 还有要连接的元素? */
        L->top = top - 1;  /* top 是最后一个元素之后(在 top-2) */
        luaV_concat(L, total);  /* 连接它们(可能再次 yield) */
      }
      /* 将最终结果移到最终位置 */
      setobj2s(L, ci->u.l.base + GETARG_A(inst), L->top - 1);
      L->top = ci->top;  /* 恢复 top */
      break;
    }
    case OP_TFORCALL: {
      lua_assert(GET_OPCODE(*ci->u.l.savedpc) == OP_TFORLOOP);
      L->top = ci->top;  /* 修正 top */
      break;
    }
    case OP_CALL: {
      if (GETARG_C(inst) - 1 >= 0)  /* nresults >= 0? */
        L->top = ci->top;  /* 调整结果 */
      break;
    }
    case OP_TAILCALL: case OP_SETTABUP: case OP_SETTABLE:
      break;
    default: lua_assert(0);
  }
}

/*
** {==================================================================
** 函数 'luaV_execute': 主解释器循环
** ===================================================================
*/

/*
** 'luaV_execute' 中常用任务的宏
*/

#define RA(i)	(base+GETARG_A(i))
#define RB(i)	check_exp(getBMode(GET_OPCODE(i)) == OpArgR, base+GETARG_B(i))
#define RC(i)	check_exp(getCMode(GET_OPCODE(i)) == OpArgR, base+GETARG_C(i))
#define RKB(i)	check_exp(getBMode(GET_OPCODE(i)) == OpArgK, \
	ISK(GETARG_B(i)) ? k+INDEXK(GETARG_B(i)) : base+GETARG_B(i))
#define RKC(i)	check_exp(getCMode(GET_OPCODE(i)) == OpArgK, \
	ISK(GETARG_C(i)) ? k+INDEXK(GETARG_C(i)) : base+GETARG_C(i))

/* 执行跳转指令 */
#define dojump(ci,i,e) \
  { int a = GETARG_A(i); \
    if (a != 0) luaF_close(L, ci->u.l.base + a - 1); \
    ci->u.l.savedpc += GETARG_sBx(i) + e; }

/* 对于测试指令，执行其后的跳转指令 */
#define donextjump(ci)	{ i = *ci->u.l.savedpc; dojump(ci, i, 1); }

#define Protect(x)	{ {x;}; base = ci->u.l.base; }

#define checkGC(L,c)  \
	{ luaC_condGC(L, L->top = (c),  /* 活动值的限制 */ \
                         Protect(L->top = ci->top));  /* 恢复 top */ \
           luai_threadyield(L); }

/* 获取指令并准备其执行 */
#define vmfetch()	{ \
  i = *(ci->u.l.savedpc++); \
  if (L->hookmask & (LUA_MASKLINE | LUA_MASKCOUNT)) \
    Protect(luaG_traceexec(L)); \
  ra = RA(i); /* 警告: 任何栈重分配都会使 'ra' 无效 */ \
  lua_assert(base == ci->u.l.base); \
  lua_assert(base <= L->top && L->top < L->stack + L->stacksize); \
}

#define vmdispatch(o)	switch(o)
#define vmcase(l)	case l:
#define vmbreak		break

/*
** 'luaV_gettable' 的副本，但保护对潜在元方法的调用
** (元方法可能重新分配栈)
*/
#define gettableProtected(L,t,k,v)  { const TValue *slot; \
  if (luaV_fastget(L,t,k,slot,luaH_get)) { setobj2s(L, v, slot); } \
  else Protect(luaV_finishget(L,t,k,v,slot)); }

/* 同样适用于 'luaV_settable' */
#define settableProtected(L,t,k,v) { const TValue *slot; \
  if (!luaV_fastset(L,t,k,slot,luaH_get,v)) \
    Protect(luaV_finishset(L,t,k,v,slot)); }

/* 虚拟机主执行循环 */
void luaV_execute (lua_State *L) {
  CallInfo *ci = L->ci;
  LClosure *cl;
  TValue *k;
  StkId base;
  ci->callstatus |= CIST_FRESH;  /* 'luaV_execute' 的新调用 */
 newframe:  /* 当帧改变(调用/返回)时的重入点 */
  lua_assert(ci == L->ci);
  cl = clLvalue(ci->func);  /* 函数闭包的局部引用 */
  k = cl->p->k;  /* 函数常量表的局部引用 */
  base = ci->u.l.base;  /* 函数基址的局部副本 */
  /* 解释器主循环 */
  for (;;) {
    Instruction i;
    /**
     * 指令说明
     * i = 0b 1 000000010 00000001 011101
     * 低6位：         011101     表示指令类型，这个是29 表示 OP_CONCAT
     * 7-14位（8位）： 00000001    表示A偏移位置
     * 15-23位（9位）：000000010   表示B偏移位置
     * 23-32位（9位）：000000010   表示B偏移位置
     */
    StkId ra;
    vmfetch();
    vmdispatch (GET_OPCODE(i)) { // 低6位是指令位
      vmcase(OP_MOVE) {
        setobjs2s(L, ra, RB(i));  /* 将值从一个寄存器移动到另一个 */
        vmbreak;
      }
      vmcase(OP_LOADK) {
        TValue *rb = k + GETARG_Bx(i);  /* 从常量表加载常量 */
        setobj2s(L, ra, rb);
        vmbreak;
      }
      vmcase(OP_LOADKX) {
        TValue *rb;
        lua_assert(GET_OPCODE(*ci->u.l.savedpc) == OP_EXTRAARG);
        rb = k + GETARG_Ax(*ci->u.l.savedpc++);  /* 从常量表加载扩展常量 */
        setobj2s(L, ra, rb);
        vmbreak;
      }
      vmcase(OP_LOADBOOL) {
        setbvalue(ra, GETARG_B(i));  /* 加载布尔值 */
        if (GETARG_C(i))
            ci->u.l.savedpc++;  /* 跳过下一条指令(如果 C) */
        vmbreak;
      }
      vmcase(OP_LOADNIL) {
        int b = GETARG_B(i);  /* 将连续的寄存器设置为 nil */
        do {
          setnilvalue(ra++);
        } while (b--);
        vmbreak;
      }
      vmcase(OP_GETUPVAL) {
        int b = GETARG_B(i);  /* 获取上值 */
        setobj2s(L, ra, cl->upvals[b]->v);
        vmbreak;
      }
      vmcase(OP_GETTABUP) {
        TValue *upval = cl->upvals[GETARG_B(i)]->v;  /* 从上值表中获取值 */
        TValue *rc = RKC(i);
        gettableProtected(L, upval, rc, ra);
        vmbreak;
      }
      vmcase(OP_GETTABLE) {
        StkId rb = RB(i);  /* 从表中获取值 */
        TValue *rc = RKC(i);
        gettableProtected(L, rb, rc, ra);
        vmbreak;
      }
      vmcase(OP_SETTABUP) {
        TValue *upval = cl->upvals[GETARG_A(i)]->v;  /* 设置上值表中的值 */
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        settableProtected(L, upval, rb, rc);
        vmbreak;
      }
      vmcase(OP_SETUPVAL) {
        UpVal *uv = cl->upvals[GETARG_B(i)];  /* 设置上值 */
        setobj(L, uv->v, ra);
        luaC_upvalbarrier(L, uv);
        vmbreak;
      }
      vmcase(OP_SETTABLE) {
        TValue *rb = RKB(i);  /* 设置表中的值 */
        TValue *rc = RKC(i);
        settableProtected(L, ra, rb, rc);
        vmbreak;
      }
      vmcase(OP_NEWTABLE) {
        int b = GETARG_B(i);  /* 创建新表 */
        int c = GETARG_C(i);
        Table *t = luaH_new(L);
        sethvalue(L, ra, t);
        if (b != 0 || c != 0)
          luaH_resize(L, t, luaO_fb2int(b), luaO_fb2int(c));
        checkGC(L, ra + 1);
        vmbreak;
      }
      vmcase(OP_SELF) {
        const TValue *aux;  /* 方法调用的准备 */
        StkId rb = RB(i);
        TValue *rc = RKC(i);
        TString *key = tsvalue(rc);  /* key 必须是字符串 */
        setobjs2s(L, ra + 1, rb);
        if (luaV_fastget(L, rb, key, aux, luaH_getstr)) {
          setobj2s(L, ra, aux);
        }
        else Protect(luaV_finishget(L, rb, rc, ra, aux));
        vmbreak;
      }
      vmcase(OP_ADD) {
        TValue *rb = RKB(i);  /* 加法运算 */
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (ttisinteger(rb) && ttisinteger(rc)) {
          lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
          setivalue(ra, intop(+, ib, ic));  /* 整数加法 */
        }
        else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          setfltvalue(ra, luai_numadd(L, nb, nc));  /* 浮点数加法 */
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_ADD)); }  /* 尝试元方法 */
        vmbreak;
      }
      vmcase(OP_SUB) {
        TValue *rb = RKB(i);  /* 减法运算 */
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (ttisinteger(rb) && ttisinteger(rc)) {
          lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
          setivalue(ra, intop(-, ib, ic));  /* 整数减法 */
        }
        else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          setfltvalue(ra, luai_numsub(L, nb, nc));  /* 浮点数减法 */
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_SUB)); }  /* 尝试元方法 */
        vmbreak;
      }
      vmcase(OP_MUL) {
        TValue *rb = RKB(i);  /* 乘法运算 */
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (ttisinteger(rb) && ttisinteger(rc)) {
          lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
          setivalue(ra, intop(*, ib, ic));  /* 整数乘法 */
        }
        else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          setfltvalue(ra, luai_nummul(L, nb, nc));  /* 浮点数乘法 */
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_MUL)); }  /* 尝试元方法 */
        vmbreak;
      }
      vmcase(OP_DIV) {  /* 浮点数除法(总是使用浮点数) */
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          setfltvalue(ra, luai_numdiv(L, nb, nc));  /* 执行浮点数除法 */
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_DIV)); }  /* 尝试元方法 */
        vmbreak;
      }
      vmcase(OP_BAND) {
        TValue *rb = RKB(i);  /* 按位与运算 */
        TValue *rc = RKC(i);
        lua_Integer ib; lua_Integer ic;
        if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
          setivalue(ra, intop(&, ib, ic));  /* 执行整数按位与 */
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_BAND)); }  /* 尝试元方法 */
        vmbreak;
      }
      vmcase(OP_BOR) {
        TValue *rb = RKB(i);  /* 按位或运算 */
        TValue *rc = RKC(i);
        lua_Integer ib; lua_Integer ic;
        if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
          setivalue(ra, intop(|, ib, ic));  /* 执行整数按位或 */
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_BOR)); }  /* 尝试元方法 */
        vmbreak;
      }
      vmcase(OP_BXOR) {
        TValue *rb = RKB(i);  /* 按位异或运算 */
        TValue *rc = RKC(i);
        lua_Integer ib; lua_Integer ic;
        if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
          setivalue(ra, intop(^, ib, ic));  /* 执行整数按位异或 */
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_BXOR)); }  /* 尝试元方法 */
        vmbreak;
      }
      vmcase(OP_SHL) {
        TValue *rb = RKB(i);  /* 左移运算 */
        TValue *rc = RKC(i);
        lua_Integer ib; lua_Integer ic;
        if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
          setivalue(ra, luaV_shiftl(ib, ic));  /* 执行左移 */
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_SHL)); }  /* 尝试元方法 */
        vmbreak;
      }
      vmcase(OP_SHR) {
        TValue *rb = RKB(i);  /* 右移运算 */
        TValue *rc = RKC(i);
        lua_Integer ib; lua_Integer ic;
        if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
          setivalue(ra, luaV_shiftl(ib, -ic));  /* 执行右移(通过负的左移实现) */
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_SHR)); }  /* 尝试元方法 */
        vmbreak;
      }
      vmcase(OP_MOD) {
        TValue *rb = RKB(i);  /* 取模运算 */
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (ttisinteger(rb) && ttisinteger(rc)) {
          lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
          setivalue(ra, luaV_mod(L, ib, ic));  /* 整数取模 */
        }
        else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          lua_Number m;
          luai_nummod(L, nb, nc, m);  /* 浮点数取模 */
          setfltvalue(ra, m);
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_MOD)); }  /* 尝试元方法 */
        vmbreak;
      }
      vmcase(OP_IDIV) {  /* 向下取整除法 */
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (ttisinteger(rb) && ttisinteger(rc)) {
          lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
          setivalue(ra, luaV_div(L, ib, ic));  /* 整数向下取整除法 */
        }
        else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          setfltvalue(ra, luai_numidiv(L, nb, nc));  /* 浮点数向下取整除法 */
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_IDIV)); }  /* 尝试元方法 */
        vmbreak;
      }
      vmcase(OP_POW) {
        TValue *rb = RKB(i);  /* 幂运算 */
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          setfltvalue(ra, luai_numpow(L, nb, nc));  /* 执行幂运算 */
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_POW)); }  /* 尝试元方法 */
        vmbreak;
      }
      vmcase(OP_UNM) {
        TValue *rb = RB(i);  /* 负号运算 */
        lua_Number nb;
        if (ttisinteger(rb)) {
          lua_Integer ib = ivalue(rb);
          setivalue(ra, intop(-, 0, ib));  /* 整数取负 */
        }
        else if (tonumber(rb, &nb)) {
          setfltvalue(ra, luai_numunm(L, nb));  /* 浮点数取负 */
        }
        else {
          Protect(luaT_trybinTM(L, rb, rb, ra, TM_UNM));  /* 尝试元方法 */
        }
        vmbreak;
      }
      vmcase(OP_BNOT) {
        TValue *rb = RB(i);  /* 按位非运算 */
        lua_Integer ib;
        if (tointeger(rb, &ib)) {
          setivalue(ra, intop(^, ~l_castS2U(0), ib));  /* 执行按位非 */
        }
        else {
          Protect(luaT_trybinTM(L, rb, rb, ra, TM_BNOT));  /* 尝试元方法 */
        }
        vmbreak;
      }
      vmcase(OP_NOT) {
        TValue *rb = RB(i);  /* 逻辑非运算 */
        int res = l_isfalse(rb);  /* 下一个赋值可能改变这个值 */
        setbvalue(ra, res);  /* 设置布尔结果 */
        vmbreak;
      }
      vmcase(OP_LEN) {
        Protect(luaV_objlen(L, ra, RB(i)));  /* 获取对象长度 */
        vmbreak;
      }
      vmcase(OP_CONCAT) {
        int b = GETARG_B(i);  /* 第一个要连接的元素 */
        int c = GETARG_C(i);  /* 最后一个要连接的元素 */
        StkId rb;
        L->top = base + c + 1;  /* 标记连接操作数的结束 */
        Protect(luaV_concat(L, c - b + 1));  /* 执行字符串连接 */
        ra = RA(i);  /* 'luaV_concat' 可能调用 TM 并移动栈 */
        rb = base + b;
        setobjs2s(L, ra, rb);  /* 将结果移动到目标位置 */
        checkGC(L, (ra >= rb ? ra + 1 : rb));  /* 检查是否需要 GC */
        L->top = ci->top;  /* 恢复 top */
        vmbreak;
      }
      vmcase(OP_JMP) {
        dojump(ci, i, 0);  /* 无条件跳转 */
        vmbreak;
      }
      vmcase(OP_EQ) {
        TValue *rb = RKB(i);  /* 相等性比较 */
        TValue *rc = RKC(i);
        Protect(
          if (luaV_equalobj(L, rb, rc) != GETARG_A(i))  /* 比较结果与预期不符? */
            ci->u.l.savedpc++;  /* 跳过跳转指令 */
          else
            donextjump(ci);  /* 执行跳转 */
        )
        vmbreak;
      }
      vmcase(OP_LT) {
        Protect(
          if (luaV_lessthan(L, RKB(i), RKC(i)) != GETARG_A(i))  /* 小于比较结果与预期不符? */
            ci->u.l.savedpc++;  /* 跳过跳转指令 */
          else
            donextjump(ci);  /* 执行跳转 */
        )
        vmbreak;
      }
      vmcase(OP_LE) {
        Protect(
          if (luaV_lessequal(L, RKB(i), RKC(i)) != GETARG_A(i))  /* 小于等于比较结果与预期不符? */
            ci->u.l.savedpc++;  /* 跳过跳转指令 */
          else
            donextjump(ci);  /* 执行跳转 */
        )
        vmbreak;
      }
      vmcase(OP_TEST) {
        if (GETARG_C(i) ? l_isfalse(ra) : !l_isfalse(ra))  /* 条件测试 */
            ci->u.l.savedpc++;  /* 跳过跳转指令 */
          else
          donextjump(ci);  /* 执行跳转 */
        vmbreak;
      }
      vmcase(OP_TESTSET) {
        TValue *rb = RB(i);  /* 条件测试并赋值 */
        if (GETARG_C(i) ? l_isfalse(rb) : !l_isfalse(rb))  /* 条件测试 */
          ci->u.l.savedpc++;  /* 跳过跳转指令 */
        else {
          setobjs2s(L, ra, rb);  /* 赋值 */
          donextjump(ci);  /* 执行跳转 */
        }
        vmbreak;
      }
      vmcase(OP_CALL) {
        int b = GETARG_B(i);  /* 参数数量 */
        int nresults = GETARG_C(i) - 1;  /* 期望的结果数量 */
        if (b != 0) L->top = ra+b;  /* 否则前一条指令设置 top */
        if (luaD_precall(L, ra, nresults)) {  /* C 函数? */
          if (nresults >= 0)
            L->top = ci->top;  /* 调整结果 */
          Protect((void)0);  /* 更新 'base' */
        }
        else {  /* Lua 函数 */
          ci = L->ci;
          goto newframe;  /* 在新 Lua 函数上重启 luaV_execute */
        }
        vmbreak;
      }
      vmcase(OP_TAILCALL) {
        int b = GETARG_B(i);  /* 参数数量 */
        if (b != 0) L->top = ra+b;  /* 否则前一条指令设置 top */
        lua_assert(GETARG_C(i) - 1 == LUA_MULTRET);  /* 必须是 LUA_MULTRET */
        if (luaD_precall(L, ra, LUA_MULTRET)) {  /* C 函数? */
          Protect((void)0);  /* 更新 'base' */
        }
        else {
          /* 尾调用: 将被调用帧(n)放在调用者帧(o)的位置 */
          CallInfo *nci = L->ci;  /* 被调用帧 */
          CallInfo *oci = nci->previous;  /* 调用者帧 */
          StkId nfunc = nci->func;  /* 被调用函数 */
          StkId ofunc = oci->func;  /* 调用者函数 */
          /* 'precall' 填充的最后一个栈槽 */
          StkId lim = nci->u.l.base + getproto(nfunc)->numparams;
          int aux;
          /* 关闭前一次调用的所有上值 */
          if (cl->p->sizep > 0) luaF_close(L, oci->u.l.base);
          /* 将新帧移动到旧帧中 */
          for (aux = 0; nfunc + aux < lim; aux++)
            setobjs2s(L, ofunc + aux, nfunc + aux);
          oci->u.l.base = ofunc + (nci->u.l.base - nfunc);  /* 修正基址 */
          oci->top = L->top = ofunc + (L->top - nfunc);  /* 修正 top */
          oci->u.l.savedpc = nci->u.l.savedpc;
          oci->callstatus |= CIST_TAIL;  /* 函数被尾调用 */
          ci = L->ci = oci;  /* 移除新帧 */
          lua_assert(L->top == oci->u.l.base + getproto(ofunc)->maxstacksize);
          goto newframe;  /* 在新 Lua 函数上重启 luaV_execute */
        }
        vmbreak;
      }
      vmcase(OP_RETURN) {
        int b = GETARG_B(i);  /* 返回值数量 */
        if (cl->p->sizep > 0) luaF_close(L, base);  /* 关闭所有上值 */
        b = luaD_poscall(L, ci, ra, (b != 0 ? b - 1 : cast_int(L->top - ra)));
        if (ci->callstatus & CIST_FRESH)  /* 局部 'ci' 仍来自被调用者 */
          return;  /* 外部调用: 返回 */
        else {  /* 通过重入调用: 继续执行 */
          ci = L->ci;
          if (b) L->top = ci->top;
          lua_assert(isLua(ci));
          lua_assert(GET_OPCODE(*((ci)->u.l.savedpc - 1)) == OP_CALL);
          goto newframe;  /* 在新 Lua 函数上重启 luaV_execute */
        }
      }
      vmcase(OP_FORLOOP) {
        if (ttisinteger(ra)) {  /* 整数循环? */
          lua_Integer step = ivalue(ra + 2);  /* 步长 */
          lua_Integer idx = intop(+, ivalue(ra), step); /* 增加索引 */
          lua_Integer limit = ivalue(ra + 1);  /* 限制值 */
          if ((0 < step) ? (idx <= limit) : (limit <= idx)) {  /* 检查循环条件 */
            ci->u.l.savedpc += GETARG_sBx(i);  /* 跳回 */
            chgivalue(ra, idx);  /* 更新内部索引... */
            setivalue(ra + 3, idx);  /* ...和外部索引 */
          }
        }
        else {  /* 浮点数循环 */
          lua_Number step = fltvalue(ra + 2);  /* 步长 */
          lua_Number idx = luai_numadd(L, fltvalue(ra), step); /* 增加索引 */
          lua_Number limit = fltvalue(ra + 1);  /* 限制值 */
          if (luai_numlt(0, step) ? luai_numle(idx, limit)
                                  : luai_numle(limit, idx)) {  /* 检查循环条件 */
            ci->u.l.savedpc += GETARG_sBx(i);  /* 跳回 */
            chgfltvalue(ra, idx);  /* 更新内部索引... */
            setfltvalue(ra + 3, idx);  /* ...和外部索引 */
          }
        }
        vmbreak;
      }
      vmcase(OP_FORPREP) {
        TValue *init = ra;  /* 初始值 */
        TValue *plimit = ra + 1;  /* 限制值 */
        TValue *pstep = ra + 2;  /* 步长 */
        lua_Integer ilimit;
        int stopnow;
        if (ttisinteger(init) && ttisinteger(pstep) &&
            forlimit(plimit, &ilimit, ivalue(pstep), &stopnow)) {
          /* 所有值都是整数 */
          lua_Integer initv = (stopnow ? 0 : ivalue(init));
          setivalue(plimit, ilimit);
          setivalue(init, intop(-, initv, ivalue(pstep)));
        }
        else {  /* 尝试将所有值转换为浮点数 */
          lua_Number ninit; lua_Number nlimit; lua_Number nstep;
          if (!tonumber(plimit, &nlimit))
            luaG_runerror(L, "'for' limit must be a number");
          setfltvalue(plimit, nlimit);
          if (!tonumber(pstep, &nstep))
            luaG_runerror(L, "'for' step must be a number");
          setfltvalue(pstep, nstep);
          if (!tonumber(init, &ninit))
            luaG_runerror(L, "'for' initial value must be a number");
          setfltvalue(init, luai_numsub(L, ninit, nstep));
        }
        ci->u.l.savedpc += GETARG_sBx(i);  /* 跳转到循环体 */
        vmbreak;
      }
      vmcase(OP_TFORCALL) {
        StkId cb = ra + 3;  /* 调用基址 */
        setobjs2s(L, cb+2, ra+2);  /* 复制状态 */
        setobjs2s(L, cb+1, ra+1);  /* 复制控制变量 */
        setobjs2s(L, cb, ra);  /* 复制迭代器函数 */
        L->top = cb + 3;  /* func. + 2 args (state and index) */
        Protect(luaD_call(L, cb, GETARG_C(i)));  /* 调用迭代器 */
        L->top = ci->top;
        i = *(ci->u.l.savedpc++);  /* 转到下一条指令 */
        ra = RA(i);
        lua_assert(GET_OPCODE(i) == OP_TFORLOOP);
        goto l_tforloop;
      }
      vmcase(OP_TFORLOOP) {
        l_tforloop:
        if (!ttisnil(ra + 1)) {  /* 继续循环? */
          setobjs2s(L, ra, ra + 1);  /* 保存控制变量 */
           ci->u.l.savedpc += GETARG_sBx(i);  /* 跳回 */
        }
        vmbreak;
      }
      vmcase(OP_SETLIST) {
        int n = GETARG_B(i);  /* 要设置的元素数量 */
        int c = GETARG_C(i);  /* 表大小 */
        unsigned int last;  /* 最后一个元素的位置 */
        Table *h;
        if (n == 0) n = cast_int(L->top - ra) - 1;  /* 使用所有可用值 */
        if (c == 0) {
          lua_assert(GET_OPCODE(*ci->u.l.savedpc) == OP_EXTRAARG);
          c = GETARG_Ax(*ci->u.l.savedpc++);
        }
        h = hvalue(ra);  /* 获取表 */
        last = ((c-1)*LFIELDS_PER_FLUSH) + n;  /* 计算最后一个元素的位置 */
        if (last > h->sizearray)  /* 需要更多空间? */
          luaH_resizearray(L, h, last);  /* 一次性预分配 */
        for (; n > 0; n--) {
          TValue *val = ra+n;
          luaH_setint(L, h, last--, val);  /* 设置元素 */
          luaC_barrierback(L, h, val);  /* 写屏障 */
        }
        L->top = ci->top;  /* 修正 top (以防之前的开放调用) */
        vmbreak;
      }
      vmcase(OP_CLOSURE) {
        Proto *p = cl->p->p[GETARG_Bx(i)];  /* 获取新的原型 */
        LClosure *ncl = getcached(p, cl->upvals, base);  /* 尝试获取缓存的闭包 */
        if (ncl == NULL)  /* 没有匹配? */
          pushclosure(L, p, cl->upvals, base, ra);  /* 创建一个新的 */
        else
          setclLvalue(L, ra, ncl);  /* 压入缓存的闭包 */
        checkGC(L, ra + 1);  /* 检查是否需要 GC */
        vmbreak;
      }
      vmcase(OP_VARARG) {
        int b = GETARG_B(i) - 1;  /* 所需结果数 */
        int j;
        int n = cast_int(base - ci->func) - cl->p->numparams - 1;  /* 可变参数数量 */
        if (n < 0)  /* 参数少于参数数量? */
          n = 0;  /* 没有可变参数 */
        if (b < 0) {  /* B == 0? */
          b = n;  /* 获取所有可变参数 */
          Protect(luaD_checkstack(L, n));  /* 确保栈空间足够 */
          ra = RA(i);  /* 前一次调用可能改变栈 */
          L->top = ra + n;
        }
        for (j = 0; j < b && j < n; j++)  /* 复制可变参数 */
          setobjs2s(L, ra + j, base - n + j);
        for (; j < b; j++)  /* 用 nil 完成所需结果 */
          setnilvalue(ra + j);
        vmbreak;
      }
      vmcase(OP_EXTRAARG) {
        lua_assert(0);  /* 不应该执行到这里 */
        vmbreak;
      }
    }
  }
}

/* }================================================================== */

