/*
** $Id: lobject.c,v 2.113.1.1 2017/04/19 17:29:57 roberto Exp $
** Lua对象的一些通用函数
** 参见lua.h中的版权声明
*/

#define lobject_c
#define LUA_CORE

#include "lprefix.h"

#include <locale.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lua.h"

#include "lctype.h"
#include "ldebug.h"
#include "ldo.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "lvm.h"

/* 定义全局nil对象 */
LUAI_DDEF const TValue luaO_nilobject_ = {NILCONSTANT};

/*
** 将整数转换为"浮点字节"，表示为(eeeeexxx)，
** 其中实际值为(1xxx) * 2^(eeeee - 1)如果eeeee != 0，
** 否则为(xxx)。
*/
int luaO_int2fb (unsigned int x) {
  int e = 0;  /* 指数 */
  if (x < 8) return x;
  while (x >= (8 << 4)) {  /* 粗略步骤 */
    x = (x + 0xf) >> 4;  /* x = ceil(x / 16) */
    e += 4;
  }
  while (x >= (8 << 1)) {  /* 精细步骤 */
    x = (x + 1) >> 1;  /* x = ceil(x / 2) */
    e++;
  }
  return ((e+1) << 3) | (cast_int(x) - 8);
}

/* 转换回来 */
int luaO_fb2int (int x) {
  return (x < 8) ? x : ((x & 7) + 8) << ((x >> 3) - 1);
}

/*
** 计算ceil(log2(x))
*/
/**
 * 计算以2为底的对数的上界值（即ceil(log2(x))）
 *
 * 参数:
 * x (unsigned int): 需要计算对数的整数
 *
 * 返回值:
 * int: 计算结果，表示不小于log2(x)的最小整数
 */
int luaO_ceillog2 (unsigned int x) {
  static const lu_byte log_2[256] = {  /* log_2[i] = ceil(log2(i - 1)) */
  // 预计算好的log2表，log_2[i] = ceil(log2(i - 1))
    0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
    6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
  };
  int l = 0;
  x--;  // 调整x，使其与log_2表对应
  while (x >= 256) { l += 8; x >>= 8; }  // 处理大于256的部分，每次右移8位
  return l + log_2[x];  // 返回最终结果
}

/* 整数算术运算 */
static lua_Integer intarith (lua_State *L, int op, lua_Integer v1,
                                                   lua_Integer v2) {
  switch (op) {
    case LUA_OPADD: return intop(+, v1, v2);  /* 加法 */
    case LUA_OPSUB:return intop(-, v1, v2);   /* 减法 */
    case LUA_OPMUL:return intop(*, v1, v2);   /* 乘法 */
    case LUA_OPMOD: return luaV_mod(L, v1, v2);  /* 取模 */
    case LUA_OPIDIV: return luaV_div(L, v1, v2);  /* 整数除法 */
    case LUA_OPBAND: return intop(&, v1, v2);  /* 位与 */
    case LUA_OPBOR: return intop(|, v1, v2);   /* 位或 */
    case LUA_OPBXOR: return intop(^, v1, v2);  /* 位异或 */
    case LUA_OPSHL: return luaV_shiftl(v1, v2);  /* 左移 */
    case LUA_OPSHR: return luaV_shiftl(v1, -v2);  /* 右移 */
    case LUA_OPUNM: return intop(-, 0, v1);    /* 负号 */
    case LUA_OPBNOT: return intop(^, ~l_castS2U(0), v1);  /* 位非 */
    default: lua_assert(0); return 0;
  }
}

/* 浮点数算术运算 */
static lua_Number numarith (lua_State *L, int op, lua_Number v1,
                                                  lua_Number v2) {
  switch (op) {
    case LUA_OPADD: return luai_numadd(L, v1, v2);  /* 加法 */
    case LUA_OPSUB: return luai_numsub(L, v1, v2);  /* 减法 */
    case LUA_OPMUL: return luai_nummul(L, v1, v2);  /* 乘法 */
    case LUA_OPDIV: return luai_numdiv(L, v1, v2);  /* 除法 */
    case LUA_OPPOW: return luai_numpow(L, v1, v2);  /* 幂 */
    case LUA_OPIDIV: return luai_numidiv(L, v1, v2);  /* 整数除法 */
    case LUA_OPUNM: return luai_numunm(L, v1);  /* 负号 */
    case LUA_OPMOD: {
      lua_Number m;
      luai_nummod(L, v1, v2, m);  /* 取模 */
      return m;
    }
    default: lua_assert(0); return 0;
  }
}

/*
** 执行算术运算
** 根据操作数类型选择整数或浮点数运算
*/
void luaO_arith (lua_State *L, int op, const TValue *p1, const TValue *p2,
                 TValue *res) {
  switch (op) {
    case LUA_OPBAND: case LUA_OPBOR: case LUA_OPBXOR:
    case LUA_OPSHL: case LUA_OPSHR:
    case LUA_OPBNOT: {  /* 仅对整数操作 */
      lua_Integer i1; lua_Integer i2;
      if (tointeger(p1, &i1) && tointeger(p2, &i2)) {
        setivalue(res, intarith(L, op, i1, i2));
        return;
      }
      else break;  /* 转到末尾 */
    }
    case LUA_OPDIV: case LUA_OPPOW: {  /* 仅对浮点数操作 */
      lua_Number n1; lua_Number n2;
      if (tonumber(p1, &n1) && tonumber(p2, &n2)) {
        setfltvalue(res, numarith(L, op, n1, n2));
        return;
      }
      else break;  /* 转到末尾 */
    }
    default: {  /* 其他操作 */
      lua_Number n1; lua_Number n2;
      if (ttisinteger(p1) && ttisinteger(p2)) {
        setivalue(res, intarith(L, op, ivalue(p1), ivalue(p2)));
        return;
      }
      else if (tonumber(p1, &n1) && tonumber(p2, &n2)) {
        setfltvalue(res, numarith(L, op, n1, n2));
        return;
      }
      else break;  /* 转到末尾 */
    }
  }
  /* 无法执行原始操作；尝试元方法 */
  lua_assert(L != NULL);  /* 编译时不应失败 */
  luaT_trybinTM(L, p1, p2, res, cast(TMS, (op - LUA_OPADD) + TM_ADD));
}

/* 获取十六进制字符的值 */
int luaO_hexavalue (int c) {
  if (lisdigit(c)) return c - '0';
  else return (ltolower(c) - 'a') + 10;
}

/* 检查字符串是否以负号开头 */
static int isneg (const char **s) {
  if (**s == '-') { (*s)++; return 1; }
  else if (**s == '+') (*s)++;
  return 0;
}

/*
** {==================================================================
** Lua的'lua_strx2number'实现
** ===================================================================
*/

#if !defined(lua_strx2number)

/* 读取的最大有效数字位数（以避免单精度浮点数溢出） */
#define MAXSIGDIG	30

/*
** 将十六进制数字字符串转换为数字，遵循C99的'strtod'规范
*/
static lua_Number lua_strx2number (const char *s, char **endptr) {
  int dot = lua_getlocaledecpoint();
  lua_Number r = 0.0;  /* 结果（累加器） */
  int sigdig = 0;  /* 有效数字位数 */
  int nosigdig = 0;  /* 非有效数字位数 */
  int e = 0;  /* 指数修正 */
  int neg;  /* 1表示数字为负 */
  int hasdot = 0;  /* 是否已看到小数点 */
  *endptr = cast(char *, s);  /* 目前还没有有效内容 */
  while (lisspace(cast_uchar(*s))) s++;  /* 跳过初始空格 */
  neg = isneg(&s);  /* 检查符号 */
  if (!(*s == '0' && (*(s + 1) == 'x' || *(s + 1) == 'X')))  /* 检查'0x' */
    return 0.0;  /* 无效格式（没有'0x'） */
  for (s += 2; ; s++) {  /* 跳过'0x'并读取数字 */
    if (*s == dot) {
      if (hasdot) break;  /* 第二个小数点？停止循环 */
      else hasdot = 1;
    }
    else if (lisxdigit(cast_uchar(*s))) {
      if (sigdig == 0 && *s == '0')  /* 非有效数字（零）？ */
        nosigdig++;
      else if (++sigdig <= MAXSIGDIG)  /* 可以在不溢出时读取？ */
          r = (r * cast_num(16.0)) + luaO_hexavalue(*s);
      else e++; /* 太多数字；忽略，但仍计入指数 */
      if (hasdot) e--;  /* 小数点后的数字？修正指数 */
    }
    else break;  /* 既不是小数点也不是数字 */
  }
  if (nosigdig + sigdig == 0)  /* 没有数字？ */
    return 0.0;  /* 无效格式 */
  *endptr = cast(char *, s);  /* 到此为止有效 */
  e *= 4;  /* 每个数字将值乘以/除以2^4 */
  if (*s == 'p' || *s == 'P') {  /* 指数部分？ */
    int exp1 = 0;  /* 指数值 */
    int neg1;  /* 指数符号 */
    s++;  /* 跳过'p' */
    neg1 = isneg(&s);  /* 符号 */
    if (!lisdigit(cast_uchar(*s)))
      return 0.0;  /* 无效；必须至少有一个数字 */
    while (lisdigit(cast_uchar(*s)))  /* 读取指数 */
      exp1 = exp1 * 10 + *(s++) - '0';
    if (neg1) exp1 = -exp1;
    e += exp1;
    *endptr = cast(char *, s);  /* 到此为止有效 */
  }
  if (neg) r = -r;
  return l_mathop(ldexp)(r, e);
}

#endif
/* }====================================================== */

/* 数字的最大长度 */
#if !defined (L_MAXLENNUM)
#define L_MAXLENNUM	200
#endif

/* 将字符串转换为数字的本地函数 */
static const char *l_str2dloc (const char *s, lua_Number *result, int mode) {
  char *endptr;
  *result = (mode == 'x') ? lua_strx2number(s, &endptr)  /* 尝试转换 */
                          : lua_str2number(s, &endptr);
  if (endptr == s) return NULL;  /* 没有识别到任何内容？ */
  while (lisspace(cast_uchar(*endptr))) endptr++;  /* 跳过尾部空格 */
  return (*endptr == '\0') ? endptr : NULL;  /* 如果没有尾部字符则OK */
}

/*
** 将字符串's'转换为Lua数字（放入'result'）
** 失败时返回NULL，成功时返回结束'\0'的地址
** 'pmode'指向（'mode'包含）字符串中的特殊内容：
** - 'x'/'X'表示十六进制数字
** - 'n'/'N'表示'inf'或'nan'（应该被拒绝）
** - '.'仅优化常见情况的搜索（没有特殊内容）
** 此函数接受当前区域设置或点作为基数标记
** 如果转换失败，可能意味着数字有点但区域设置接受其他内容
** 在这种情况下，代码将's'复制到缓冲区（因为's'是只读的），
** 将点更改为当前区域设置的基数标记，然后再次尝试转换
*/
static const char *l_str2d (const char *s, lua_Number *result) {
  const char *endptr;
  const char *pmode = strpbrk(s, ".xXnN");
  int mode = pmode ? ltolower(cast_uchar(*pmode)) : 0;
  if (mode == 'n')  /* 拒绝'inf'和'nan' */
    return NULL;
  endptr = l_str2dloc(s, result, mode);  /* 尝试转换 */
  if (endptr == NULL) {  /* 失败？可能是不同的区域设置 */
    char buff[L_MAXLENNUM + 1];
    const char *pdot = strchr(s, '.');
    if (strlen(s) > L_MAXLENNUM || pdot == NULL)
      return NULL;  /* 字符串太长或没有点；失败 */
    strcpy(buff, s);  /* 复制字符串到缓冲区 */
    buff[pdot - s] = lua_getlocaledecpoint();  /* 修正小数点 */
    endptr = l_str2dloc(buff, result, mode);  /* 再次尝试 */
    if (endptr != NULL)
      endptr = s + (endptr - buff);  /* 相对于's' */
  }
  return endptr;
}

#define MAXBY10		cast(lua_Unsigned, LUA_MAXINTEGER / 10)
#define MAXLASTD	cast_int(LUA_MAXINTEGER % 10)

/* 将字符串转换为整数 */
static const char *l_str2int (const char *s, lua_Integer *result) {
  lua_Unsigned a = 0;
  int empty = 1;
  int neg;
  while (lisspace(cast_uchar(*s))) s++;  /* 跳过初始空格 */
  neg = isneg(&s);
  if (s[0] == '0' &&
      (s[1] == 'x' || s[1] == 'X')) {  /* 十六进制？ */
    s += 2;  /* 跳过'0x' */
    for (; lisxdigit(cast_uchar(*s)); s++) {
      a = a * 16 + luaO_hexavalue(*s);
      empty = 0;
    }
  }
  else {  /* 十进制 */
    for (; lisdigit(cast_uchar(*s)); s++) {
      int d = *s - '0';
      if (a >= MAXBY10 && (a > MAXBY10 || d > MAXLASTD + neg))  /* 溢出？ */
        return NULL;  /* 不接受（作为整数） */
      a = a * 10 + d;
      empty = 0;
    }
  }
  while (lisspace(cast_uchar(*s))) s++;  /* 跳过尾部空格 */
  if (empty || *s != '\0') return NULL;  /* 数字中有错误 */
  else {
    *result = l_castU2S((neg) ? 0u - a : a);
    return s;
  }
}

/*
** 将字符串转换为数字
** 返回转换后的字符串长度，如果转换失败则返回0
*/
size_t luaO_str2num (const char *s, TValue *o) {
  lua_Integer i; lua_Number n;
  const char *e;
  if ((e = l_str2int(s, &i)) != NULL) {  /* 尝试作为整数 */
    setivalue(o, i);
  }
  else if ((e = l_str2d(s, &n)) != NULL) {  /* 否则尝试作为浮点数 */
    setfltvalue(o, n);
  }
  else
    return 0;  /* 转换失败 */
  return (e - s) + 1;  /* 成功；返回字符串长度 */
}

/*
** 将Unicode字符编码为UTF-8
** 返回编码后的字节数
*/
int luaO_utf8esc (char *buff, unsigned long x) {
  int n = 1;  /* 放入缓冲区的字节数（从后向前） */
  lua_assert(x <= 0x10FFFF);
  if (x < 0x80)  /* ASCII？ */
    buff[UTF8BUFFSZ - 1] = cast(char, x);
  else {  /* 需要连续字节 */
    unsigned int mfb = 0x3f;  /* 第一个字节中可容纳的最大值 */
    do {  /* 添加连续字节 */
      buff[UTF8BUFFSZ - (n++)] = cast(char, 0x80 | (x & 0x3f));
      x >>= 6;  /* 移除已添加的位 */
      mfb >>= 1;  /* 现在第一个字节中可用的位少了一个 */
    } while (x > mfb);  /* 还需要连续字节？ */
    buff[UTF8BUFFSZ - n] = cast(char, (~mfb << 1) | x);  /* 添加第一个字节 */
  }
  return n;
}

/* 数字转换为字符串的最大长度 */
#define MAXNUMBER2STR	50

/*
** 将数字对象转换为字符串
*/
void luaO_tostring (lua_State *L, StkId obj) {
  char buff[MAXNUMBER2STR];
  size_t len;
  lua_assert(ttisnumber(obj));
  if (ttisinteger(obj))
    len = lua_integer2str(buff, sizeof(buff), ivalue(obj));
  else {
    len = lua_number2str(buff, sizeof(buff), fltvalue(obj));
#if !defined(LUA_COMPAT_FLOATSTRING)
    if (buff[strspn(buff, "-0123456789")] == '\0') {  /* 看起来像整数？ */
      buff[len++] = lua_getlocaledecpoint();
      buff[len++] = '0';  /* 添加'.0'到结果 */
    }
#endif
  }
  setsvalue2s(L, obj, luaS_newlstr(L, buff, len));
}

/* 将字符串压入栈 */
static void pushstr (lua_State *L, const char *str, size_t l) {
  setsvalue2s(L, L->top, luaS_newlstr(L, str, l));
  luaD_inctop(L);
}

/*
** 此函数仅处理'%d'、'%c'、'%f'、'%p'和'%s'常规格式，
** 以及Lua特定的'%I'和'%U'
*/
const char *luaO_pushvfstring (lua_State *L, const char *fmt, va_list argp) {
  int n = 0;
  for (;;) {
    const char *e = strchr(fmt, '%');
    if (e == NULL) break;
    pushstr(L, fmt, e - fmt);
    switch (*(e+1)) {
      case 's': {  /* 以零结尾的字符串 */
        const char *s = va_arg(argp, char *);
        if (s == NULL) s = "(null)";
        pushstr(L, s, strlen(s));
        break;
      }
      case 'c': {  /* 作为字符的'int' */
        char buff = cast(char, va_arg(argp, int));
        if (lisprint(cast_uchar(buff)))
          pushstr(L, &buff, 1);
        else  /* 不可打印字符；打印其代码 */
          luaO_pushfstring(L, "<\\%d>", cast_uchar(buff));
        break;
      }
      case 'd': {  /* 一个'int' */
        setivalue(L->top, va_arg(argp, int));
        goto top2str;
      }
      case 'I': {  /* 一个'lua_Integer' */
        setivalue(L->top, cast(lua_Integer, va_arg(argp, l_uacInt)));
        goto top2str;
      }
      case 'f': {  /* 一个'lua_Number' */
        setfltvalue(L->top, cast_num(va_arg(argp, l_uacNumber)));
      top2str:  /* 将顶部元素转换为字符串 */
        luaD_inctop(L);
        luaO_tostring(L, L->top - 1);
        break;
      }
      case 'p': {  /* 一个指针 */
        char buff[4*sizeof(void *) + 8]; /* 应该足够空间存储'%p' */
        void *p = va_arg(argp, void *);
        int l = lua_pointer2str(buff, sizeof(buff), p);
        pushstr(L, buff, l);
        break;
      }
      case 'U': {  /* 作为UTF-8序列的'int' */
        char buff[UTF8BUFFSZ];
        int l = luaO_utf8esc(buff, cast(long, va_arg(argp, long)));
        pushstr(L, buff + UTF8BUFFSZ - l, l);
        break;
      }
      case '%': {
        pushstr(L, "%", 1);
        break;
      }
      default: {
        luaG_runerror(L, "invalid option '%%%c' to 'lua_pushfstring'",
                         *(e + 1));
      }
    }
    n += 2;
    fmt = e+2;
  }
  luaD_checkstack(L, 1);
  pushstr(L, fmt, strlen(fmt));
  if (n > 0) luaV_concat(L, n + 1);
  return svalue(L->top - 1);
}

/*
** 格式化字符串并压入栈
** 返回格式化后的字符串
*/
const char *luaO_pushfstring (lua_State *L, const char *fmt, ...) {
  const char *msg;
  va_list argp;
  va_start(argp, fmt);
  msg = luaO_pushvfstring(L, fmt, argp);
  va_end(argp);
  return msg;
}

/* 字面字符串的字符数（不包括结尾的\0） */
#define LL(x)	(sizeof(x)/sizeof(char) - 1)

#define RETS	"..."
#define PRE	"[string \""
#define POS	"\"]"

#define addstr(a,b,l)	( memcpy(a,b,(l) * sizeof(char)), a += (l) )

/*
** 获取代码块ID
** 用于调试信息
*/
void luaO_chunkid (char *out, const char *source, size_t bufflen) {
  size_t l = strlen(source);
  if (*source == '=') {  /* '字面'源 */
    if (l <= bufflen)  /* 足够小？ */
      memcpy(out, source + 1, l * sizeof(char));
    else {  /* 截断它 */
      addstr(out, source + 1, bufflen - 1);
      *out = '\0';
    }
  }
  else if (*source == '@') {  /* 文件名 */
    if (l <= bufflen)  /* 足够小？ */
      memcpy(out, source + 1, l * sizeof(char));
    else {  /* 在名称其余部分前添加'...' */
      addstr(out, RETS, LL(RETS));
      bufflen -= LL(RETS);
      memcpy(out, source + 1 + l - bufflen, bufflen * sizeof(char));
    }
  }
  else {  /* 字符串；格式化为[string "source"] */
    const char *nl = strchr(source, '\n');  /* 查找第一个换行符（如果有） */
    addstr(out, PRE, LL(PRE));  /* 添加前缀 */
    bufflen -= LL(PRE RETS POS) + 1;  /* 为前缀+后缀+'\0'保存空间 */
    if (l < bufflen && nl == NULL) {  /* 小的单行源？ */
      addstr(out, source, l);  /* 保留它 */
    }
    else {
      if (nl != NULL) l = nl - source;  /* 在第一个换行符处停止 */
      if (l > bufflen) l = bufflen;
      addstr(out, source, l);
      addstr(out, RETS, LL(RETS));
    }
    memcpy(out, POS, (LL(POS) + 1) * sizeof(char));
  }
}

