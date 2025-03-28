/*
** $Id: lvm.h,v 2.41.1.1 2017/04/19 17:20:42 roberto Exp $
** Lua 虚拟机头文件
** 作者: R. Ierusalimschy, L. H. de Figueiredo, W. Celes
** 创建日期: 2018年
*/

#ifndef lvm_h
#define lvm_h


#include "ldo.h"
#include "lobject.h"
#include "ltm.h"


/* 控制数字和字符串之间的转换 */
#if !defined(LUA_NOCVTN2S)
#define cvt2str(o)	ttisnumber(o)  /* 允许数字转换为字符串 */
#else
#define cvt2str(o)	0	/* 禁止数字转换为字符串 */
#endif


#if !defined(LUA_NOCVTS2N)
#define cvt2num(o)	ttisstring(o)  /* 允许字符串转换为数字 */
#else
#define cvt2num(o)	0	/* 禁止字符串转换为数字 */
#endif


/*
** 定义 LUA_FLOORN2I 宏来控制浮点数到整数的转换方式
** 如果定义，则使用向下取整方式转换
** 如果未定义，则对非整数值报错
*/
#if !defined(LUA_FLOORN2I)
#define LUA_FLOORN2I		0
#endif


/* 类型转换宏 */
#define tonumber(o,n) \
	(ttisfloat(o) ? (*(n) = fltvalue(o), 1) : luaV_tonumber_(o,n))  /* 转换为浮点数 */

#define tointeger(o,i) \
    (ttisinteger(o) ? (*(i) = ivalue(o), 1) : luaV_tointeger(o,i,LUA_FLOORN2I))  /* 转换为整数 */

#define intop(op,v1,v2) l_castU2S(l_castS2U(v1) op l_castS2U(v2))  /* 整数运算 */

#define luaV_rawequalobj(t1,t2)		luaV_equalobj(NULL,t1,t2)


/*
** 快速获取表值的宏
** 如果 't' 是表且 't[k]' 不为 nil，返回 1 并将 'slot' 指向 't[k]'
** 否则返回 0，'slot' 指向 nil 的 't[k]'（如果 't' 是表）或 NULL
** 'f' 是用于原始访问的函数
*/
#define luaV_fastget(L,t,k,slot,f) \
  (!ttistable(t)  \
   ? (slot = NULL, 0)  /* 不是表，返回0 */  \
   : (slot = f(hvalue(t), k),  /* 执行原始访问 */  \
      !ttisnil(slot)))  /* 结果不为nil? */

/*
** 标准表值获取实现
** 使用快速路径或完成获取操作
*/
#define luaV_gettable(L,t,k,v) { const TValue *slot; \
  if (luaV_fastget(L,t,k,slot,luaH_get)) { setobj2s(L, v, slot); } \
  else luaV_finishget(L,t,k,v,slot); }


/*
** 快速设置表值的宏
** 如果 't' 是表且 't[k]' 不为 nil，调用 GC 屏障，执行原始赋值，返回 true
** 否则返回 false，'slot' 为 NULL（如果 't' 不是表）或 'nil'
** 注意：如果宏返回 true，不需要 'invalidateTMcache'，因为不会创建新条目
*/
#define luaV_fastset(L,t,k,slot,f,v) \
  (!ttistable(t) \
   ? (slot = NULL, 0) \
   : (slot = f(hvalue(t), k), \
     ttisnil(slot) ? 0 \
     : (luaC_barrierback(L, hvalue(t), v), \
        setobj2t(L, cast(TValue *,slot), v), \
        1)))


/*
** 标准表值设置实现
** 使用快速路径或完成设置操作
*/
#define luaV_settable(L,t,k,v) { const TValue *slot; \
  if (!luaV_fastset(L,t,k,slot,luaH_get,v)) \
    luaV_finishset(L,t,k,v,slot); }



/* 虚拟机核心函数声明 */
LUAI_FUNC int luaV_equalobj (lua_State *L, const TValue *t1, const TValue *t2);  /* 值相等性比较 */
LUAI_FUNC int luaV_lessthan (lua_State *L, const TValue *l, const TValue *r);    /* 小于比较 */
LUAI_FUNC int luaV_lessequal (lua_State *L, const TValue *l, const TValue *r);   /* 小于等于比较 */
LUAI_FUNC int luaV_tonumber_ (const TValue *obj, lua_Number *n);                 /* 转换为数字 */
LUAI_FUNC int luaV_tointeger (const TValue *obj, lua_Integer *p, int mode);      /* 转换为整数 */
LUAI_FUNC void luaV_finishget (lua_State *L, const TValue *t, TValue *key,       /* 完成表值获取 */
                               StkId val, const TValue *slot);
LUAI_FUNC void luaV_finishset (lua_State *L, const TValue *t, TValue *key,       /* 完成表值设置 */
                               StkId val, const TValue *slot);
LUAI_FUNC void luaV_finishOp (lua_State *L);                                     /* 完成操作 */
LUAI_FUNC void luaV_execute (lua_State *L);                                      /* 执行字节码 */
LUAI_FUNC void luaV_concat (lua_State *L, int total);                           /* 字符串连接 */
LUAI_FUNC lua_Integer luaV_div (lua_State *L, lua_Integer x, lua_Integer y);     /* 整数除法 */
LUAI_FUNC lua_Integer luaV_mod (lua_State *L, lua_Integer x, lua_Integer y);     /* 整数取模 */
LUAI_FUNC lua_Integer luaV_shiftl (lua_Integer x, lua_Integer y);                /* 位运算左移 */
LUAI_FUNC void luaV_objlen (lua_State *L, StkId ra, const TValue *rb);          /* 获取对象长度 */

#endif
