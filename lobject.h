/*
** $Id: lobject.h,v 2.117.1.1 2017/04/19 17:39:34 roberto Exp $
** Lua对象系统的类型定义
** 参见lua.h中的版权声明
*/

#ifndef lobject_h
#define lobject_h

#include <stdarg.h>
#include "llimits.h"
#include "lua.h"

/*
** 非值的额外标签
*/
#define LUA_TPROTO	LUA_NUMTAGS		/* 函数原型 */
#define LUA_TDEADKEY	(LUA_NUMTAGS+1)		/* 表中已删除的键 */

/*
** 所有可能标签的数量（包括LUA_TNONE但不包括DEADKEY）
*/
#define LUA_TOTALTAGS	(LUA_TPROTO + 2)

/*
** 标记值的标签使用以下位：
** 位0-3：实际标签（LUA_T*值）
** 位4-5：变体位
** 位6：值是否可被垃圾回收
*/

/*
** LUA_TFUNCTION变体：
** 0 - Lua函数
** 1 - 轻量级C函数
** 2 - 常规C函数（闭包）
*/

/* 函数变体标签 */
#define LUA_TLCL	(LUA_TFUNCTION | (0 << 4))  /* Lua闭包 */
#define LUA_TLCF	(LUA_TFUNCTION | (1 << 4))  /* 轻量级C函数 */
#define LUA_TCCL	(LUA_TFUNCTION | (2 << 4))  /* C闭包 */

/* 字符串变体标签 */
#define LUA_TSHRSTR	(LUA_TSTRING | (0 << 4))  /* 短字符串 */
#define LUA_TLNGSTR	(LUA_TSTRING | (1 << 4))  /* 长字符串 */

/* 数字变体标签 */
#define LUA_TNUMFLT	(LUA_TNUMBER | (0 << 4))  /* 浮点数 */
#define LUA_TNUMINT	(LUA_TNUMBER | (1 << 4))  /* 整数 */

/* 可回收类型的位标记 */
#define BIT_ISCOLLECTABLE	(1 << 6)

/* 将标签标记为可回收 */
#define ctb(t)			((t) | BIT_ISCOLLECTABLE)

/*
** 所有可回收对象的通用类型
*/
typedef struct GCObject GCObject;

/*
** 所有可回收对象的通用头部（以宏形式，用于包含在其他对象中）
*/
#define CommonHeader	GCObject *next; lu_byte tt; lu_byte marked

/*
** 通用类型只包含通用头部
*/
struct GCObject {
  CommonHeader;
};

/*
** 标记值。这是Lua中值的基本表示，
** 包含实际值和类型标签。
*/

/*
** Lua所有值的联合体
*/
typedef union Value {
  GCObject *gc;    /* 可回收对象 */
  void *p;         /* 轻量级用户数据 */
  int b;           /* 布尔值 */
  lua_CFunction f; /* 轻量级C函数 */
  lua_Integer i;   /* 整数 */
  lua_Number n;    /* 浮点数 */
} Value;

#define TValuefields	Value value_; int tt_

typedef struct lua_TValue {
  TValuefields;
} TValue;

/* 定义nil值的宏 */
#define NILCONSTANT	{NULL}, LUA_TNIL

#define val_(o)		((o)->value_)

/* TValue的原始类型标签 */
#define rttype(o)	((o)->tt_)

/* 无变体的标签（位0-3） */
#define novariant(x)	((x) & 0x0F)

/* TValue的类型标签（位0-3用于标签 + 变体位4-5） */
#define ttype(o)	(rttype(o) & 0x3F)

/* TValue的无变体类型标签（位0-3） */
#define ttnov(o)	(novariant(rttype(o)))

/* 类型检查宏 */
#define checktag(o,t)		(rttype(o) == (t))
#define checktype(o,t)		(ttnov(o) == (t))
#define ttisnumber(o)		checktype((o), LUA_TNUMBER)
#define ttisfloat(o)		checktag((o), LUA_TNUMFLT)
#define ttisinteger(o)		checktag((o), LUA_TNUMINT)
#define ttisnil(o)		checktag((o), LUA_TNIL)
#define ttisboolean(o)		checktag((o), LUA_TBOOLEAN)
#define ttislightuserdata(o)	checktag((o), LUA_TLIGHTUSERDATA)
#define ttisstring(o)		checktype((o), LUA_TSTRING)
#define ttisshrstring(o)	checktag((o), ctb(LUA_TSHRSTR))
#define ttislngstring(o)	checktag((o), ctb(LUA_TLNGSTR))
#define ttistable(o)		checktag((o), ctb(LUA_TTABLE))
#define ttisfunction(o)		checktype(o, LUA_TFUNCTION)
#define ttisclosure(o)		((rttype(o) & 0x1F) == LUA_TFUNCTION)
#define ttisCclosure(o)		checktag((o), ctb(LUA_TCCL))
#define ttisLclosure(o)		checktag((o), ctb(LUA_TLCL))
#define ttislcf(o)		checktag((o), LUA_TLCF)
#define ttisfulluserdata(o)	checktag((o), ctb(LUA_TUSERDATA))
#define ttisthread(o)		checktag((o), ctb(LUA_TTHREAD))
#define ttisdeadkey(o)		checktag((o), LUA_TDEADKEY)

/* 访问值的宏 */
#define ivalue(o)	check_exp(ttisinteger(o), val_(o).i)
#define fltvalue(o)	check_exp(ttisfloat(o), val_(o).n)
#define nvalue(o)	check_exp(ttisnumber(o), \
	(ttisinteger(o) ? cast_num(ivalue(o)) : fltvalue(o)))
#define gcvalue(o)	check_exp(iscollectable(o), val_(o).gc)
#define pvalue(o)	check_exp(ttislightuserdata(o), val_(o).p)
#define tsvalue(o)	check_exp(ttisstring(o), gco2ts(val_(o).gc))
#define uvalue(o)	check_exp(ttisfulluserdata(o), gco2u(val_(o).gc))
#define clvalue(o)	check_exp(ttisclosure(o), gco2cl(val_(o).gc))
#define clLvalue(o)	check_exp(ttisLclosure(o), gco2lcl(val_(o).gc))
#define clCvalue(o)	check_exp(ttisCclosure(o), gco2ccl(val_(o).gc))
#define fvalue(o)	check_exp(ttislcf(o), val_(o).f)
#define hvalue(o)	check_exp(ttistable(o), gco2t(val_(o).gc))
#define bvalue(o)	check_exp(ttisboolean(o), val_(o).b)
#define thvalue(o)	check_exp(ttisthread(o), gco2th(val_(o).gc))
/* 死值可能获得'gc'字段，但不能访问其内容 */
#define deadvalue(o)	check_exp(ttisdeadkey(o), cast(void *, val_(o).gc))

#define l_isfalse(o)	(ttisnil(o) || (ttisboolean(o) && bvalue(o) == 0))

#define iscollectable(o)	(rttype(o) & BIT_ISCOLLECTABLE)

/* 内部测试宏 */
#define righttt(obj)		(ttype(obj) == gcvalue(obj)->tt)

#define checkliveness(L,obj) \
	lua_longassert(!iscollectable(obj) || \
		(righttt(obj) && (L == NULL || !isdead(G(L),gcvalue(obj)))))

/* 设置值的宏 */
#define settt_(o,t)	((o)->tt_=(t))

#define setfltvalue(obj,x) \
  { TValue *io=(obj); val_(io).n=(x); settt_(io, LUA_TNUMFLT); }

#define chgfltvalue(obj,x) \
  { TValue *io=(obj); lua_assert(ttisfloat(io)); val_(io).n=(x); }

#define setivalue(obj,x) \
  { TValue *io=(obj); val_(io).i=(x); settt_(io, LUA_TNUMINT); }

#define chgivalue(obj,x) \
  { TValue *io=(obj); lua_assert(ttisinteger(io)); val_(io).i=(x); }

#define setnilvalue(obj) settt_(obj, LUA_TNIL)

#define setfvalue(obj,x) \
  { TValue *io=(obj); val_(io).f=(x); settt_(io, LUA_TLCF); }

#define setpvalue(obj,x) \
  { TValue *io=(obj); val_(io).p=(x); settt_(io, LUA_TLIGHTUSERDATA); }

#define setbvalue(obj,x) \
  { TValue *io=(obj); val_(io).b=(x); settt_(io, LUA_TBOOLEAN); }

#define setgcovalue(L,obj,x) \
  { TValue *io = (obj); GCObject *i_g=(x); \
    val_(io).gc = i_g; settt_(io, ctb(i_g->tt)); }

#define setsvalue(L,obj,x) \
  { TValue *io = (obj); TString *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(x_->tt)); \
    checkliveness(L,io); }

#define setuvalue(L,obj,x) \
  { TValue *io = (obj); Udata *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TUSERDATA)); \
    checkliveness(L,io); }

#define setthvalue(L,obj,x) \
  { TValue *io = (obj); lua_State *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TTHREAD)); \
    checkliveness(L,io); }

#define setclLvalue(L,obj,x) \
  { TValue *io = (obj); LClosure *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TLCL)); \
    checkliveness(L,io); }

#define setclCvalue(L,obj,x) \
  { TValue *io = (obj); CClosure *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TCCL)); \
    checkliveness(L,io); }

#define sethvalue(L,obj,x) \
  { TValue *io = (obj); Table *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TTABLE)); \
    checkliveness(L,io); }

#define setdeadvalue(obj)	settt_(obj, LUA_TDEADKEY)

#define setobj(L,obj1,obj2) \
	{ TValue *io1=(obj1); *io1 = *(obj2); \
	  (void)L; checkliveness(L,io1); }

/*
** 不同类型的赋值，根据目标位置
*/

/* 从栈到（相同）栈 */
#define setobjs2s	setobj
/* 到栈（不是从相同栈） */
#define setobj2s	setobj
#define setsvalue2s	setsvalue
#define sethvalue2s	sethvalue
#define setptvalue2s	setptvalue
/* 从表到相同表 */
#define setobjt2t	setobj
/* 到新对象 */
#define setobj2n	setobj
#define setsvalue2n	setsvalue

/* 到表（定义为表达式以在宏中使用） */
#define setobj2t(L,o1,o2)  ((void)L, *(o1)=*(o2), checkliveness(L,(o1)))

/*
** {======================================================
** 类型和原型
** =======================================================
*/

typedef TValue *StkId;  /* 栈元素索引 */

/*
** 字符串值的头部；字符串字节跟随此结构
** （根据'UTString'对齐；见下文）
*/
typedef struct TString {
  CommonHeader;
  lu_byte extra;  /* 短字符串的保留字；长字符串的"有哈希"标记 */
  lu_byte shrlen;  /* 短字符串的长度 */
  unsigned int hash;
  union {
    size_t lnglen;  /* 长字符串的长度 */
    struct TString *hnext;  /* 哈希表的链表 */
  } u;
} TString;

/*
** 确保此类型后的地址始终完全对齐
*/
typedef union UTString {
  L_Umaxalign dummy;  /* 确保字符串的最大对齐 */
  TString tsv;
} UTString;

/*
** 从'TString'获取实际字符串（字节数组）
** （访问'extra'确保值真的是'TString'）
*/
#define getstr(ts)  \
  check_exp(sizeof((ts)->extra), cast(char *, (ts)) + sizeof(UTString))

/* 从Lua值获取实际字符串（字节数组） */
#define svalue(o)       getstr(tsvalue(o))

/* 从'TString *s'获取字符串长度 */
#define tsslen(s)	((s)->tt == LUA_TSHRSTR ? (s)->shrlen : (s)->u.lnglen)

/* 从'TValue *o'获取字符串长度 */
#define vslen(o)	tsslen(tsvalue(o))

/*
** 用户数据的头部；内存区域跟随此结构
** （根据'UUdata'对齐；见下文）
*/
typedef struct Udata {
  CommonHeader;
  lu_byte ttuv_;  /* 用户值的标签 */
  struct Table *metatable;
  size_t len;  /* 字节数 */
  union Value user_;  /* 用户值 */
} Udata;

/*
** 确保此类型后的地址始终完全对齐
*/
typedef union UUdata {
  L_Umaxalign dummy;  /* 确保'local'用户数据的最大对齐 */
  Udata uv;
} UUdata;

/*
** 获取'Udata'内部内存块的地址
** （访问'ttuv_'确保值真的是'Udata'）
*/
#define getudatamem(u)  \
  check_exp(sizeof((u)->ttuv_), (cast(char*, (u)) + sizeof(UUdata)))

#define setuservalue(L,u,o) \
	{ const TValue *io=(o); Udata *iu = (u); \
	  iu->user_ = io->value_; iu->ttuv_ = rttype(io); \
	  checkliveness(L,io); }

#define getuservalue(L,u,o) \
	{ TValue *io=(o); const Udata *iu = (u); \
	  io->value_ = iu->user_; settt_(io, iu->ttuv_); \
	  checkliveness(L,io); }

/*
** 函数原型的上值描述
*/
typedef struct Upvaldesc {
  TString *name;  /* 上值名称（用于调试信息） */
  lu_byte instack;  /* 是否在栈中（寄存器） */
  lu_byte idx;  /* 上值索引（在栈中或在外层函数的列表中） */
} Upvaldesc;

/*
** 函数原型的局部变量描述
** （用于调试信息）
*/
typedef struct LocVar {
  TString *varname;
  int startpc;  /* 变量首次活跃的点 */
  int endpc;    /* 变量首次死亡的点 */
} LocVar;

/*
** 函数原型
*/
typedef struct Proto {
  CommonHeader;
  lu_byte numparams;  /* 固定参数数量 */
  lu_byte is_vararg;
  lu_byte maxstacksize;  /* 此函数需要的寄存器数量 */
  int sizeupvalues;  /* 'upvalues'的大小 */
  int sizek;  /* 'k'的大小 */
  int sizecode;
  int sizelineinfo;
  int sizep;  /* 'p'的大小 */
  int sizelocvars;
  int linedefined;  /* 调试信息 */
  int lastlinedefined;  /* 调试信息 */
  TValue *k;  /* 函数使用的常量 */
  Instruction *code;  /* 操作码 */
  struct Proto **p;  /* 函数内定义的函数 */
  int *lineinfo;  /* 从操作码到源代码行的映射（调试信息） */
  LocVar *locvars;  /* 局部变量信息（调试信息） */
  Upvaldesc *upvalues;  /* 上值信息 */
  struct LClosure *cache;  /* 使用此原型的最后创建的闭包 */
  TString  *source;  /* 用于调试信息 */
  GCObject *gclist;
} Proto;

/*
** Lua上值
*/
typedef struct UpVal UpVal;

/*
** 闭包
*/

#define ClosureHeader \
	CommonHeader; lu_byte nupvalues; GCObject *gclist

typedef struct CClosure {
  ClosureHeader;
  lua_CFunction f;
  TValue upvalue[1];  /* 上值列表 */
} CClosure;

typedef struct LClosure {
  ClosureHeader;
  struct Proto *p;
  UpVal *upvals[1];  /* 上值列表 */
} LClosure;

typedef union Closure {
  CClosure c;
  LClosure l;
} Closure;

#define isLfunction(o)	ttisLclosure(o)

#define getproto(o)	(clLvalue(o)->p)

/*
** 表
*/

typedef union TKey {
  struct {
    TValuefields;
    int next;  /* 用于链接（下一个节点的偏移量） */
  } nk;
  TValue tvk;
} TKey;

/* 将值复制到键中而不破坏'next'字段 */
#define setnodekey(L,key,obj) \
	{ TKey *k_=(key); const TValue *io_=(obj); \
	  k_->nk.value_ = io_->value_; k_->nk.tt_ = io_->tt_; \
	  (void)L; checkliveness(L,io_); }

typedef struct Node {
  TValue i_val;
  TKey i_key;
} Node;

typedef struct Table {
  CommonHeader;
  lu_byte flags;  /* 1<<p表示tagmethod(p)不存在 */
  lu_byte lsizenode;  /* 'node'数组大小的log2 */
  unsigned int sizearray;  /* 'array'数组的大小 */
  TValue *array;  /* 数组部分 */
  Node *node;
  Node *lastfree;  /* 任何空闲位置都在此位置之前 */
  struct Table *metatable;
  GCObject *gclist;
} Table;

/*
** 用于哈希的'module'操作（大小始终是2的幂）
*/
#define lmod(s,size) \
	(check_exp((size&(size-1))==0, (cast(int, (s) & ((size)-1)))))

#define twoto(x)	(1<<(x))
#define sizenode(t)	(twoto((t)->lsizenode))

/*
** （固定nil值的地址）
*/
#define luaO_nilobject		(&luaO_nilobject_)

LUAI_DDEC const TValue luaO_nilobject_;

/* 'luaO_utf8esc'函数的缓冲区大小 */
#define UTF8BUFFSZ	8

LUAI_FUNC int luaO_int2fb (unsigned int x);
LUAI_FUNC int luaO_fb2int (int x);
LUAI_FUNC int luaO_utf8esc (char *buff, unsigned long x);
LUAI_FUNC int luaO_ceillog2 (unsigned int x);
LUAI_FUNC void luaO_arith (lua_State *L, int op, const TValue *p1,
                           const TValue *p2, TValue *res);
LUAI_FUNC size_t luaO_str2num (const char *s, TValue *o);
LUAI_FUNC int luaO_hexavalue (int c);
LUAI_FUNC void luaO_tostring (lua_State *L, StkId obj);
LUAI_FUNC const char *luaO_pushvfstring (lua_State *L, const char *fmt,
                                                       va_list argp);
LUAI_FUNC const char *luaO_pushfstring (lua_State *L, const char *fmt, ...);
LUAI_FUNC void luaO_chunkid (char *out, const char *source, size_t len);

#endif

