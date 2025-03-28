/*
** $Id: llex.h,v 1.79.1.1 2017/04/19 17:20:42 roberto Exp $
** Lexical Analyzer - 词法分析器
** See Copyright Notice in lua.h
*/

#ifndef llex_h
#define llex_h

#include "lobject.h"
#include "lzio.h"


/* 第一个保留字的token值，小于此值的token都是单字符token */
#define FIRST_RESERVED	257


/* 环境变量名称，用于全局环境表 */
#if !defined(LUA_ENV)
#define LUA_ENV		"_ENV"
#endif


/*
* WARNING: if you change the order of this enumeration,
* grep "ORDER RESERVED"
*/
enum RESERVED {
  /* 保留字对应的终结符 */
  TK_AND = FIRST_RESERVED, TK_BREAK,  /* and, break */
  TK_DO, TK_ELSE, TK_ELSEIF, TK_END, TK_FALSE, TK_FOR, TK_FUNCTION,  /* do, else, elseif, end, false, for, function */
  TK_GOTO, TK_IF, TK_IN, TK_LOCAL, TK_NIL, TK_NOT, TK_OR, TK_REPEAT,  /* goto, if, in, local, nil, not, or, repeat */
  TK_RETURN, TK_THEN, TK_TRUE, TK_UNTIL, TK_WHILE,  /* return, then, true, until, while */
  /* 其他终结符 */
  TK_IDIV, TK_CONCAT, TK_DOTS, TK_EQ, TK_GE, TK_LE, TK_NE,  /* //, .., ..., ==, >=, <=, ~= */
  TK_SHL, TK_SHR,  /* <<, >> */
  TK_DBCOLON, TK_EOS,  /* ::, 文件结束 */
  TK_FLT, TK_INT, TK_NAME, TK_STRING  /* 浮点数, 整数, 标识符, 字符串 */
};

/* 保留字的数量 */
#define NUM_RESERVED	(cast(int, TK_WHILE-FIRST_RESERVED+1))


/* 词法单元语义信息联合体 */
typedef union {
  lua_Number r;    /* 浮点数 */
  lua_Integer i;   /* 整数 */
  TString *ts;     /* 字符串 */
} SemInfo;  /* 语义信息 */


/* 词法单元结构 */
typedef struct Token {
  int token;       /* 词法单元类型 */
  SemInfo seminfo; /* 词法单元的语义信息 */
} Token;


/* 词法分析器状态结构体，包含词法分析器和解析器共享的状态 */
typedef struct LexState {
  int current;     /* 当前字符 */
  int linenumber;  /* 当前行号 */
  int lastline;    /* 上一个已处理的词法单元所在行号 */
  Token t;         /* 当前词法单元 */
  Token lookahead; /* 向前看一个词法单元 */
  struct FuncState *fs;  /* 当前函数状态(解析器) */
  struct lua_State *L;   /* Lua状态机 */
  ZIO *z;          /* 输入流 */
  Mbuffer *buff;   /* 词法单元缓冲区 */
  Table *h;        /* 字符串表，用于避免重复创建相同的字符串 */
  struct Dyndata *dyd;  /* 解析器使用的动态数据结构 */
  TString *source; /* 当前源文件名 */
  TString *envn;   /* 环境变量名 */
} LexState;


/* 词法分析器函数声明 */
LUAI_FUNC void luaX_init (lua_State *L);  /* 初始化词法分析器 */
LUAI_FUNC void luaX_setinput (lua_State *L, LexState *ls, ZIO *z,  /* 设置输入流 */
                              TString *source, int firstchar);
LUAI_FUNC TString *luaX_newstring (LexState *ls, const char *str, size_t l);  /* 创建新字符串 */
LUAI_FUNC void luaX_next (LexState *ls);  /* 读取下一个词法单元 */
LUAI_FUNC int luaX_lookahead (LexState *ls);  /* 向前看一个词法单元 */
LUAI_FUNC l_noret luaX_syntaxerror (LexState *ls, const char *s);  /* 语法错误处理 */
LUAI_FUNC const char *luaX_token2str (LexState *ls, int token);  /* 将token转换为字符串 */


#endif
