# -*- coding: utf-8 -*-
from oad import *
from oad.dinpy import *

# when lisp meets prolog in python

# 假如有一门语言，(),{},[],冒号，逗号，分号，句号，空格，换行，缩进等等都是运算符，
# 而且都可以任意重载，并且由程序员指定运算符的优先级，那么就可以随意定义新语言了。

# 关键字
# 必须避开python的关键字，
# 尽量短: 不超过5个字母
# control: do, loop, each, if_, iff, then, els, elsif, when, until, on, at, 
  # exit, next, goto
# variable: use, set, out, local, var, v, globl, my, dummy
# structure: rule, rules, lamda, let, letre, fun, macro, klass, block,
  # label
# solve, eval

X, Expr, ExprList, Result, Y = var. X. Y. Expr. ExprList. Result # 语句不能放在列表中

X, Expr, ExprList, Result, Y = vars('X, Expr, ExprList, Result, Y') # 语句不能放在列表中

_ = dummy._ # 哑变量
a = local.a # 局部变量
x = v.x # 全局变量

oad.ver090[
# oad program samples:
use.a.b.c,
use [_.a, _.x.y._.b],  # import name  
use.a.b/[_.a, _.b >> x], #重命名
use.a.b.all,
use.a/'a*',
use.a/'test_*1',
v.a_trt_b, # 刚导入的变量

do[
  write(1),
  when(1).write(2)
],

loop(10) [write(1)],
loop(10) [write(1), write(2)],
loop(10) [write(1), write(2)],
##  loop.write(1), #无限循环
loop[write(1), write(2)], #无限循环
put.i==0,  #赋值
put.my.i==0,  # 最内层局部变量赋值
put.out.i==0, # 外层变量赋值
put.i^2==0, # 外层变量赋值
put.i.j.z==(0,1,2), #i.j.z = (0,1,2)
put[out.i, a, a^3]==(0,1,2),
loop(100) [label.a, 
          inc(i), 
          if_(i==1).next, 
          if_(i==1).next.a, #再一次执行label为a的块
          write(i), 
          if_(i==5).exit, #从block a退出，返回None
          if_(i==5).exit >12, #从block a退出，返回12
          if_(i==5).exit.a  >>12, #从block a退出，返回12
          goto.a, #要不要实现它？？？
          ], #跳到下一轮循环，退出循环
write(1).until(i<3),
do [write(1), 1].  until(i==3),

'''block comment''',  
"block comment",

let ({a: 1,
      b: 2}). 
   do[write(a,b)],

do[write(a,b)].where({a:1,b:2}),

fun. a(x)== write(1), #覆盖与a(x)匹配的整个定义
fun. a==
  at(x)  [write(1)], #覆盖a的整个定义
fun. a(x) <= [write(2)], #在前面插入定义
fun. a(x) >= [write(2)],#在后面附加定义
fun==at(x)[1]
       (y)[y],
fun(x)==[1],
fun(x)==at[1][2],
macro. a(x,y) == write(2),
macro. a ==
  at(x,y).  write(2),
macro. a(x,[y],{a:1}) >= (write(2)), #可选参数，关键字参数
fun. a (x) == [], #删除函数a中与(x)一致的定义
fun. a == [], # 删除函数a的整个定义
- fun.a/3,
- fun.a(x),

##  rule. r1 == at(1) [write(1)],
##  rules. rs1 == at(1) [write(1)],
##  fun. a - rule.r1, #从函数a中删除匹配规则r1的规则
##  fun. a - rules.rs1, #从函数a中删除匹配规则集rs1的规则

each.i[1:10].
  do[write(i)],
label.a % 
each.i[1:10].j[1:10].
  do[write(i, j)],
  
each(i,j)[zip(range(5), range(5))].
  do [write(i,j)],

case(x).of(1)[write(1)]
      .of(2)[write(2)],

case(x)/{1: write(1),
       2: write(2),
       els: write(3)
      },

on({f1: open('readme.txt'),
    f2: open('out.txt', 'w')}).
  do [write(f1, 'hello')],

loop(10)[some.char(x)*10],  

py.open('readme.txt'),

some.char(x)*10,
any.char(x)*10,
may.char(x), # 可选字符

sub(x, 1),    # 成为x-1, 又可以正常地使用运算符了。
x-1,          # 与上面意思相同
-char,        # 可以定义成optional(char),漂亮
+char,        # 可以定义成some(char)
~+char,       # 非贪婪的一个或多个字符
char[:],      # char重复任意次（包括0次）
char[:]/',',  # 逗号分隔的列表
char*5,       # char重复五次
char[5],      # char重复5次
char[5]/space,      # 空格分隔的字符列表，重复5次
element(x,y,z)[:]&(x+y)>>z, #任意项element(x,y,z)以模板x+y收集到z
char[:5],     # char不大于五次
char[5:],      # char至少五次

fun. evalRule(Result)== sexpression(Expr2)+eof+is_(Result, eval_.getvalue(lExpr2)),

fun. sexpression== 
  at(Result)  [char('{')+sexpression(Expr2)+char('}')+setvalue(Result, eval_.getvalue(Expr2))]
  (Expr)     [atomExpression(Expr)]
             [bracketExpression(Expr)]
             [puncExpression(Expr)],
  
fun. sexpression== {
  Result:  char('{')+sexpression(Expr2)+char('}')+setvalue(Result, eval_.getvalue(Expr2)),
  Expr:   [atomExpression(Expr),
           bracketExpression(Expr),
           puncExpression(Expr)]},
  
fun. sexpression== at
  (Result)[char('{')+sexpression(Expr2)+char('}')+setvalue(Result, eval_.getvalue(Expr2))]
  (Expr)[atomExpression(Expr)]
        [bracketExpression(Expr)]
        [puncExpression(Expr)],
fun. atomExpression(X)== [number(X) | dqstring(X) |symbol(X)],
fun. bracketExpression (ExprList)== at
    [char('(')+spaces0(_)+sexpressionList(ExprList)+spaces0(_)+char(')')]
    [char('[')+spaces0(_)+sexpressionList(ExprList)+spaces0(_)+char(']')],
fun. puncExpression ==
    at('quote', Expr)  [char("'")/sexpression(Expr)]
    .at('quote', Expr)  [('quasiquote', Expr)==  char("`")+sexpression(Expr)]
    .at('unquote-splicing', Expr)  [literal(",@")+sexpression(Expr)]
    .at('unquote', Expr)  [char(",")+(sexpression, Expr)],
fun. sexpressionList==
  at(Expr,ExprList)   [sexpression(Expr)+condSpace+sexpressionList(ExprList)]
  (NIL)[epsilon],
fun. sexpression1(Expr)>= [spaces0(_)+sexpressionList(Expr)+spaces0(_)],
fun. condSpace()>=  [ifp(notFollowChars('([])')+notFollowByChars('([])')+not_(eof)).spaces(_)
        .spaces0(_)]
]