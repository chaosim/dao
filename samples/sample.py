# -*- coding: utf-8 -*-

# when lisp meets prolog in python

# 假如有一门语言，(),{},[],冒号，逗号，分号，句号，空格，换行，缩进等等都是运算符，
# 而且都可以任意重载，并且由程序员指定运算符的优先级，那么就可以随意定义新语言了。

from oad import *
from oad.dinpy import *

from oad.term import nil
from oad.builtins.format import write
from oad.builtins.terminal import *
from oad.builtins.arith import *
from oad.special import eval_
from oad.builtins.term import setvalue
from oad.builtins.matcher import nullword
from oad.builtins.control import if_p, not_p
from oad.builtins.term import is_

dao.version = '0.1.0'

_a = _.a # 哑变量
##a = local.a # 局部变量
a, x, y = v.a, v.x, v.y # 普通变量
i, j = var.i.j

dao[
# oad program samples:

##use.a.b.c,
##use [_.a, _.x.y._.b],  # import name  
##use.a.b/[_.a, _.b >> x], #重命名
##use.a.b.all,
##use.a/'a*',
##use.a/'test_*1',

v.a_trt_b, # 刚导入的变量

block.block1[
  loop[
    write(1),
##    next.loop,
##    exit,
    write(2),
    exit >>3,
  ]
]]
print dao.code
print dao.eval()

dao[
loop(10) [write(1)]
]
print dao.code
print dao.eval()

dao[
loop(10) [write(1), write(2)],
loop(10) [write(1), write(2)],
loop[write(1), write(2)], #无限循环
put.i==0,  #赋值
put.i.j.z==(0,1,2), #i.j.z = (0,1,2)
##put[out.i, my.a, globl.b, a, a^3]==(0,1,2),
label.a %
loop(100) [ #label.a, 
##          inc(i), 
          iff (i==1) [next], 
          iff (i==1) [next.loop], #再一次执行label为a的块
          write(i), 
          iff(i==5) [exit], #从block a退出，返回None
          iff(i==5) [exit >>12], #从block a退出，返回12
          iff(i==5) [exit.loop1 >>12], #从block a退出，返回12
##          goto.a, #要不要实现它？？？
          ], #跳到下一轮循环，退出循环
do[write(1)].when(i<3),
do[write(1), 1].until(i==3),

'''block comment''',  
"block comment",

let ({v.a: 1,
      v.b: 2}). 
   do[write(v.a,v.b)],

do[write(v.a,v.b)].where({v.a:1,v.b:2}),

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

each(i)[1:10].
  do[write(i)],
  
label.a % 
each.i[1:10].j[1:10].
  do[write(i, j)],
  
each(i,j)[zip(range(5), range(5))].
  do [write(i,j)],

case(x).of(1)[write(1)]
      .of(2)[write(2)],

##on({f1: open('readme.txt'),
##    f2: open('out.txt', 'w')}).
##  do [write(f1, 'hello')],

loop(10)[char(x)[10]],  

put.command==open,
py.command('readme.txt'),
py(open, 'readme.txt'), 

char(x)[10][10],
char(x)[10][:],
-char(x), # 可选字符
]

dao[
x-1,          
-char(x),               # 非贪婪可选
+char(x),               # 贪婪可选
char(x)[:],             # 非贪婪char(x)重复任意次（包括0次）
+char(x)[1:],           # 贪婪char(x)重复任意次（不包括0次）
-char(x)[1:],           # 懒惰char(x)重复任意次（不包括0次）
char(x)[:]/char(' '),   # 空格分隔的列表
char(x)[5],             # char(x)重复5次
char(x)[5]/char(','),   # 空格分隔的char(x)列表，重复5次
char(x)[:]/' '%(x,y)*a, #任意项char(x)以模板x,y收集到a
char(x)[:5],            # char(x)不大于五次
char(x)[5:],            # char(x)至少五次
char(x)[5:8],           # char(x)至少五次
]

X, Expr, Expr2, ExprList, Result, Y = var.X.Y.Expr.Expr2.ExprList.Result # 语句不能放在列表中

X, Expr, ExprList, Result, Y, sexpression = vars('X, Expr, ExprList, Result, Y, sexpression') # 语句不能放在列表中
stringExpression, bracketExpression, puncExpression, sexpressionList, condSpace = vars(
  'stringExpression, bracketExpression, puncExpression, sexpressionList, condSpace')

dao[
fun. evalRule(Result) == sexpression(Expr2)+eos+is_(Result, eval_<getvalue<Expr2),

fun. sexpression == at
  (Result)
    [char('{')+sexpression(Expr2)+char('}')+setvalue(Result, eval_<getvalue<Expr2)]
  (Expr)
    [stringExpression(Expr)]
    [bracketExpression(Expr)]
    [puncExpression(Expr)],
  
fun. sexpression == at
  (Result)
    [char('{')+sexpression(Expr2)+char('}')+setvalue(Result, eval_<getvalue<Expr2)]
  (Expr)
    [stringExpression(Expr)]
    [bracketExpression(Expr)]
    [puncExpression(Expr)],
  
fun. sexpression == at
  (Result)
    [char('{')+sexpression(Expr2)+char('}')+setvalue(Result, eval_<getvalue<Expr2)]
  (Expr)
    [stringExpression(Expr)]
    [bracketExpression(Expr)]
    [puncExpression(Expr)],
    
fun. stringExpression(X) == at [number(X)] [dqstring(X)] [symbol(X)],

fun. bracketExpression (ExprList) == at
    [char('(')+spaces0(_)+sexpressionList(ExprList)+spaces0(_)+char(')')]
    [char('[')+spaces0(_)+sexpressionList(ExprList)+spaces0(_)+char(']')],
    
fun. puncExpression == at
  ('quote', Expr) [char("'")][sexpression(Expr)]
  ('quote', Expr) [char("`")+sexpression(Expr)]
  ('unquote-splicing', Expr) [literal(",@")+sexpression(Expr)]
  ('unquote', Expr) [char(",")+(sexpression, Expr)],
  
fun. sexpressionList == at
  (Expr, ExprList) [sexpression(Expr)+condSpace+sexpressionList(ExprList)]
  (nil) [nullword],
  
fun. sexpression1(Expr) >= [spaces0(_)+sexpressionList(Expr)+spaces0(_)],

fun. condSpace() >=  [if_p(not_plead_chars('([])')+not_pfollow_chars('([])')+not_p(eos))+spaces(_)
        +spaces0(_)]
]

