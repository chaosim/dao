# -*- coding: utf-8 -*-

# when lisp meets prolog in python

# dao program samples

from dao.dinpy import *

dao.version = '0.1.0'

dao[

##use.a.b.c,
##use [_.a, _.x.y._.b],  # import name  
##use.a.b/[_.a, _.b >> x], #重命名
##use.a.b.all,
##use.a/'a*',
##use.a/'test_*1',

v.a_trt_b, # 刚导入的变量

each(i)[1:3].
  loop[prin(i)],

each(i,j)[1:3][1:3].
  loop[prin(i, j)],
  
label.a % 
each(i,j)[1:10][1:10].
  loop[prin(i, j)],
  
each(i,j)[zip(range(5), range(5))].
  loop [prin(i,j)],

case(1).
  of(1)[prin(1)].
  of(2)[prin(2)],
fun. a== at(x)  [prin('sdgsgd')],
a(1),

loop(3)[prin(1)],

i << 0, prin(i), ++i , prin(i), --i, prin(i),

block.block1[
  loop[
    prin(1),
##    next.loop,
##    exit,
    prin(2),
    exit >>3,
  ]
],

loop(3) [prin(1)],

loop(2) [prin(3), prin(4)],
loop[ prin(1), prin(2), exit ], # 无限循环
put.i.j.z << (0, 1, 2), 

i << 0,
label.a %
loop[
  -- i,
  prin(i), 
  ++ i,
  ++ i,
  i << i+10,
  prin(i), 
  iff (i==1) .do[next], 
  prin(i), 
  iff (i>20) .do[exit], 
  iff (i==1) .do[next.loop], #再一次执行label为a的块
##  iff(eq(i,3)) .do[exit], #从block a退出，返回None
##  iff(eq(i,3)) .do[exit >>12], #从block a退出，返回12
##  iff(eq(i,3)) .do[exit.loop1 >>12], #从block a退出，返回12
], 

i << 0,
loop[ i << i+1, prin(i)].when(i==3),

i << 0,
loop[ ++i, prin(i)].until(i==3),

'''block comment''',  
"block comment",

let (a << 1, 
     b << 2) 
  .do[prin(v.a, v.b)],
  
##let (a << b << c << 1) #serial let is waiting to implemented.
##  .do[prin(v.a, v.b)],

let( a/ b/ c << range(3)) 
  .do[prin(v.a, v.b, v.c)],
  
prin('adafa'),

fun. a(x)== [prin(1)], #覆盖与a(x)匹配的整个定义
fun. a == at(x)  [prin(x)], #覆盖a的整个定义
fun. a(1) <= [prin(1)], #在前面插入定义
fun. a(3) >= [prin(3)], #在后面附加定义
fun (x)[1]
    (y)[y],
fun [1],
fun(x)[1][2],
##macro. a(x,[y],{a:1}) >= (prin(2)), #可选参数，关键字参数
##fun. a (x) == [], #删除函数a中与(x)一致的定义
##fun. a == [], # 删除函数a的整个定义
a(1),
- fun.a/3,
- fun.a(x),

##a('affd'), # NoSolutionFound

##  rule. r1 == at(1) [prin(1)],
##  rules. rs1 == at(1) [prin(1)],
##  fun. a - rule.r1, #从函数a中删除匹配规则r1的规则
##  fun. a - rules.rs1, #从函数a中删除匹配规则集rs1的规则

each(i)[1:10].
  loop[prin(i)],
  
label.a % 
each(i,j)[1:10][1:10].
  loop[prin(i, j)],
  
each(i,j)[zip(range(5), range(5))].
  loop [prin(i,j)],

case(1)
  .of(1)[prin(1)]
  .of(2)[prin(2)],

##on(f1==open('readme.txt'),
##    f2==open('out.txt', 'w')).
##  do [prin(f1, 'hello')],

##loop(10)[char(x)[10]],  

v.command << open,
##v.command('readme.txt'),
##pycall(open, 'readme.txt'), 
##char(x)[10][10],
##char(x)[10][:],
set_text('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'),
x-1,          
-char(x),               # 非贪婪可选
+char(x),               # 贪婪可选
char(x)[:],             # 非贪婪char(x)重复任意次（包括0次）
+char(x)[1:],           # 贪婪char(x)重复任意次（不包括0次）
##-char(x)[1:],           # 懒惰char(x)重复任意次（不包括0次）
##char(x)[:]/char(' '),   # 空格分隔的列表
##char(x)[5],             # char(x)重复5次
##char(x)[5]/char(','),   # 空格分隔的char(x)列表，重复5次
##char(x)[:]/' '%(x,y)*a, #任意项char(x)以模板x,y收集到a
##char(x)[:5],            # char(x)不大于五次
##char(x)[5:],            # char(x)至少五次
##char(x)[5:8],           # char(x)至少五次
]

X, Expr, Expr2, ExprList, Result, Y, sexpression = symbols(
'X, Expr, Expr2, ExprList, Result, Y, sexpression') 
stringExpression, bracketExpression, puncExpression, sexpressionList, condSpace = symbols(
  'stringExpression, bracketExpression, puncExpression, sexpressionList, condSpace')
##symbols('X, Expr, Expr2, ExprList, Result, Y, sexpression' 
##  'stringExpression, bracketExpression, puncExpression, sexpressionList, condSpace')

dao[
fun. evalRule(Result) == [sexpression(Expr2)+eos+is_(Result, eval_<getvalue<Expr2)],

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
    
fun. stringExpression(X) == at [number(X)] [dqstring(X)] [uLetterdigitString(X)],

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
  
fun. sexpression1(Expr) == [spaces0(_)+sexpressionList(Expr)+spaces0(_)],

fun. condSpace() ==  
  [ if_p(not_p(lead_chars('([])'))+not_follow_chars('([])')
        +not_p(eos))+spaces(_)+spaces0(_)
  ]
]

dao.eval()
