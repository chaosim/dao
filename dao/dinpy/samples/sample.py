# -*- coding: utf-8 -*-

# when lisp meets prolog in python

# dinpy program samples

from dao.dinpy import *

dinpy.version = '0.7.2'

dinpy[

println('parsing:'),

parse_text(char(x1)+any(~char('b')+some(char(x1)))+eoi, 'abaaaa'),

prin(x1),

println('\n\nfindall:'),

let( f << fun()[2][3] ) 
  .do[ findall(is_(x, f()), x, y), prin(y) ],

println('\n\nrepeat/fail:'),

let( i<<0 ). do[ repeat, prin(i), ++i, iff(i<3).do[fail] ],

println('\n\nand_p(b(x),c(x)): '),

letr( a << fun(x) [ and_p(b(x),c(x)) ]
                  [ d(x) ],
      b << fun(1) ['b1']
              (4) ['b4'],
      c << fun(4) ['c4'], 
      d << fun(3) ['d3'], 
     ).do[ 
     a(x), prin(x) ],

println('\n\neach(i)[1:3]: '),

each(i)[1:3].
  loop[prin(i)],

println('\n\neach(i,j)[1:3][1:3]: '),

each(i,j)[1:3][1:3].
  loop[prin(i, j)],
  
label.a % 
each(i,j)[1:10][1:10].
  loop[prin(i, j)],
  
each(i,j)[zip(range(5), range(5))].
  loop [prin(i,j)],

println('\n\ncase: '),

case(1).
  of(1)[prin(1)].
  of(2)[prin(2)],
fun. a== at(x)  [prin('sdgsgd')],
a(1),

println('\n\nloop: '),

loop(3)[prin(1)],

println('\n\n++, --: '),

i << 0, prin(i), ++i , prin(i), --i, prin(i),

println('\n\nblock / exit: '),

block.block1[
  loop[
    prin(1),
    prin(2),
    exit >>3,
  ]
],

println('\n\nloop times: '),

loop(3) [prin(1)],

loop(2) [prin(3), prin(4)],
loop[ prin(1), prin(2), exit ], # 无限循环
put.i.j.z << (0, 1, 2), 

println('\n\nloop: '),

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
  iff (i==1) .do[next.loop],
], 

println('\n\nloop-when: '),

i << 0,
loop[ i << i+1, prin(i)].when(i==3),

println('\n\nloop-until: '),

i << 0,
loop[ ++i, prin(i)].until(i==3),

'''block comment''',  
"block comment",

println('\n\nlet: '),

let (a << 1, 
     b << 2) 
  .do[prin(v.a, v.b)],
  
println('\n\nlet( a/ b/ c << range(3)) '),

let( a/ b/ c << range(3)) 
  .do[prin(v.a, v.b, v.c)],
  
println('\n\nfun definitions '),

##char(x)[5:8],           # 5-8 times char(x) 
fun. a(x)[prin(1)], # redefine a(x)
fun. a == at(x)  [prin(x)], # redefine a 
fun. a(1) <= [prin(1)], # insert defintion before rule list of a/1
fun. a(3) >= [prin(3)], # append definition after rule list of a/1
fun (x)[1]
    (y)[y], # define a function with two rules
fun [1], # same as fun()[1]
fun(x)[1][2], # same as fun()[1[2]

a(1), # call function a(1)

- fun.a/3, # remove rule list which head have 3 arguments in a's function defintion
- fun.a(x), # remove rules which head matches with (x) in a's function defintion

println('\n\neach(i)[1:10]'),

each(i)[1:10].
  loop[prin(i)],
  
println('\n\neach(i,j)[1:10][1:10]'),

label.a % 
each(i,j)[1:10][1:10].
  loop[prin(i, j)],
  
println('\n\neach(i,j)[zip(range(5), range(5))]'),

each(i,j)[zip(range(5), range(5))].
  loop [prin(i,j)],

println('\n\ncase(1)'),
case(1)
  .of(1)[prin(1)]
  .of(2)[prin(2)],

#v.command << open, # assign open to var command
#pycall(open, 'readme.txt'), 

set_text('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'),
# char(x)[10][10],
# char(x)[10][:],
x-1,          
-char(x),                 # nongreedy optional
~char(x),                 # greedy optional
char(x)[:],               # nongreedy any times char(x), include 0 times.
# ~char(x)[1:],           # greedy any times char(x), exclude 0 times.
# -char(x)[1:],           # lazy some times char(x), exclude 0 times.
# char(x)[:]/char(' '),   # list separated by ' ', include 0 times of x
# char(x)[5],             # 5 times char(x)
# char(x)[5]/char(','),   # 5 times list separated by ','
# char(x)[:]/' '%x*y,     # any times list separated by  ' ', collect result by x in y
# char(x)[:5],            # no more 5 times char(x)
# char(x)[5:],            # 5 times or more char(x)
]


X, Expr, Expr2, ExprList, Result, Y, sexpression = symbols(
'X, Expr, Expr2, ExprList, Result, Y, sexpression') 

stringExpression, bracketExpression, puncExpression, sexpressionList, condSpace = symbols(
  'stringExpression, bracketExpression, puncExpression, sexpressionList, condSpace')

dinpy[
fun. evalRule(Result) 
  [sexpression(Expr2)+eoi+is_(Result, eval_<getvalue<Expr2)],

fun. sexpression
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
    
fun. stringExpression(X) [number(X)] [dqstring(X)] [uLetterdigitString(X)],

fun. bracketExpression (ExprList)
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
  
fun. sexpression1(Expr) [spaces0(_)+sexpressionList(Expr)+spaces0(_)],

fun. condSpace() 
  [ if_p(not_p(lead_chars('([])'))+not_follow_chars('([])')
        +not_p(eoi))+spaces(_)+spaces0(_)
  ]
]

dinpy.eval()

if 1:
  parsing:
  a 
  
  findall:
  [2, 3] 
  
  repeat/fail:
  0 1 2 
  
  and_p(b(x),c(x)): 
  4 
  
  each(i)[1:3]: 
  1 2 
  
  each(i,j)[1:3][1:3]: 
  1 1 2 2 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 0 0 1 1 2 2 3 3 4 4 
  
  case: 
  1 sdgsgd 
  
  loop: 
  1 1 1 
  
  ++, --: 
  0 1 0 
  
  block / exit: 
  1 2 
  
  loop times: 
  1 1 1 3 4 3 4 1 2 
  
  loop: 
  -1 11 11 10 22 22 
  
  loop-when: 
  1 
  
  loop-until: 
  1 2 3 
  
  let: 
  1 2 
  
  let( a/ b/ c << range(3)) 
  0 1 2 
  
  fun definitions 
  1 
  
  each(i)[1:10]
  1 2 3 4 5 6 7 8 9 
  
  each(i,j)[1:10][1:10]
  1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 
  
  each(i,j)[zip(range(5), range(5))]
  0 0 1 1 2 2 3 3 4 4 
  
  case(1)
  1
    