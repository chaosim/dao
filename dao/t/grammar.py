# -*- coding: utf-8 -*-

from dao.dinpy import *

dinpy.version = '0.7.2'

(sexpression1, sexpression, bracketExpression, puncExpression, sexpressionList, 
 stringExpression, condSpace, evalRule) = (var. sexpression1.sexpression.bracketExpression.puncExpression.sexpressionList
 .stringExpression.condSpace.evalRule)

_ = dummy._
a= local.a
X, Expr, ExprList, Result, Y = var. X. Y. Expr. ExprList. Result 
Expr2 = var.Expr2

'''
# 1*2+3+4
# 1+2*3 == 5/6+3
# 1+((2-(-3)*6)+2) == 1/(2+3)


number(x) -> E(x, 100)

E(x,100) : number(x)
E((op,x,y), p2): E(x, p1)+op(p2)+E(y, p3) 

E(x, p1)+op(p2)+E(y, p3) -> E((op,x,y), p2)

'''

dinpy[

letr(
  expression << fun
    ((op, E1, E2), Precedence)
       [ expression(E1, Precedence1)+binary(op, Precedence)+(Precedence1>=Precedence<=Precedence2) +expression(E2, Precedence2) ],
    (X, 100) [ number(X) ],
    
  binary << fun(add, 50) [ char('+') ],
            fun(mul, 60) [ char('*') ],
  unary << fun(positive, 90) [ char('+') ],
  
]