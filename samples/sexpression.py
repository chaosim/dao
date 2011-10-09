(sexpression1, sexpression, bracketExpression, puncExpression, sexpressionList, 
 atomExpression, condSpace, evalRule) = (var. sexpression1.sexpression.bracketExpression.puncExpression.sexpressionList
 .atomExpression.condSpace.evalRule)

_ = dummy._
a= local.a
X, Expr, ExprList, Result, Y = var. X. Y. Expr. ExprList. Result # 语句不能放在列表中
Expr2 = var.Expr2

from dao.tests.util import function, eval_

functions = [
  fun. evalRule(Result)> sexpression(Expr2)+eos+is_(Result, eval_.getvalue(lExpr2)),
  fun. sexpression> 
    [(Result)> char('{')+sexpression(Expr2)+char('}')+setvalue(Result, eval_.getvalue(Expr2)),
    (Expr)> atomExpression(Expr),
    (Expr)> bracketExpression(Expr),
    (Expr)> puncExpression(Expr)],
  fun. atomExpression(X)> 
    [number(X) | dqstring(X) |symbol(X)],
  fun. bracketExpression(ExprList)> 
    [char('(')+spaces0(_)+sexpressionList(ExprList)+spaces0(_)+char(')'),
     char('[')+spaces0(_)+sexpressionList(ExprList)+spaces0(_)+char(']')],
  fun. puncExpression> 
    [['quote', Expr]> 
        char("'")/sexpression(Expr),
     ['quasiquote', Expr]>  
        char("`")+sexpression(Expr),
     ['unquote-splicing', Expr]>  
        literal(",@")+sexpression(Expr),
     ['unquote', Expr]/  
        char(",")+(sexpression, Expr)],
  fun. sexpressionList>
    [[Expr, ExprList]>  sexpression(Expr)+condSpace+sexpressionList(ExprList),
     NIL/epsilon],
  fun. sexpression1(Expr)>  spaces0(_)+sexpressionList(Expr)+spaces0(_),
  fun. condSpace()>  ifp(notFollowChars('([])')+notFollowByChars('([])')+not_(eos)).spaces(_)
          .spaces0(_)]