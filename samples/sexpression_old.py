from dao.term import Var, DummyVar, LocalVar, NIL, Cons, conslist
from dao.rule import Rule
from dao.builtins.terminal import spaces0, spaces, char, number, symbol, dqstring
from dao.builtins.terminal import epsilon, literal, eos
from dao.builtins.terminal import notFollowChars, notFollowByChars
from dao.builtins.matcher import parallel, any, some
from dao.builtins.control import if_p, not_p, and_, or_
from dao.builtins.term import getvalue, setvalue
from dao.builtins.arith import define, is_, assign
from dao.parse import Grammar

(sexpression1, sexpression, bracketExpression, puncExpression, sexpressionList, 
 stringExpression, condSpace, evalRule) = (Var(name) for name in (
   'sexpression1',  'sexpression', "bracketExpression", "puncExpression",  'sexpressionList', 
   'stringExpression', 'condSpace', 'evalRule'))

_ = DummyVar('_')
X, Expr, ExprList, Result, Y = Var('X'), Var('Expr'), Var('ExprList'), Var('Result'), Var('Y')
Expr2 = LocalVar('Expr2')
from dao.tests.util import function, eval_

functions = [
  (evalRule, (function, 
    ([Result], (and_, (sexpression, Expr2), [eos], (is_, Result, (eval_, (getvalue, Expr2))))))),
  (sexpression, (function, 
    ([Result], (and_, (char, '{'), (sexpression, Expr2), (char, '}'), (setvalue, Result, (eval_, (getvalue, Expr2))))),
    ([Expr], (stringExpression, Expr)),
    ([Expr], (bracketExpression, Expr)),
    ([Expr], (puncExpression, Expr)))),
  (stringExpression, (function, 
    ([X], (number, X)),
    ([X], (dqstring, X)),
    ([X], (symbol, X)))),
  (bracketExpression, (function, 
    ([ExprList], (and_, (char, '('), (spaces0, _), (sexpressionList, ExprList), (spaces0, _), (char, ')'))),
    ([ExprList], (and_, (char, '['), (spaces0, _), (sexpressionList, ExprList), (spaces0, _), (char, ']'))))),
  (puncExpression, (function, 
    ([('quote', Expr)], (and_, (char, "'"), (sexpression, Expr))),
    ([('quasiquote', Expr)], (and_, (char, "`"), (sexpression, Expr))),
    ([('unquote-splicing', Expr)], (and_, (literal, ",@"), (sexpression, Expr))),
    ([('unquote', Expr)], (and_, (char, ","), (sexpression, Expr))))),
  (sexpressionList, (function, 
    ([Cons(Expr, ExprList)], (and_, (sexpression, Expr), [condSpace], (sexpressionList, ExprList))),
    ([NIL], [epsilon]))),
  (sexpression1, (function, 
    ([Expr], (and_, (spaces0, _), (sexpressionList, Expr), (spaces0, _))))),
  (condSpace, (function, 
    ([], (or_, (if_p, (and_, (notFollowChars, '([])'), (notFollowByChars, '([])'), (not_p, [eos])),
            (spaces, _)),
          (spaces0, _)))))]
 
grammar1 = Grammar([number, Expr], functions, Expr)
grammar2 = Grammar([sexpressionList, Expr], functions, Expr)
grammar = Grammar([sexpression, Expr], functions, Expr)
grammar3 = Grammar([evalRule, Result], functions, Result)
