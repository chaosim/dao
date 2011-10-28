from dao.rule import Rule

from dao import term
from dao.term import nil, Cons, conslist, cons2tuple
from dao.term import Var, DummyVar, Command, CommandCall
from dao.solve import Solver, set_run_mode, noninteractive
from dao import builtin

from dao.special import *
from dao.builtins.arith import *
from dao.builtins.control import *
from dao.builtins.io import *
from dao.builtins.string import *
from dao.builtins.parser import *
from dao.builtins.matcher import *
from dao.builtins.terminal import *
from dao.builtins.rule import *
from dao.builtins.term import *

set_run_mode(noninteractive)

builtins = {}

def collocet_builtins():
  def is_subclass(sub, sup):
    try: return sup in sub.__bases__
    except: return False
    
  for name, obj in globals().items():
    if isinstance(obj, Command)  or isinstance(obj, CommandCall) \
       or is_subclass(obj, SpecialForm):
      try: symbol = obj.symbol
      except AttributeError:
        try: symbol = obj.name
        except AttributeError: symbol = name
      builtins[symbol] = obj

collocet_builtins()
builtins['let'] = let
builtins['letrec'] = letr

_SYMBOL_FORBID_CHARS = '\'", \r\n\t[]{}()`'
_var_cache = {}
def var(name):
  if name[0]=='_': klass = DummyVar
  else: klass = Var
  return _var_cache.setdefault(klass, {}).setdefault(name, klass(name))

@builtin.macro()
def symbol(solver, cont, result):
  text, pos = solver.parse_state
  if pos>=len(text): return
  char = text[pos]
  if char in _SYMBOL_FORBID_CHARS or '0'<=char<='9':
    return
  p = pos
  while p<len(text): 
    if text[p] in _SYMBOL_FORBID_CHARS: break 
    else: p += 1
  name = text[pos:p]
  sym = builtins.get(name, var(name))
  for _ in term.unify(result, sym, solver.env):
    solver.parse_state = text, p
    yield cont, True
  solver.parse_state = text, pos


class Grammar:
  def __init__(self, start, rules, result):
    self.rules, self.start, self.result = rules, start, result

def parse(grammar, text):
  solver = Solver()
  exp = letr(grammar.rules, set_text(text), 
                 and_p(grammar.start, eoi), grammar.result)
  result = solver.eval(exp)
  return cons2tuple(solver.eval(result)) 

def eval(grammar, text): #don't need any more!!!
  solver = Solver()
  exp = letr(grammar.rules, set_text(text), 
                 and_p(grammar.start, eoi), grammar.result)
  parsedExp = solver.eval(exp)
  return solver.eval(parsedExp) #, prelude=False

(sexpression1, sexpression, bracketExpression, puncExpression, sexpressionList, 
 stringExpression, condSpace, evalRule) = (Var(name) for name in (
   'sexpression1',  'sexpression', "bracketExpression", "puncExpression",  'sexpressionList', 
   'stringExpression', 'condSpace', 'evalRule'))

_ = DummyVar('_')
X, Expr, ExprList, Result, Y = Var('X'), Var('Expr'), Var('ExprList'), Var('Result'), Var('Y')
Expr2 = Var('Expr2')

functions = [
  (evalRule, function(
    ([Result], and_p(sexpression(Expr2), eoi, is_(Result, eval_(getvalue(Expr2))))))),
  (sexpression, function(
    ([Result], and_p(char('{'), sexpression(Expr2), char('}'), setvalue(Result, eval_(getvalue(Expr2))))),
    ([Expr], stringExpression(Expr)),
    ([Expr], bracketExpression(Expr)),
    ([Expr], puncExpression(Expr)))),
  (stringExpression, function(
    ([X], number(X)),
    ([X], dqstring(X)),
    ([X], symbol(X))
    )),
  (bracketExpression, function(
    ([ExprList], and_p(char('('), spaces0(_), sexpressionList(ExprList), spaces0(_), char(')'))),
    ([ExprList], and_p(char('['), spaces0(_), sexpressionList(ExprList), spaces0(_), char(']'))))),
  (puncExpression, function(
    ([('quote', Expr)], and_p(char("'"), sexpression(Expr))),
    ([('quasiquote', Expr)], and_p(char("`"), sexpression(Expr))),
    ([('unquote-splicing', Expr)], and_p(literal(",@"), sexpression(Expr))),
    ([('unquote', Expr)], and_p(char(","), sexpression(Expr))))),
  (sexpressionList, function(
    ([Cons(Expr, ExprList)], and_p(sexpression(Expr), condSpace(), sexpressionList(ExprList))),
    ([nil], null))),
  (sexpression1, function(
    ([Expr], and_p(spaces0(_), sexpressionList(Expr), spaces0(_))))),
  (condSpace, function(
    ([], or_p(if_p(and_p(not_lead_chars('([])'), not_follow_chars('([])'), not_p(eoi)),
                   spaces(_)),
          spaces0(_)))
    ))
  ]

grammar = Grammar(sexpression(Expr), functions, Expr)

grammar1 = Grammar(number(Expr), functions, Expr)
grammar2 = Grammar(sexpressionList(Expr), functions, Expr)
grammar3 = Grammar(evalRule(Result), functions, Result)
