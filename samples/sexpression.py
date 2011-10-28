from dao.rule import Rule

from dao import term
from dao.term import nil, Cons, conslist as L, cons2tuple
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

class Grammar:
  def __init__(self, start, rules, result):
    self.rules, self.start, self.result = rules, start, result

_builtins = {}

def collocet_builtins():
  def is_subclass(sub, sup):
    try: 
      if sup in sub.__bases__: return True
    except: return False
    for klass in sub.__bases__:
      if is_subclass(klass, sup): return True
    
  for name, obj in globals().items():
    if isinstance(obj, Command) or is_subclass(obj, SpecialForm):
      try: symbol = obj.symbol
      except AttributeError:
        try: symbol = obj.name
        except AttributeError: symbol = name
      _builtins[symbol] = obj

collocet_builtins()

_builtins.update({'let':let, 'letr':letr, 'lambda':lambda_})

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
  sym = _builtins.get(name, var(name))
  for _ in term.unify(result, sym, solver.env):
    solver.parse_state = text, p
    yield cont, True
  solver.parse_state = text, pos

def sexpression2dao_exp(item):
  if isinstance(item, Cons):
    head = sexpression2dao_exp(item.head)
    if head==let or head==letr:
      bindings = tuple((var, sexpression2dao_exp(exp)) for var, exp in item.tail.head)
      body = tuple(sexpression2dao_exp(x) for x in item.tail.tail)
      return head(bindings, *body)
    elif head==lambda_:
      vars = tuple(item.tail.head)
      body = tuple(sexpression2dao_exp(x) for x in item.tail.tail)
      return head(vars, *body)
    elif head==FunctionForm or head==MacroForm:
      return head(*tuple((cons2tuple(rule.head),)+tuple(sexpression2dao_exp(stmt)
                                              for stmt in rule.tail) 
                    for rule in item.tail)) 
    return head(*tuple(sexpression2dao_exp(x) for x in item.tail))
  else: return item

(sexpression1, sexpression, bracketExpression, puncExpression, sexpressionList, 
 stringExpression, condSpace, evalRule) = (Var(name) for name in (
   'sexpression1',  'sexpression', "bracketExpression", "puncExpression",  'sexpressionList', 
   'stringExpression', 'condSpace', 'evalRule'))

_ = DummyVar('_')
X, Expr, ExprList, Result, Y = Var('X'), Var('Expr'), Var('ExprList'), Var('Result'), Var('Y')
Expr2 = Var('Expr2')

functions = [
  (stringExpression, function(
    ([X], number(X)),
    ([X], dqstring(X)),
    ([X], symbol(X))
    )),
  (bracketExpression, function(
    ([ExprList], and_p(char('('), spaces0(_), sexpressionList(ExprList), spaces0(_), char(')'))),
    ([ExprList], and_p(char('['), spaces0(_), sexpressionList(ExprList), spaces0(_), char(']'))))),
  (puncExpression, function(
    ([L(quote, Expr)], and_p(char("'"), sexpression(Expr))),
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
          spaces0(_) ) ) ) ),
  
  (sexpression, function(
     # dynamic grammar arises!
    ([Result], and_p(char('{'), sexpression(Expr2), char('}'), 
                     setvalue(Result, eval_(pycall(sexpression2dao_exp, Expr2))))),

    ([Expr], stringExpression(Expr)),
    ([Expr], bracketExpression(Expr)),
    ([Expr], puncExpression(Expr))),
   ),
  
  # the kernel of dynamic grammar  
  (evalRule, function(
    ([Result], and_p(sexpression(Expr2), eoi, 
          is_(Result, eval_(pycall(sexpression2dao_exp, Expr2))))))),
  
  ]

def parse(grammar, text):
  solver = Solver()
  exp = letr(grammar.rules, set_text(text), 
                 and_p(grammar.start, eoi), grammar.result)
  result = solver.eval(exp)
  return cons2tuple(result) 

def eval(grammar, text):
  solver = Solver()
  exp = letr(grammar.rules, set_text(text), 
                 and_p(grammar.start, eoi), grammar.result)
  exp = solver.eval(exp)
  exp = sexpression2dao_exp(exp)
  return solver.eval(exp)

grammar = Grammar(sexpression(Expr), functions, Expr)

grammar1 = Grammar(number(Expr), functions, Expr)
grammar2 = Grammar(sexpressionList(Expr), functions, Expr)
grammar3 = Grammar(evalRule(Result), functions, Result)
