from dao.rule import Rule

from dao import term
from dao.term import nil, Cons, conslist as L, cons2tuple
from dao.command import Command
from dao.term import vars, DummyVar, CommandCall
from dao.solve import make_solver, set_run_mode, noninteractive
from dao import builtin

from dao.special import *
from dao.builtins.arith import *
from dao.builtins.control import *
from dao.builtins.io import *
from dao.builtins.container import *
from dao.builtins.parser import *
from dao.builtins.matcher import *
from dao.builtins.terminal import *
from dao.builtins.rule import *
from dao.builtins.term import *
from dao.builtins.quasiquote import *

set_run_mode(noninteractive)

from dao.base import is_subclass

_builtins = {}

def collocet_builtins():    
  for name, obj in globals().items():
    if isinstance(obj, Command) or is_subclass(obj, SpecialForm):
      try: symbol = obj.symbol
      except AttributeError:
        try: symbol = obj.name
        except AttributeError: symbol = name
      _builtins[symbol] = obj

collocet_builtins()

_builtins.update({'let':let, 'letr':letr, 'lambda':lambda_})

_var_cache = {}
def var(name):
  if name[0]=='_': klass = DummyVar
  else: klass = Var
  return _var_cache.setdefault(klass, {}).setdefault(name, klass(name))

_SYMBOL_FORBID_CHARS = '\'", \r\n\t[]{}()`'

@builtin.macro()
def symbol(solver, result):
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

def sexpression2daoexpression(item):
  if isinstance(item, Cons):
    head = sexpression2daoexpression(item.head)
    if head==let or head==letr:
      bindings = tuple((var, sexpression2daoexpression(exp)) for var, exp in item.tail.head)
      body = tuple(sexpression2daoexpression(x) for x in item.tail.tail)
      return head(bindings, *body)
    elif head==lambda_:
      vars = tuple(item.tail.head)
      body = tuple(sexpression2daoexpression(x) for x in item.tail.tail)
      return head(vars, *body)
    elif head==FunctionForm or head==MacroForm:
      return head(*tuple((cons2tuple(rule.head),)+tuple(sexpression2daoexpression(stmt)
                                              for stmt in rule.tail) 
                    for rule in item.tail)) 
    return head(*tuple(sexpression2daoexpression(x) for x in item.tail))
  else: return item

X, Expr, ExprList, Result, Y, Expr2 = vars('X, Expr, ExprList, Result, Y, Expr2')

(sexpression1, sexpression, bracketExpression, puncExpression, sexpressionList, 
 atom_expression, spaces_on_condition, eval_parse_result) = vars(
   'sexpression1, sexpression, bracketExpression, puncExpression, sexpressionList, '
 'atom_expression, spaces_on_condition, eval_parse_result')

_ = DummyVar('_')

sexpression_rules = [
  
  (atom_expression, function(
    ([X], integer(X)),
    ([X], dqstring(X)),
    ([X], symbol(X))
    )),
  
  (bracketExpression, function(
    ([ExprList], and_p(char('('), whitespaces0, sexpressionList(ExprList), whitespaces0, char(')'))),
    ([ExprList], and_p(char('['), whitespaces0, sexpressionList(ExprList), whitespaces0, char(']'))))),
  
  (puncExpression, function(
    ([L(quote, Expr)], and_p(char("'"), sexpression(Expr))),
    ([L(quasiquote, Expr)], and_p(char("`"), sexpression(Expr))),
    ([L(unquote_splice, Expr)], and_p(literal(",@"), sexpression(Expr))),
    ([L(unquote, Expr)], and_p(char(","), sexpression(Expr))))),
  
  (sexpressionList, function(
    ([Cons(Expr, ExprList)], and_p(sexpression(Expr), spaces_on_condition(), sexpressionList(ExprList))),
    ([nil], null))),
  
  (sexpression1, function(
    ([Expr], and_p(whitespaces0, sexpressionList(Expr), whitespaces0)))),
  
  (spaces_on_condition, function(
    ([], or_p(if_p(and_p(not_lead_chars('([])'), not_follow_chars('([])'), not_p(eoi)),
                   spaces),
          whitespaces0 ) ) ) ),
  
  (sexpression, function(
     # dynamic grammar arises!
    ([Result], and_p(char('{'), sexpression(Expr2), char('}'), 
                     setvalue(Result, eval_(pycall(cons2tuple, Expr2))))),

    ([Expr], atom_expression(Expr)),
    ([Expr], bracketExpression(Expr)),
    ([Expr], puncExpression(Expr))),
   ),
  
  # the kernel of dynamic grammar  
  (eval_parse_result, function(
    ([Result], and_p(sexpression(Expr2), eoi, 
          is_(Result, eval_(pycall(cons2tuple,Expr2))))))),#pycall(sexpression2daoexpression, Expr2)
  
  ]

class Grammar:
  def __init__(self, start, rules, result):
    self.rules, self.start, self.result = rules, start, result

def make_parse_statement(grammar, text):
  return letr(grammar.rules, 
              set_text(text), and_p(grammar.start, eoi), grammar.result)

def parse(grammar, text):
  solver = make_solver()
  exp = make_parse_statement(grammar, text)
  sexp = to_sexpression(exp)
  return solver.eval(exp)

def parse_eval(grammar, text):
  solver = make_solver()
  exp = make_parse_statement(grammar, text)
  sexp = to_sexpression(exp)
  exp = solver.eval(sexp) 
  
  # Do not need the second eval any more, with the rule 'eval_parse_result'.
  if isinstance(exp, Cons): exp = cons2tuple(exp)
  return solver.eval(exp)#sexpression2daoexpression

eval = parse_eval

grammar = Grammar(sexpression(Expr), sexpression_rules, Expr)
grammar1 = Grammar(integer(Expr), sexpression_rules, Expr)
grammar2 = Grammar(sexpressionList(Expr), sexpression_rules, Expr)

# use the eval_parse_result to eval in parse at the end.
grammar3 = Grammar(eval_parse_result(Result), sexpression_rules, Result)
