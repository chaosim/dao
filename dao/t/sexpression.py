from dao import term
from dao.term import Cons, nil, conslist as L, cons2tuple 
from dao.term import var, vars, DummyVar #, Command, CommandCall
from dao.special import function, eval_, from_, quote, in_module
from dao.solve import to_sexpression

from dao import builtin

from dao.builtins.control import and_p, or_p, if_p, not_p
from dao.builtins.matcher import null
from dao.builtins.terminal import char, spaces0, spaces, eoi, integer, literal
from dao.builtins.terminal import dqstring, not_lead_chars, not_follow_chars
from dao.builtins.term import setvalue, pycall, is_, define
from dao.builtins.quasiquote import quasiquote, unquote, unquote_splice

from dao.t.builtins.globalenv import sexpression as sexpression_module

#from dao.builtins.rule import *
#from dao.builtins.io import *
#from dao.builtins.container import *
#from dao.builtins.arith import *

_SYMBOL_FORBID_CHARS = '\'`",;.: \r\n\t[]{}()'

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
  sym = var(name)
  for _ in term.unify(result, sym, solver.env):
    solver.parse_state = text, p
    yield cont, True
  solver.parse_state = text, pos

x, exp, exp_list, result, y, exp2 = vars('x, exp, exp_list, result, y, exp2')

(sexpression1, sexpression, bracket_expression, punct_expression, sexpression_list, 
 atom, maybe_spaces, eval_parse_result) = vars(
   'sexpression1, sexpression, bracket_expression, punct_expression, sexpression_list, '
 'atom, maybe_spaces, eval_parse_result')

_ = DummyVar('_')

def from_sexp(var):
  return from_(sexpression_module, var)

defines = in_module(sexpression_module, 
  
  define(atom, function(
    ([x], integer(x)),
    ([x], dqstring(x)),
    ([x], symbol(x))
    )),
  
  define(bracket_expression, function(
    ([exp_list], and_p(char('('), spaces0(_), from_sexp(sexpression_list)(exp_list), spaces0(_), char(')'))),
    ([exp_list], and_p(char('['), spaces0(_), from_sexp(sexpression_list)(exp_list), spaces0(_), char(']'))))),
  
  define(punct_expression, function(
    ([L(quote, exp)], and_p(char("'"), from_sexp(sexpression)(exp))),
    ([L(quasiquote, exp)], and_p(char("`"), from_sexp(sexpression)(exp))),
    ([L(unquote_splice, exp)], and_p(literal(",@"), from_sexp(sexpression)(exp))),
    ([L(unquote, exp)], and_p(char(","), from_sexp(sexpression)(exp))))),
  
  define(sexpression_list, function(
    ([Cons(exp, exp_list)], and_p(from_sexp(sexpression)(exp), from_sexp(maybe_spaces)(), from_sexp(sexpression_list)(exp_list))),
    ([nil], null))),
  
  define(sexpression1, function(
    ([exp], and_p(spaces0(_), from_sexp(sexpression_list)(exp), spaces0(_))))),
  
  define(maybe_spaces, function(
    ([], or_p(if_p(and_p(not_lead_chars('([])'), not_follow_chars('([])'), not_p(eoi)),
                   spaces(_)),
          spaces0(_) ) ) ) ),
  
  define(sexpression, function(
     # dynamic grammar arises!
    ([result], and_p(char('{'), from_sexp(sexpression)(exp2), char('}'), 
                     setvalue(result, eval_(pycall(cons2tuple, exp2))))),

    ([exp], from_sexp(atom)(exp)),
    ([exp], from_sexp(bracket_expression)(exp)),
    ([exp], from_sexp(punct_expression)(exp))),
   ),
  )
