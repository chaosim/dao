from oad.term import Var, Cons, getvalue
from oad import builtin

# type verifications

@builtin.macro('var')
def isvar(evaluator, arg):
  if isinstance(arg, Var): yield True

@builtin.macro('nonvar')
def nonvar(evaluator, arg):  
  if not isinstance(arg, Var): yield True
  
@builtin.macro('free')
def free(evaluator, arg):
  if isinstance(deref(arg, evaluator.env), Var): yield True

@builtin.macro('bound')
def bound(evaluator, arg):
  if not isinstance(arg.deref(evaluator.env), Var): yield True

def isinteger(evaluator, arg):
  if isinstance(deref(arg, evaluator.env), int): yield True

@builtin.macro('float')
def isfloat(evaluator, arg):
  if isinstance(deref(arg, evaluator.env), float): yield True

@builtin.macro('isnumber')
def isnumber(evaluator, arg):
  if isinstance(deref(arg, evaluator.env), int) \
     or isinstance(deref(arg, evaluator.env), float): 
    yield True

def iscons(evaluator, arg):
  if isinstance(deref(arg, evaluator.env), Cons): yield True

def is_ground(term):
  if isinstance(term, Var): return False
  if isinstance(term, Cons): 
    if not is_ground(term.head): return False
    if not is_ground(term.tail): return False
  return True
  
@builtin.macro('ground')
def ground(evaluator, term):
  if is_ground(getvalue(term, evaluator.env)): yield True
