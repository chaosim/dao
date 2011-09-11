from oad.term import Var, Cons, getvalue
from oad import builtin

# type verifications

@builtin.macro('var')
def isvar(solver, arg):
  if isinstance(arg, Var): yield True

@builtin.macro('nonvar')
def nonvar(solver, arg):  
  if not isinstance(arg, Var): yield True
  
@builtin.macro('free')
def free(solver, arg):
  if isinstance(deref(arg, solver.env), Var): yield True

@builtin.macro('bound')
def bound(solver, arg):
  if not isinstance(arg.deref(solver.env), Var): yield True

def isinteger(solver, arg):
  if isinstance(deref(arg, solver.env), int): yield True

@builtin.macro('float')
def isfloat(solver, arg):
  if isinstance(deref(arg, solver.env), float): yield True

@builtin.macro('isnumber')
def isnumber(solver, arg):
  if isinstance(deref(arg, solver.env), int) \
     or isinstance(deref(arg, solver.env), float): 
    yield True

def iscons(solver, arg):
  if isinstance(deref(arg, solver.env), Cons): yield True

def is_ground(term):
  if isinstance(term, Var): return False
  if isinstance(term, Cons): 
    if not is_ground(term.head): return False
    if not is_ground(term.tail): return False
  return True
  
@builtin.macro('ground')
def ground(solver, term):
  if is_ground(getvalue(term, solver.env)): yield True
