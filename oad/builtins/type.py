from oad.term import Var, Cons, getvalue
from oad import builtin

# type verifications

@builtin.macro('var')
def isvar(solver, cont, arg):
  if isinstance(arg, Var): yield cont, True

@builtin.macro('nonvar')
def nonvar(solver, cont, arg):  
  if not isinstance(arg, Var): yield cont, True
  
@builtin.macro('free')
def free(solver, cont, arg):
  if isinstance(deref(arg, solver.env), Var): yield cont, True

@builtin.macro('bound')
def bound(solver, cont, arg):
  if not isinstance(arg.deref(solver.env), Var): yield cont, True

def isinteger(solver, cont, arg):
  if isinstance(deref(arg, solver.env), int): yield cont, True

@builtin.macro('float')
def isfloat(solver, cont, arg):
  if isinstance(deref(arg, solver.env), float): yield cont, True

@builtin.macro('isnumber')
def isnumber(solver, cont, arg):
  if isinstance(deref(arg, solver.env), int) \
     or isinstance(deref(arg, solver.env), float): 
    yield cont, True

def iscons(solver, cont, arg):
  if isinstance(deref(arg, solver.env), Cons): yield cont, True

def is_ground(term):
  if isinstance(term, Var): return False
  if isinstance(term, Cons): 
    if not is_ground(term.head): return False
    if not is_ground(term.tail): return False
  return True
  
@builtin.macro('ground')
def ground(solver, cont, term):
  if is_ground(getvalue(term, solver.env)): yield cont, True
