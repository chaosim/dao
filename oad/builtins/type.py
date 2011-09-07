from oad.term import Var#, SUCCESS, UList
from oad.helper import atomic
from oad import builtin

# type verifications

@builtin.macro('var')
def isvar(evaluator, arg):
  if not isinstance(arg, Var): raise UnifyFail()
  evaluator.value = SUCCESS

@builtin.macro('nonvar')
def nonvar(evaluator, arg):  
  if isinstance(arg, Var): raise UnifyFail()
  evaluator.value = SUCCESS
  
@builtin.macro('free')
def free(evaluator, arg):
  if not isinstance(arg.deref(evaluator.trail), Var): raise UnifyFail()
  evaluator.value = SUCCESS

@builtin.macro('bound')
def bound(evaluator, arg):
  if isinstance(arg.deref(trail), Var): raise UnifyFail()
  evaluator.value = SUCCESS


def isinteger(evaluator, arg):
  if not isinstance(arg.deref(evaluator.trail), Integer): raise UnifyFail()
  evaluator.value = SUCCESS

@builtin.macro('float')

def isfloat(evaluator, arg):
  if not isinstance(arg.deref(evaluator.trail), Float): raise UnifyFail()
  evaluator.value = SUCCESS

@builtin.macro('isnumber')

def isnumber(evaluator, arg):
  if not isinstance(arg.deref(evaluator.trail), Number): raise UnifyFail()
  evaluator.value = SUCCESS

@builtin.macro('atom')

def isatom(evaluator, arg):
  arg = arg.deref(evaluator.trail)
  if isinstance(arg, Var) or not isinstance(arg, Atom):
    raise UnifyFail()
  evaluator.value = SUCCESS


def isatomic(evaluator, arg):
  if not is_atomic(arg.deref(evaluator.trail)): raise UnifyFail() 
  evaluator.value = SUCCESS
   

def iscompound(evaluator, arg):
  if not isinstance(arg.deref(evaluator.trail), UList): raise UnifyFail()
  evaluator.value = SUCCESS

@builtin.macro('iscallable')
def iscallable(evaluator, arg):
  if not is_callable(arg.deref(evaluator.trail)): raise UnifyFail()
  evaluator.value = SUCCESS

def impl_ground(evaluator, term):
  if isinstance(term, Var): raise UnifyFail()
  if isinstance(term, UList):
    for arg in var.elements: impl_ground(evaluator, arg)

@builtin.macro('ground')
def ground(evaluator, term):
  term = term.getvalue(evaluator.trail)
  impl_ground(evaluator, term)
  evaluator.value = SUCCESS
