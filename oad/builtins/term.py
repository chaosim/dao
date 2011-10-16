from oad.term import Var, unify, deref, Cons, ClosureVar
import oad.term as term
from oad import builtin

# analysing and construction terms

@builtin.macro()
def getvalue(solver, cont, item):
  yield cont, term.getvalue(item, solver.env)
  
@builtin.macro()
def ground_value(solver, cont, item, default=None):
  v = term.getvalue(item, solver.env)
  if isinstance(v, Var): v = default #deref(default, solver.env)
  yield cont, v
  
@builtin.macro()
def setvalue(solver, cont, var, value):
  var = deref(var, solver.env)
  value = deref(value, solver.env)
  @mycont(cont)
  def setvalue_cont(value, solver):
    var = var.var
    if var in solver.env.bindings:
      v = solver.env.bindings[var]
      if isinstance(v, Var): var = v
    old = var.getvalue(solver.env)
    var.setvalue(value, solver.env)
    yield cont, True
    var.setvalue(old, solver.env)
  yield solver.cont(value, set_cont), True

@builtin.macro()
def copy_term(solver, cont, item, copy):
  for _ in term.unify(copy, term.copy(item, {}), solver.env):
    yield cont, True
 
# comparison and unification of terms

@builtin.macro('unify')
def unify(solver, cont, v0, v1):
  for _ in term.unify(v0, v1, solver.env): yield cont, True

@builtin.macro('unify_with_occurs_check')
def unify_with_occurs_check(solver, cont, v0, v1):
  for _ in term.unify(v0, v1, solver.env, occurs_check=True): yield cont, True

@builtin.macro('notunify')
def notunify(solver, cont, var0, var1):
  for _ in term.unify(var0, var1, solver.env): 
    return
  else: yield cont, True
  
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

@builtin.macro('iscons')
def iscons(solver, cont, arg):
  if isinstance(deref(arg, solver.env), Cons): yield cont, True

@builtin.function('cons?')
def is_cons(x): return isinstance(x, Cons)


def is_ground(term):
  if isinstance(term, Var): return False
  if isinstance(term, Cons): 
    if not is_ground(term.head): return False
    if not is_ground(term.tail): return False
  return True
  
@builtin.macro('ground')
def ground(solver, cont, term):
  if is_ground(getvalue(term, solver.env)): yield cont, True
  
@builtin.function('conslist')
def conslist(*arguments): return conslist(arguments)

@builtin.function('pylist')
def pylist(*arguments): return list(arguments)

@builtin.function('is_pylist?')
def is_pylist(x): return isinstance(x, list)

@builtin.function('is_pytuple?')
def pytuple(*arguments): 
  return tuple(arguments)

@builtin.function('is_pytuple?')
def is_pytuple(x):  return isinstance(x, tuple)

@builtin.function('pycall')
def pycall(fun, *args):  
  return fun(*args)

@builtin.function('py_apply')
def py_apply(fun, args):  
  return fun(*args)

@builtin.function('head_list')
def head_list(head, tail): 
  if isinstance(tail, list): return [head]+tail
  else: return (head,)+tuple(tail)

@builtin.function('list_tail')
def list_tail(head, tail):
  if isinstance(head, list): return head+[tail]
  else: return head+(tail,)

@builtin.function('index')
def index(sequence, index): 
  return sequence[index]

@builtin.function('first')
def first(sequence): 
  return sequence[0]

@builtin.function('left')
def left(sequence): 
  return sequence[1:]

@builtin.function('second')
def second(sequence): 
  return sequence[1]

from oad.solve import DaoStopIteration

@builtin.function('iter_next')
def iter_next(iterator): 
  try: return iterator.next()
  except StopIteration:
##    iterator.close()
    raise DaoStopIteration

@builtin.function('make_iter')
def make_iter(iterator): 
  try: 
    iterator.next
    return iterator
  except: return iter(iterator)
  
@builtin.function('to_list')
def to_list(item): 
  if isinstance(item, list) or isinstance(item, tuple): 
    return item
  return [item]

@builtin.function('items')
def items(dict):
  return dict.items()

@builtin.macro('is')
def is_(solver, cont, var, func):
  func = deref(func, solver.env)
  yield solver.cont(func, lambda value, solver:
    ((cont, True) for _ in var.unify(value, solver.env))), True

@builtin.macro()
def define(solver, cont, var, value):
  value = deref(value, solver.env)
  if isinstance(var, ClosureVar): var = var.var
  def define_cont(value, solver):
    solver.env[var] = value
    yield cont, value
  yield solver.cont(value, define_cont), True

