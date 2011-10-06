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
  
@builtin.function('conslist')
def conslist(*arguments): return conslist(arguments)

@builtin.function('cons?')
def is_cons(x): return isinstance(x, Cons)

@builtin.function('pylist')
def pylist(*arguments): return list(arguments)

@builtin.function('is_pylist?')
def is_pylist(x): return isinstance(x, list)

@builtin.function('is_pytuple?')
def pytuple(*arguments): 
  return tuple(arguments)

@builtin.function('is_pytuple?')
def is_pytuple(x):  return isinstance(x, tuple)

@builtin.function('make_apply?')
def make_apply(fun, *args):  
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

@builtin.function('second')
def second(sequence): 
  return sequence[1]


@builtin.function('iter_next')
def iter_next(iterator): 
  return iterator.next()

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
