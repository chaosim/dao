##from oad.term import Integer, Float, Bool
from oad import builtin
from oad.term import Var, ClosureVar, deref, getvalue

@builtin.function('+')
def add(x, y): 
  return x+y

@builtin.function('-')
def sub(x, y): 
  return x-y
@builtin.function('*')
def mul(x, y): return x*y

@builtin.function('/')
def div(x, y): return x/y

def compare_function(function, name):
  @builtin.function(name)
  def func(x, y): 
    return function(x, y)
  return func

eq = compare_function(lambda x,y: x==y, '==')
ne = compare_function(lambda x,y: x!=y, '!=')
lt = compare_function(lambda x,y: x<y, '<')
le = compare_function(lambda x,y: x<=y, '<=')
gt = compare_function(lambda x,y: x>y, '>')
ge = compare_function(lambda x,y: x>=y, '>=')

@builtin.function('not')
def not_(value):
  return not value

@builtin.function2()
def between(solver, cont, *exps):
  lower, upper, mid = exps
  lower = deref(lower, solver.env)
  if isinstance(lower, Var): error.throw_instantiation_error()
  upper = deref(upper, solver.env)
  if isinstance(upper, Var): error.throw_instantiation_error()
  mid = deref(mid, solver.env)
  if not isinstance(mid, Var):
    if lower<=mid<=upper: yield cont, True
    else: return
  for x in range(lower, upper+1):
    for y in mid.unify(x, solver.env): yield cont, True

@builtin.function2('====')
def equal(solver, cont, left, right):
  if deref(left, solver.env)==deref(right, solver.env): 
    yield cont, True

def arithmeticCmpPredicate(function, name):
  @builtin.macro(name)
  def pred(solver, cont, var0, var1):
    if function(getvalue(var0, solver.env), 
                getvalue(var1, solver.env)):
      yield cont, True
  return pred

eq_p = arithmeticCmpPredicate(lambda x,y: x==y, '===')
ne_p = arithmeticCmpPredicate(lambda x,y: x!=y, '!=')
lt_p = arithmeticCmpPredicate(lambda x,y: x<y, '<')
le_p = arithmeticCmpPredicate(lambda x,y: x<=y, '<=')
gt_p = arithmeticCmpPredicate(lambda x,y: x>y, '>')
ge_p = arithmeticCmpPredicate(lambda x,y: x>=y, '>=')
