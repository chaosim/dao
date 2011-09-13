from oad import helper, error, builtin 
from oad.term import Var, ClosureVar, deref

# arithmetic

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

def arithmeticCmpPredicate(function, name):
  @builtin.macro(name)
  def pred(solver, cont, var0, var1):
    if function(deref(var0, solver.env), 
                deref(var1, solver.env)):
      yield cont, True
  return pred

eq = arithmeticCmpPredicate(lambda x,y: x==y, '===')
ne = arithmeticCmpPredicate(lambda x,y: x!=y, '!=')
lt = arithmeticCmpPredicate(lambda x,y: x<y, '<')
le = arithmeticCmpPredicate(lambda x,y: x<=y, '<=')
gt = arithmeticCmpPredicate(lambda x,y: x>y, '>')
ge = arithmeticCmpPredicate(lambda x,y: x>=y, '>=')
