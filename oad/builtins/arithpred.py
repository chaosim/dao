from oad import helper, error, builtin 
from oad.term import Var, ClosureVar, deref

# arithmetic

@builtin.function2()
def between(evaluator, *exps):
  lower, upper, mid = exps
  lower = deref(lower, evaluator.env)
  if isinstance(lower, Var): error.throw_instantiation_error()
  upper = deref(upper, evaluator.env)
  if isinstance(upper, Var): error.throw_instantiation_error()
  mid = deref(mid, evaluator.env)
  if not isinstance(mid, Var):
    if lower<=mid<=upper: yield True
    else: return
  for x in range(lower, upper+1):
    for y in mid.unify(x, evaluator.env): yield True

@builtin.function2('====')
def equal(evaluator, left, right):
  if deref(left, evaluator.env)==deref(right, evaluator.env): 
    yield True

@builtin.macro('is')
def is_(evaluator, var, func):
  func = deref(func, evaluator.env)
  for x in evaluator.solve(func):
    for _ in var.unify(x, evaluator.env): 
      yield True   

@builtin.macro()
def  define(evaluator, var, value):
  if isinstance(var, ClosureVar): var = var.var
  value = deref(value, evaluator.env)
  evaluator.env[var] = value
  yield value

def arithmeticCmpPredicate(function, name):
  @builtin.macro(name)
  def pred(evaluator, var0, var1):
    if function(deref(var0, evaluator.env), 
                     deref(var1, evaluator.env)):
      yield True
  return pred

eq = arithmeticCmpPredicate(lambda x,y: x==y, '===')
ne = arithmeticCmpPredicate(lambda x,y: x!=y, '!=')
lt = arithmeticCmpPredicate(lambda x,y: x<y, '<')
le = arithmeticCmpPredicate(lambda x,y: x<=y, '<=')
gt = arithmeticCmpPredicate(lambda x,y: x>y, '>')
ge = arithmeticCmpPredicate(lambda x,y: x>=y, '>=')
