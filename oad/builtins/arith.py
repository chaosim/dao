from oad import builtin
from oad.term import Var, ClosureVar, deref, getvalue
import operator

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

@builtin.function()
def lt(x, y): return operator.lt(x, y)  
@builtin.function()
def le(x, y): return operator.le(x, y)  
@builtin.function()
def eq(x, y): return operator.eq(x, y)  
@builtin.function()
def ne(x, y): return operator.ne(x, y)  
@builtin.function()
def gt(x, y): return operator.gt(x, y)  
@builtin.function()
def ge(x, y): return operator.ge(x, y)  
@builtin.function()
def getattr(x, y): return operator.getattr(x, y)  
@builtin.function()
def getitem(x, y): return operator.getitem(x, y)
@builtin.function()
def add(x, y): return operator.add(x, y)  
@builtin.function()
def sub(x, y): return operator.sub(x, y)  
@builtin.function()
def mul(x, y): return operator.mul(x, y)  
@builtin.function()
def floordiv(x, y): return operator.floordiv  
@builtin.function()
def div(x, y): return operator.div(x, y)  
@builtin.function()
def truediv(x, y): return operator.truediv(x, y)  
@builtin.function()
def mod(x, y): return operator.mod(x, y)  
@builtin.function()
def pow(x, y): return operator.pow(x, y)  
@builtin.function()
def lshift(x, y): return operator.lshift(x, y)
@builtin.function()
def rshift(x, y): return operator.rshift(x, y)  
@builtin.function()
def and_(x, y): return operator.and_(x, y)  
@builtin.function()
def xor(x, y): return operator.xor(x, y)  
@builtin.function()
def or_(x, y): return operator.or_(x, y)

@builtin.function()
def iter(x): return operator.iter(x) 
@builtin.function()
def neg(x): return operator.neg(x)  
@builtin.function()
def pos(x): return operator.pos(x)  
@builtin.function()
def abs(x): return operator.abs(x)  
@builtin.function()
def invert(x): return operator.invert(x)

@builtin.function()
def __lt__(x, y): return operator.__lt__(x, y)  
@builtin.function()
def __le__(x, y): return operator.__le__(x, y)  
@builtin.function()
def __eq__(x, y): return operator.__eq__(x, y)  
@builtin.function()
def __ne__(x, y): return operator.__ne__(x, y)  
@builtin.function()
def __gt__(x, y): return operator.__gt__(x, y)  
@builtin.function()
def __ge__(x, y): return operator.__ge__(x, y)  
@builtin.function()
def __getattr__(x, y): return operator.__getattr__(x, y)  
@builtin.function()
def __getitem__(x, y): return operator.__getitem__(x, y)
@builtin.function()
def __add__(x, y): return operator.__add__(x, y)  
@builtin.function()
def __sub__(x, y): return operator.__sub__(x, y)  
@builtin.function()
def __mul__(x, y): return operator.__mul__(x, y)  
@builtin.function()
def __floordiv__(x, y): return operator.__floordiv__  
@builtin.function()
def __div__(x, y): return operator.__div__(x, y)  
@builtin.function()
def __truediv__(x, y): return operator.__truediv__(x, y)  
@builtin.function()
def __mod__(x, y): return operator.__mod__(x, y)  
@builtin.function()
def __pow__(x, y): return operator.__pow__(x, y)  
@builtin.function()
def __lshift__(x, y): return operator.__lshift__(x, y)
@builtin.function()
def __rshift__(x, y): return operator.__rshift__(x, y)  
@builtin.function()
def __and__(x, y): return operator.__and__(x, y)  
@builtin.function()
def __xor__(x, y): return operator.__xor__(x, y)  
@builtin.function()
def __or__(x, y): return operator.__or__(x, y)

@builtin.function()
def __iter__(x): return operator.__iter__(x) 
@builtin.function()
def __neg__(x): return operator.__neg__(x)  
@builtin.function()
def __pos__(x): return operator.__pos__(x)  
@builtin.function()
def __abs__(x): return operator.__abs__(x)  
@builtin.function()
def __invert__(x): return operator.__invert__(x)

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

def arith_predicate(function, name):
  @builtin.macro(name)
  def pred(solver, cont, var0, var1):
    if function(getvalue(var0, solver.env), 
                getvalue(var1, solver.env)):
      yield cont, True
  return pred

eq_p = arith_predicate(operator.eq, '===')
ne_p = arith_predicate(operator.ne, '!=')
lt_p = arith_predicate(operator.lt, '<')
le_p = arith_predicate(operator.le, '<=')
gt_p = arith_predicate(operator.gt, '>')
ge_p = arith_predicate(operator.ge, '>=')
