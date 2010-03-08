from oad import builtin
from oad.term import Var, ClosureVar, deref, getvalue
import operator

@builtin.function('not')
def not_(value):
  return not value

@builtin.function('<')
def lt(x, y): return operator.lt(x, y)  
@builtin.function('<=')
def le(x, y): return operator.le(x, y)  
@builtin.function('==')
def eq(x, y): return operator.eq(x, y)  
@builtin.function('!=')
def ne(x, y): return operator.ne(x, y)  
@builtin.function('>')
def gt(x, y): return operator.gt(x, y)  
@builtin.function('>=')
def ge(x, y): return operator.ge(x, y)  
@builtin.function('getattr')
def getattr(x, y): return operator.getattr(x, y)  
@builtin.function('getitem')
def getitem(x, y): return operator.getitem(x, y)
@builtin.function('+')
def add(x, y): return operator.add(x, y)  
@builtin.function('-')
def sub(x, y): return operator.sub(x, y)  
@builtin.function('*')
def mul(x, y): return operator.mul(x, y)  
@builtin.function('/')
def floordiv(x, y): return operator.floordiv  
@builtin.function('/')
def div(x, y): return operator.div(x, y)  
@builtin.function('//')
def truediv(x, y): return operator.truediv(x, y)  
@builtin.function('%')
def mod(x, y): return operator.mod(x, y)  
@builtin.function('**')
def pow(x, y): return operator.pow(x, y)  
@builtin.function('<<')
def lshift(x, y): return operator.lshift(x, y)
@builtin.function('>>')
def rshift(x, y): return operator.rshift(x, y)  
@builtin.function('&')
def and_(x, y): return operator.and_(x, y)  
@builtin.function('^')
def xor(x, y): return operator.xor(x, y)  
@builtin.function('|')
def or_(x, y): return operator.or_(x, y)

@builtin.function('iter')
def iter(x): return operator.iter(x) 
@builtin.function('-')
def neg(x): return operator.neg(x)  
@builtin.function('+')
def pos(x): return operator.pos(x)  
@builtin.function('abs')
def abs(x): return operator.abs(x)  
@builtin.function('~')
def invert(x): return operator.invert(x)

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

eq_p = arith_predicate(operator.eq, '==?')
ne_p = arith_predicate(operator.ne, '!=?')
lt_p = arith_predicate(operator.lt, '<?')
le_p = arith_predicate(operator.le, '<="')
gt_p = arith_predicate(operator.gt, '>?')
ge_p = arith_predicate(operator.ge, '>=?')
