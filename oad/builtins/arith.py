##from oad.term import Integer, Float, Bool
from oad import builtin

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

@builtin.function('==')
def eq(x,y): return x==y

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

