##from oad.term import Integer, Float, Bool
from oad import builtin

@builtin.function('+')
def add(x, y): 
  return x+y

@builtin.function('-')
def sub(x, y): return x-y
@builtin.function('*')
def mul(x, y): return x*y

@builtin.function('/')
def div(x, y): return x/y

@builtin.function('==')
def eq(x,y): return x==y


