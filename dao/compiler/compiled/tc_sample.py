from dao.compiler.compiled.term import Var, unify
from dao.compiler.compiled.builtins import println
from dao.sompiler.compiled.solver import solve

'''
unify(x,1)
println(x)
'''

def sample1():
  x = Var()
  for _ in x.unify(1):
    for _ in println(x):
      yield _

# most optimized version
def sample1():
  print 1
  yield


'''
unify(x,1)
unify(y,2)
unify(z,3)
println(x)
println(y)
println(z)
'''

# if loop level is too big, use apply_generators
def sample2():
  x, y, z = Var(), Var(), Var()
  generator_list = tuple(x.unify(1), x.unify(2), x.unify(3), 
                         println(x), println(y), println(z))
  return apply_generators(generator_list)

# most optimized version
def sample2():
  print 1
  print 2
  print 3
  yield

'''
lambda_(x, unify(x,1), println(x))
'''

def sample3():
  def lambda_(x):
    for _ in unify(x, 1):
      for _ in println(x):
        yield _
  yield fun
  