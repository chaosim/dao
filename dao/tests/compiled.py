from dao.interlang import LogicVar
from dao.solvebase import Solver

solver = Solver()

def compiled_dao_function():
  def function(a1):
    a1[0] = 2
    return 2
  yield function([None])
compiled_dao_function