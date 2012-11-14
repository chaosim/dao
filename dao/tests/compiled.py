from dao.interlang import LogicVar
from dao.solvebase import Solver

solver = Solver()

def compiled_dao_function(v1):
  cut_or_cont = solver.cut_or_cont
  solver.cut_or_cont = solver.fail_cont
  fc = solver.fail_cont
  solver.fail_cont = lambda v: (solver.fail_cont = fc, solver.fail_cont(True))
  yield (solver.cut_or_cont = cut_or_cont, True)