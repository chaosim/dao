from dao.solve import Solver, set_run_mode, noninteractive
set_run_mode(noninteractive)

from dao import t

def teval(text):
  solver = Solver(t.global_env, t.global_env.extend(), None, None)
  code = t.grammar.make_expression(text)
  return solver.eval(code)
