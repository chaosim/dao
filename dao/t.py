from dao.env import GlobalEnvironment

from dao.solve import Solver

from dao.t.grammar import grammar


from dao.builtins.parser import parse_text

def teval(text):
  global_env = GlobalEnvironment({})
  solver = Solver(global_env)
  exp = parse_text(grammar, text)
  return solver.eval(exp)

teval('1')