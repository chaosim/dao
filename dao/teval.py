from dao.solve import make_solver

from dao.t.grammar import grammar

from dao.builtins.parser import parse_text

from dao.t.builtins import global_env

def teval(text):
  solver = make_solver(global_env, global_env.extend(), None, None)
  exp = parse_text(grammar, text)
  return solver.eval(exp)

teval('1')