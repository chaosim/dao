from oad.term import conslist
from oad.builtins.parser import settext
from oad.builtins.terminal import eos
from oad.builtins.control import and_
from oad.solve import Solver

class Grammar:
  def __init__(self, start, rules, result):
    self.rules, self.start, self.result = rules, start, result

def parse(grammar, text):
  global envvarCache
  envvarCache = {}
  solver = Solver()
  exp = conslist('letrec', grammar.rules, (settext, text), 
                 (and_, grammar.start, [eos]), grammar.result)
  return solver.eval(exp)

def eval(grammar, text): #don't need any more!!!
  solver = Solver()
  exp = conslist('letrec', grammar.rules, (settext, text), 
                 (and_, grammar.start, [eos]), grammar.result)
  parsedExp = solver.eval(exp)
  return solver.eval(parsedExp) #, prelude=False
  