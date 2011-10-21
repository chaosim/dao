from dao.term import conslist
from dao.builtins.parser import settext
from dao.builtins.terminal import eos
from dao.builtins.control import and_
from dao.solve import Solver

class Grammar:
  def __init__(self, start, rules, result):
    self.rules, self.start, self.result = rules, start, result

def parse(grammar, text):
  global envvarCache
  envvarCache = {}
  solver = Solver()
  exp = conslist('letr', grammar.rules, (settext, text), 
                 (and_, grammar.start, [eos]), grammar.result)
  return solver.eval(exp)

def eval(grammar, text): #don't need any more!!!
  solver = Solver()
  exp = conslist('letr', grammar.rules, (settext, text), 
                 (and_, grammar.start, [eos]), grammar.result)
  parsedExp = solver.eval(exp)
  return solver.eval(parsedExp) #, prelude=False
  