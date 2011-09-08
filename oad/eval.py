# -*- coding: utf-8 -*-

from oad.env import GlobalEnvironment

class CutException: pass

def eval(exp): 
  return Evaluator().eval(exp)

class Evaluator:
  def __init__(self, builtins=None, prelude=None, trail=None):
    self.env = GlobalEnvironment()
##    self.stream = None # for parsing
##    if builtins is None: from oad.builtin import builtins
##    self.builtinlist = [(var(b.name), b) for b in builtins]
##    self.prelude = prelude

  def eval(self, exp):
    try: 
      for x in self.solve(exp): 
        return x
    except AttributeError: return x
  def solve(self, exp):
    try: 
      for x in exp.solve(self): 
        yield x
    except AttributeError: 
      yield exp
      
def solve_exps(evaluator, exps):
  if exps==(): yield NIL
  if len(exps)==1:
    for x in evaluator.solve(exps[0]):
      yield x
  else:
      for x in evaluator.solve(exps[0]):
        for y in solve_exps(evaluator, exps[1:]):
          yield y
      