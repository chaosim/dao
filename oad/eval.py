# -*- coding: utf-8 -*-

from oad.env import GlobalEnvironment

class CutException: pass

def eval(exp): 
  return Evaluator().eval(exp)

class Evaluator:
  def __init__(self, builtins=None, prelude=None, trail=None):
    self.env = GlobalEnvironment()

  def eval(self, exp):
    for x in self.solve(exp): 
      return x
  def solve(self, exp):
    try: return exp.solve(self) 
    except AttributeError: return (exp,)
      
def solve_exps(evaluator, exps):
  if exps==(): yield True
  if len(exps)==1:
    for x in evaluator.solve(exps[0]):
      yield x
  else:
      for x in evaluator.solve(exps[0]):
        for y in solve_exps(evaluator, exps[1:]):
          yield y
