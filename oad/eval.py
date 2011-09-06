# -*- coding: utf-8 -*-

from oad.env import GlobalEnvironment
from oad.trail import Trail

class CutException: pass

def eval(exp): 
  return Evaluator().eval(exp)

class Evaluator:
  def __init__(self, builtins=None, prelude=None, trail=None):
    from oad.term import subst, env
    subst.bindings = {}
    env.evaluator = self
    self.env = GlobalEnvironment()
    if trail is None: trail = Trail()
    self.trail = trail
##    self.stream = None # for parsing
##    if builtins is None: from oad.builtin import builtins
##    self.builtinlist = [(var(b.name), b) for b in builtins]
##    self.prelude = prelude

  def eval(self, exp):
    try: 
      for x in self.solve(exp): return x
    except AttributeError: return x
  def solve(self, exp):
    try: 
      for x in exp.solve(self): 
        yield x
    except AttributeError: 
      yield exp
      
def solve_exps(evaluator, exps):
  if exps==(): yield NIL
  for e in exps[:-1]: 
    for x in evaluator.solve(e): pass
  for x in evaluator.solve(exps[-1]):
    yield x
      