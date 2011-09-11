# -*- coding: utf-8 -*-

from oad.env import GlobalEnvironment

class CutException: pass

def done(value, solver): yield done, value

def value_cont(exp, cont):
  return  lambda value, solver: [(cont, exp)]
  
def eval(exp): return Solver().eval(exp)

class Solver:
  def __init__(self):
    self.env = GlobalEnvironment()

  def eval(self, exp):
    for x in self.solve(exp): return x
    
  def solve(self, exp, until=done):
    cont = self.cont(exp, until)
    for _, result in self.run_cont(cont, None, until):
      yield result
      
  def solve_exps(self, exps, until=done):
    if len(exps)==0: yield True
    elif len(exps)==1: 
      for x in self.solve(exps[0], until):
        yield x
    else:
      for _ in self.solve(exps[0], self.exps_cont(exps[1:], until)): 
        for x in self.solve_exps(exps[1:], until):
          yield x
  def run_cont(self, cont, value, until):
    for c, v in cont(value, self):
      if c is until: yield c, v
      for _, result in self.run_cont(c, v, until):
        yield _, result
        
  def cont(self, exp, cont):    
    try: return exp.cont(cont, self)
    except: return value_cont(exp, cont)
    
  def exps_cont(self, exps, cont):
      if len(exps)==0: return value_cont(True, cont)
      elif len(exps)==1: return self.cont(exps[0], cont)
      else:
        def mycont(value, solver):
          yield self.exps_cont(exps[1:], cont), value
        return self.cont(exps[0], mycont)
