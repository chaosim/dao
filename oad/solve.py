# -*- coding: utf-8 -*-

from oad.env import GlobalEnvironment

class CutException: pass

def done(value, solver): yield done, value

def value_cont(exp, cont): return lambda value, solver: cont(exp, solver)
  
def eval(exp): return Solver().eval(exp)

def cut(cont_gen): 
  try: return cont_gen.cut
  except: return False
  
class Solver:
  def __init__(self, env=None, stop=done):
    if env is None: env = GlobalEnvironment()
    self.env = env
    self.stop = stop

  def eval(self, exp):
    for x in self.solve(exp): return x
    
  def solve(self, exp, stop=done):
    solver = Solver(self.env, stop)
    cont = solver.cont(exp, stop)
    for _, result in solver.run_cont(cont, None):
      yield result
      
  def solve_exps(self, exps, stop=done):
    if len(exps)==0: yield True
    elif len(exps)==1: 
      for x in self.solve(exps[0], stop):
        yield x
    else:
      for _ in self.solve(exps[0], self.exps_cont(exps[1:], stop)): 
        for x in self.solve_exps(exps[1:], stop):
          yield x
  def run_cont(self, cont, value):
    stop = self.stop 
    root = cont_gen = cont(value, self)
    cut_gen = {}
    cut_gen[cont_gen] = cut(cont)
    parent = {}
    while 1:
      try:
        try: c, v  = cont_gen.next()
        except CutException:
          while not cut_gen[cont_gen] and cont_gen is not root:
            cont_gen.close()
            cont_gen = parent[cont_gen]
          if cont_gen is root:  
            cont_gen.close()
            return
          cont_gen.close()
          cont_gen = parent[cont_gen]
        if c is stop: yield c, v
        else:
          cg = c(v, self)
          cut_gen[cg] =cut(c)
          parent[cg] = cont_gen
          cont_gen = cg
      except StopIteration:
        if cont_gen is root: 
          return
        else: 
##          del parent[cont_gen]
          cont_gen = parent[cont_gen]
        
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
