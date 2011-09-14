# -*- coding: utf-8 -*-

from oad.env import GlobalEnvironment

class CutException: pass

def mycont(cont):
  def make_mycont(fun):
    fun.cont = cont
    return fun
  return make_mycont
 
@mycont(None)
def done(value, solver): yield done, value

class DaoUncaughtThrow(Exception):
  def __init__(self, tag): self.tag = tag
  
def done_unwind(cont, tag, stop_cont, solver):
  if cont is stop_cont: return cont
  raise DaoUncaughtThrow(tag)
done.unwind = done_unwind

def value_cont(exp, cont):
 @mycont(cont)
 def value_cont(value, solver): 
   return cont(exp, solver)
 return value_cont
  
def cut(cont_gen): 
  try: return cont_gen.cut
  except: return False

def to_sexpression(exp):
  try: return exp.to_sexpression()
  except AttributeError: 
    if isinstance(exp, list) or isinstance(exp, tuple):
      return tuple(to_sexpression(e) for e in exp)
    else: return exp
def clean_binding(exp):
  try: return exp.clean_binding()
  except AttributeError:
    if isinstance(exp, list) or isinstance(exp, tuple):
      return tuple(clean_binding(e) for e in exp)
    else: return exp
    
def eval(exp):
  sexp = to_sexpression(exp)
  return Solver().eval(sexp)

class Solver:
  # exp, exps: sexpression and sexpression list
  def __init__(self, env=None, stop=done):
    if env is None: env = GlobalEnvironment()
    self.env = env
    self.stop = stop
    self.stream = None

  def eval(self, exp):
    for x in self.solve(exp): return x
    
  def solve(self, exp, stop=done):
##    clean_binding(exp)
    cont = self.cont(exp, stop)
    for _, result in self.run_cont(cont, stop):
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
  def run_cont(self, cont, stop, value=None):
    self = Solver(self.env, stop)
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
            del cut_gen[cont_gen]
            cg = cont_gen
            cont_gen = parent[cont_gen]
            del parent[cg]
          if cont_gen is root:  
            cont_gen.close()
            return
          cont_gen.close()
          cg = cont_gen
          cont_gen = parent[cont_gen]
          del parent[cg]
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
          cg = cont_gen
          cont_gen = parent[cont_gen]
          del parent[cg]
        
  def cont(self, exp, cont):    
    try: return exp.cont(cont, self)
    except: 
      if isinstance(exp, list) or isinstance(exp, tuple):
        return self.list_cont(exp, cont)
      else: return value_cont(exp, cont)
  def list_cont(self, exp, cont):
    if len(exp)==0: return value_cont(exp)
    try: 
      form = exp[0].make_special_form(*exp[1:])
      return self.cont(form, cont)
    except: pass
    @mycont(cont)
    def evaluate_list_tail_cont(operator, solver): 
      return operator.evaluate_cont(exp[1:], cont, solver)
    return self.cont(exp[0], evaluate_list_tail_cont)
  def exps_cont(self, exps, cont):
      if len(exps)==0: return value_cont(True, cont)
      elif len(exps)==1: return self.cont(exps[0], cont)
      else:
        @mycont(cont)
        def exps_cont(value, solver):
          yield self.exps_cont(exps[1:], cont), value
        return self.cont(exps[0], exps_cont)
