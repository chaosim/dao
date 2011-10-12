# -*- coding: utf-8 -*-

# env: environment
# cont: continution 
# stream: stream such as text used by the matchers which do parsing

from oad.env import GlobalEnvironment

class CutException: pass

class DaoUncaughtThrow(Exception):
  def __init__(self, tag): self.tag = tag

class  DaoSyntaxError(Exception):
  pass

def mycont(cont):
  def mycont_tagger(fun):
    fun.cont = cont
    return fun
  return mycont_tagger
 
def tag_unwind(fun):
  def unwind_tagger(tagged_fun):
    tagged_fun.unwind = fun
    return tagged_fun
  return unwind_tagger
 
def done_unwind(cont, tag, stop_cont, solver, next_cont=None):
  if cont is stop_cont: 
    return cont if next_cont is None else next_cont
  raise DaoUncaughtThrow(tag)

@tag_unwind(done_unwind)
@mycont(None)
def done(value, solver): yield done, value

def value_cont(exp, cont):
  @mycont(cont)
  def value_cont(value, solver): 
    return cont(exp, solver)
  return value_cont
  
def cut(cont_gen): 
  try: return cont_gen.cut
  except: return False

class Parser: 
  def parse(self, exp):
    try: parse_method = exp.___parse___
    except: 
      if isinstance(exp, list):
        return [self.parse(e) for e in exp]
      elif isinstance(exp, tuple):
        return tuple(self.parse(e) for e in exp)
      else: return exp
    try: return parse_method(self)
    except TypeError: return exp

def parse(exp): 
  return Parser().parse(exp)

class LoopExitNextTagger:
  ''' use tagger to preprocess before solve expression'''
  surfix = '$'
  def __init__(self): 
    self.new_label_id = 1
    self.labels = {}
  def make_label(self, label):
    if label is None: 
      label = '$'+str(self.new_label_id)
      self.new_label_id += 1
    return label
  def push_label(self, control_struct_type, label):
    self.labels.setdefault(control_struct_type, []).append(label)
    self.labels.setdefault(None,[]).append(label)
  def pop_label(self, control_struct_type):
    self.labels[control_struct_type].pop()
    self.labels[None].pop()

  def tag_loop_label(self, exp):
    try: exp_tag_loop_label = exp.tag_loop_label
    except: 
      if isinstance(exp, list):
        return [self.tag_loop_label(e) for e in exp]
      elif isinstance(exp, tuple):
        return tuple(self.tag_loop_label(e) for e in exp)
      else: return exp
    try: return exp_tag_loop_label(self)
    except TypeError: return exp

def tag_loop_label(exp): 
  return LoopExitNextTagger().tag_loop_label(exp)

def eval(exp):
  return Solver().eval(exp)

def eval_list(exps):
  from special import begin
  return Solver().eval(begin(*exps))

def solve(exp): 
  return Solver.solve(exp)

class Solver:
  # exp: expression 
  # exps: expression list
  
  def __init__(self, env=None, stream=None, stop=None):
    if env is None: env = GlobalEnvironment()
    self.env = env
    self.stop = stop
    self.stream = stream
  
  def eval(self, exp):
    for x in self.solve(exp): return x
    
  def solve(self, exp, stop=done):
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
    self1 = self
    self = Solver(self.env, self.stream, stop)
    stop = self.stop 
    root = cont_gen = cont(value, self)
    cut_gen = {}
    cut_gen[cont_gen] = cut(cont)
    parent = {}
    while 1:
      try: 
        c, v  = cont_gen.next()
        if c is stop: 
          env, stream = self1.env, self1.stream
          self1.env, self1.stream = self.env, self.stream
          yield c, v
          self1.env, self1.stream = env, stream
        else:
          cg = c(v, self)
          cut_gen[cg] = cut(c)
          parent[cg] = cont_gen
          cont_gen = cg
      except StopIteration:
        if cont_gen is root: return
        else: 
          cg = cont_gen
          cont_gen = parent[cont_gen]
          del parent[cg]
      except CutException: # go after StopIteration!
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
      except GeneratorExit: raise
  def cont(self, exp, cont):    
    try: to_cont = exp.cont
    except: return value_cont(exp, cont)
    try: return to_cont(cont, self)
    except TypeError: return value_cont(exp, cont)

  def exps_cont(self, exps, cont):
      if len(exps)==0: return value_cont(True, cont)
      elif len(exps)==1: return self.cont(exps[0], cont)
      else:
        @mycont(cont)
        def exps_cont(value, solver):
          yield self.exps_cont(exps[1:], cont), value
        return self.cont(exps[0], exps_cont)
