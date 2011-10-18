# -*- coding: utf-8 -*-

# env: environment
# cont: continution 
# stream: stream such as text used by the matchers which do parsing

from oad.env import GlobalEnvironment

class CutException(Exception): pass
class DaoStopIteration(Exception): pass

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
 
def done_unwind(cont, value, tag, stop_cont_cont, solver, next_cont=None):
  if cont is stop_cont_cont: 
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
    try: exp_parse = exp.___parse___
    except: 
      if isinstance(exp, list):
        return [self.parse(e) for e in exp]
      elif isinstance(exp, tuple):
        return tuple(self.parse(e) for e in exp)
      else: return exp
    try: return exp_parse(self)
    except TypeError: return exp

def preparse(exp): 
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

def solve(exp): 
  return Solver.solve(exp)

interactive, noninteractive = 1, 0
_run_mode = interactive
_interactive_solver = None
_interactive_parser = None
_interactive_tagger = None

def run_mode(): return _run_mode
def interactive_solver(): return _interactive_solver
def interactive_parser(): return _interactive_parser
def interactive_tagger(): return _interactive_tagger

def set_run_mode(mode=interactive, solver=None, tagger=None, parser=None):
  global _run_mode, _interactive_solver, _interactive_parser, _interactive_tagger
  if mode==interactive:
    _run_mode = interactive
    if solver is None:
      _interactive_solver = Solver() if _interactive_solver is None\
                           else _interactive_solver
    else: _interactive_solver = solver
    if tagger is None:
      _interactive_tagger = LoopExitNextTagger() if _interactive_tagger is None\
                           else _interactive_tagger
    else: _interactive_tagger = tagger
    if parser is None:
      _interactive_parser = Parser() if _interactive_parser is None\
                           else _interactive_parser
    else: _interactive_parser = _parser
  else: 
    _run_mode = noninteractive

class Solver:
  # exp: expression 
  # exps: expression list
  
  def __init__(self, env=None, stream=None, stop_cont=None):
    if env is None: env = GlobalEnvironment()
    self.env = env
    self.stop_cont = stop_cont
    self.stream = stream
    self.solved = False
  
  def eval(self, exp):
    if isinstance(exp, list) or isinstance(exp, tuple):
      from oad.special import begin
      exp = begin(*exp)
    for x in self.solve(exp): return x
    
  def solve(self, exp, stop_cont=done):
    for _, result in self.exp_run_cont(exp, stop_cont):
      yield result
      
  def solve_exps(self, exps, stop_cont=done):
    if len(exps)==0: yield True
    elif len(exps)==1: 
      for c, x in self.exp_run_cont(exps[0], stop_cont):
        yield x
    else:
      left_exps_cont = self.exps_cont(exps[1:], stop_cont)
      for c, x in self.exp_run_cont(exps[0], left_exps_cont): 
          yield x
          
  def exp_run_cont(self, exp, stop_cont, value=None):
    cont = self.cont(exp, stop_cont)
    return self.run_cont(cont, stop_cont, value)
  
  def exps_run_cont(self, exps, stop_cont, value=None):
    cont = self.exps_cont(exps, stop_cont)
    return self.run_cont(cont, stop_cont, value)
  
  def run_cont(self, cont, stop_cont, value=None):
    self1 = self
    self = Solver(self.env, self.stream, stop_cont)
    stop_cont = self.stop_cont 
    root = cont_gen = cont(value, self)
    cut_gen = {}
    cut_gen[cont_gen] = cut(cont)
    parent = {}
    while 1:
      try: 
        c, v  = cont_gen.next()
        if self.solved or c is stop_cont: 
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
##      except: 
##        self1.env, self1.stream = env, stream
##        raise
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
          yield solver.exps_cont(exps[1:], cont), value
        return self.cont(exps[0], exps_cont)
