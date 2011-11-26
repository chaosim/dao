# -*- coding: utf-8 -*-

# exp: expression 
# exps: expression list
# env: environment
# cont: continution 
# parse_state: parse_state such as text used by the matchers which do parsing

from dao.env import GlobalEnvironment
from dao.base import is_subclass, tag_loop_label, preparse

class BaseCommand: pass

class CutException(Exception): pass
class DaoStopIteration(Exception): pass

class DaoUncaughtThrow(Exception):
  def __init__(self, tag): 
    self.tag = tag

class  DaoSyntaxError(Exception):
  pass

class DaoError(Exception):
  def __init__(self, message): 
    self.message = message
  def __repr__(self): 
    return self.message
  __str__ = __repr__

class NoSolutionFound:
  cont_order = 0 # just use it accidently
  def __init__(self, exp): 
    self.exp = exp
  def __repr__(self): return 'exit!'
  
class NoMoreSolution(Exception): pass

def tag_lookup(fun):
  def lookup_tagger(tagged_fun):
    tagged_fun.lookup = fun
    return tagged_fun
  return lookup_tagger

def done_lookup(cont, tag, stop_cont, solver): 
  raise DaoUncaughtThrow(tag)

from dao.env import unwind
def done_lookup(cont, tag, stop_cont, solver): 
  @mycont(cont)
  def loopkup_fail_cont(value, solver):
    raise  DaoUncaughtThrow(tag)
  return unwind(stop_cont, None, tag, loopkup_fail_cont, solver)

def tag_unwind(fun):
  def unwind_tagger(tagged_fun):
    tagged_fun.unwind = fun
    return tagged_fun
  return unwind_tagger
 
def done_unwind(cont, value, tag, stop_cont_cont, solver, next_cont=None):
  if cont is stop_cont_cont: 
    return cont if next_cont is None else next_cont
  raise DaoUncaughtThrow(tag)

def mycont(cont):
  def mycont_tagger(fun):
    fun.cont = cont
    try:
      fun.cont_order = cont.cont_order+1
    except:
      cont(1, None)
    return fun
  return mycont_tagger
 
@tag_lookup(done_lookup)
@tag_unwind(done_unwind)
@mycont(NoSolutionFound)
def done(value, solver): return value

@tag_lookup(done_lookup)
@tag_unwind(done_unwind)
@mycont(NoSolutionFound)
def fail_done(value, solver): 
  solver.failed = True
  return value

def value_cont(exp, cont):
  @mycont(cont)
  def value_cont(value, solver):
    solver.scont = cont
    return exp
  return value_cont

def cut(cont_gen): 
  try: return cont_gen.cut
  except: return False

def dao_repr(exp):
    try: exp_____repr____ = exp.____repr____
    except: 
      if isinstance(exp, list) or isinstance(exp, tuple):
        return ','.join([dao_repr(e) for e in exp])
      else: return repr(exp)
    try: return exp_____repr____()
    except TypeError: return repr(exp)

def make_solver():
  global_env = GlobalEnvironment({})
  env = global_env.extend({})
  return Solver(global_env, env, None, None)

def to_sexpression(exp):
  try: exp_to_sexpression = exp.to_sexpression
  except: 
    if isinstance(exp, tuple):
      return tuple(to_sexpression(x) for x in exp)
    else: return exp
  if isinstance(exp, type):
    return exp
  return exp_to_sexpression()

def eval(exp):
  sexp = to_sexpression(exp)
  return make_solver().eval(sexp)

class Solutions:
  def __init__(self, solutions):
    self.solutions = solutions
  def next(self):
    try:
      return self.solutioins.next()
    except StopIteration:
      return 'No more solutions!'
##      raise NoMoreSolution
    
def solve(exp): 
  sexp = to_sexpression(exp)
  return Solutions(Solver.solve(exp))

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
      _interactive_solver = make_solver() if _interactive_solver is None\
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
  
  def __init__(self, global_env, env, parse_state, stop_cont):
    self.global_env = global_env
    self.env = env
    self.scont = self.stop_cont = stop_cont
    self.fcont = self.fail_stop = fail_done
    self.parse_state = parse_state
    self.solved = False
    self.failed = False
    
    # used for chart parsing, from bottom to up parsing
    # left recursive is permmited
    self.sign_state2cont = {}
    self.sign_state2results = {}
    self.call_path = []
  
  def eval(self, exp):
    for x in self.solve(exp): return x
    raise NoSolutionFound(exp)
    
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
    self = Solver(self.global_env, self.env, self.parse_state, stop_cont)
    stop_cont = self.stop_cont
    self.scont = cont
    while 1:
      value  = self.scont(value, self)
      if self.failed: return
      if self.solved or self.scont is stop_cont: 
        env, parse_state = self1.env, self1.parse_state
        self1.env, self1.parse_state = self.env, self.parse_state
        yield self.scont, value
        self.scont = self.fcont

  def cont(self, exp, cont): 
    if isinstance(exp, tuple): 
      if is_subclass(exp[0], BaseCommand): # SpecialForm
        form = exp[0](*exp[1:])
        return form.cont(cont, self)
      else:
        @mycont(cont)
        def evaluate_cont(op, solver): 
          solver.scont = cont
          return op.evaluate_cont(solver, exp[1:])
        return self.cont(exp[0], evaluate_cont)
    else:
      if is_subclass(exp, object):
        return value_cont(exp, cont)
      try: exp_cont = exp.cont
      except: return value_cont(exp, cont)
      return exp_cont(cont, self)

  def exps_cont(self, exps, cont):
      if len(exps)==0: return value_cont(None, cont)
      elif len(exps)==1: 
        return self.cont(exps[0], cont)
      else:
        @mycont(cont)
        def left_cont(value, solver):
          solver.scont = solver.exps_cont(exps[1:], cont)
          return value
        return self.cont(exps[0], left_cont)
