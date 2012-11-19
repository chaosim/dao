# -*- coding: utf-8 -*-
''' some basic utilities for solve dao expression.'''

class BaseCommand: pass

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

class NoSolution:
  def __init__(self, exp): 
    self.exp = exp
  def __repr__(self): 
    return repr(self.exp)
  
class Solutions:
  def __init__(self, exp, solutions):
    self.exp = exp
    self.solutions = solutions
  def next(self):
    try:
      return self.solutions.next()
    except StopIteration:
      raise NoSolution(self.exp)

class Bindings(dict): 
  def __getitem__(self, var): 
    try: return dict.__getitem__(self, var)
    except: return var
  def __setitem__(self, var, value):
    dict.__setitem__(self, var, value)

def deref(exp, bindings):
  try: exp_deref = exp.deref
  except:
    return exp
  return exp_deref(bindings)
  
def default_end_cont(v):
  raise NoSolution(v)

class Solver:
  def __init__(self, end_cont=None):
    if end_cont is None:
      self.fail_cont = default_end_cont
    else: self.fail_cont = end_cont
    self.cut_or_cont = self.fail_cont
    self.bindings = Bindings()
    self.parse_state = None
    self.catch_cont_map = {}
    self.unwind_cont_stack = []
    self.exit_block_cont_map = {}
    self.continue_block_cont_map = {}
    
  def push_catch_cont(self, tag, cont):
    self.catch_cont_map.setdefault(tag, []).append(cont)

  def pop_catch_cont(self, tag):
    result = self.catch_cont_map[tag].pop()
    if not self.catch_cont_map[tag]:
      del self.catch_cont_map[tag]
    return result
      
  def find_catch_cont(self, tag):
    try:
      cont_stack = self.catch_cont_map[tag]
    except:
      raise CatchError(tag)
    return cont_stack.pop()  
      
  def push_unwind_cont(self, cont):
    self.unwind_cont_stack.append(cont)

  def pop_unwind_cont(self, tag):
    self.unwind_cont_stack.pop()
    
  def unwind(self, old_unwind_cont_stack_length):
    while len(self.unwind_cont_stack)>old_unwind_cont_stack_length:
      self.unwind_cont_stack.pop()(None)
