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
  
class LogicVar(object):
  def __init__(self, name):
    self.name = name
        
  def deref(self, bindings):
    # todo:
    # how to shorten the binding chain? need to change solver.fail_cont.
    # deref(self, solver) can help
    while 1: 
      next = bindings[self]
      if not isinstance(next, LogicVar) or next==self:
        return next
      else: 
        self = next
  
  def __eq__(x, y):
    return x.__class__==y.__class__ and x.name==y.name
  
  def __hash__(self): return hash(self.name)
  
  def __repr__(self):
    return "%s"%self.name 

class UnquoteSplice:
  def __init__(self, item):
    self.item = item
  
  def __repr__(self):
    return ',@%s'%self.item

class ExpressionWithCode:
  def __init__(self, exp, function):
    self.exp = exp
    self.function = function
    
  def __eq__(x, y):
    return (x.__class__==y.__class__ and x.exp==y.exp) or x.exp==y
  
  def __iter__(self):
    return iter(self.exp)
  
  def __repr__(self):
    return repr(self.exp)

class MacroFunction: 
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    return self.function(*args)

def default_end_cont(v):
  raise NoSolution(v)

class Solver:
  def __init__(self):
    self.bindings = Bindings() # for logic variable, unify
    self.parse_state = None # for parser
    #self.new_logicvar_map = {} #{'name':index} for generating new logic variable
    
  def pop_catch_cont(self, tag):
    result = self.catch_cont_map[tag].pop()
    if not self.catch_cont_map[tag]:
      del self.catch_cont_map[tag]
    return result
      
  def find_catch_cont(self, tag):
    try:
      cont_stack = self.catch_cont_map[tag]
    except:
      raise DaoUncaughtThrow(tag)
    return cont_stack.pop()  
  
  def xxxnew_logicvar(self, name):
    try: 
      suffix = str(self.new_logicvar_map[name])
      self.new_logicvar_map[name] += 1
      return LogicVar(name+suffix)
    except:
      self.new_logicvar_map[name] = 1
      return LogicVar(name)
