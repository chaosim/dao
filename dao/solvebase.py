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
  
  def __delitem__(self, var):
    try:
      dict.__delitem__(self, var)
    except KeyError: 
      pass
      
  def copy(self):
    return Bindings(dict.copy(self))

def deref(exp, bindings):
  try: exp_deref = exp.deref
  except:
    return exp
  return exp_deref(bindings)

def getvalue(exp, memo, bindings):
  try:
    exp_getvalue = exp.getvalue
  except:
    return exp
  return exp_getvalue(memo, bindings)
    
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
  
  def getvalue(self, memo, bindings):
    try: return memo[self]
    except:
      result = LogicVar.deref(self, bindings)
      if isinstance(result, LogicVar): 
        memo[self] = result
        return result
      try: 
        result_getvalue = result.getvalue
      except: 
        memo[self] = result
        return result
      return result_getvalue(memo)
    
  def __eq__(x, y):
    return x.__class__==y.__class__ and x.name==y.name
  
  def __hash__(self): return hash(self.name)
  
  def __repr__(self):
    return "%s"%self.name 

class DummyVar(LogicVar):
  def deref(self, bindings):
    return self
  
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

class Macro: pass 

class MacroFunction(Macro): 
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    return self.function(*args)
  
class MacroRules(Macro): 
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    return self.function(*args)


def default_end_cont(v):
  raise NoSolution(v)

class Solver:
  def __init__(self):
    self.bindings = Bindings() # for logic variable, unify
    self.parse_state = None
    self.catch_cont_map = {}
    self.cut_cont = self.cut_or_cont = self.fail_cont = default_end_cont
    
  def find_catch_cont(self, tag):
    try:
      cont_stack = self.catch_cont_map[tag]
    except:
      raise DaoUncaughtThrow(tag)
    return cont_stack.pop()