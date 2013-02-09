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

def get_value(exp, memo, bindings):
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
      return result_getvalue(memo, bindings)
  
  def unify(x, y, solver):
    solver.bindings[x] = y
    return True
  
  def __eq__(x, y):
    return x.__class__==y.__class__ and x.name==y.name
  
  def __hash__(self): return hash(self.name)
  
  def __repr__(self):
    return "%s"%self.name 

class DummyVar(LogicVar):
  def deref(self, bindings):
    return self
  
class Cons: 
  def __init__(self, head, tail):
    self.head, self.tail = head, tail
    
  def unify(self, other, solver):
    if self.__class__!=other.__class__: 
      return solver.fail_cont(False)
    if solver.unify(self.head, other.head):
      if solver.unify(self.tail, other.tail):
        return True
    return solver.fail_cont(False)

  def match(self, other):
    if self.__class__!=other.__class__: return False
    return match(self.head, other.head) and match(self.tail, other.tail)

  def unify_rule_head(self, other, env, subst):
    if self.__class__!=other.__class__: return
    for _ in unify_rule_head(self.head, other.head, env, subst):
      for _ in unify_rule_head(self.tail, other.tail, env, subst):
        yield True
          
  def copy_rule_head(self, env):
    head = copy_rule_head(self.head, env)
    tail = copy_rule_head(self.tail, env)
    if head==self.head and tail==self.tail: return self
    return Cons(head, tail)

  def getvalue(self, memo, env):
    head = get_value(self.head, memo, env)
    tail = get_value(self.tail, memo, env)
    return Cons(head, tail)
  
  def take_value(self, env):
    head = take_value(self.head, env)
    tail = take_value(self.tail, env)
    if head==self.head and tail==self.tail:
      return self
    return Cons(head, tail)

  def copy(self, memo): 
    return Cons(copy(self.head, memo), copy(self.tail, memo))
  
  def closure(self, env): 
    head = closure(self.head, env)
    tail = closure(self.tail, env)
    if head==self.head and tail==self.tail:
      return self
    return Cons(head, tail)
  
  def __eq__(self, other): 
    return self.__class__==other.__class__ and self.head==other.head and self.tail==other.tail
     
  def __iter__(self):
    tail = self 
    while 1:
      yield tail.head
      if tail.tail is nil: return
      elif isinstance(tail.tail, Cons): 
        tail = tail.tail
      else: 
        yield tail.tail
        return
  def __len__(self): return len([e for e in self])
  def __repr__(self): return 'L(%s)'%' '.join([repr(e) for e in self])

cons = Cons

class Nil: 
  def alpha(self, env, compiler):
    return il.Nil()
  
  def __len__(self): 
    return 0
  
  def __iter__(self): 
    if 0: yield
    
  def __repr__(self): return 'nil'
  
nil = Nil()

def conslist(*elements):
  result = nil
  for term in reversed(elements): result = Cons(term, result)
  return result

def cons2tuple(item):
  if not isinstance(item, Cons) and not isinstance(item, list) \
     and not isinstance(item, tuple): 
    return item
  return tuple(cons2tuple(x) for x in item)

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
  
  def unify(self, x, y):
    x = deref(x, self.bindings)
    y = deref(y, self.bindings)
    try:
      x_unify = x.unify
    except:
      try:
        y_unify = y.unify
      except:
        if x==y: return True
        else: 
          return self.fail_cont(False)
      return y_unify(x, self)
    return x_unify(y, self)
  
  def find_catch_cont(self, tag):
    try:
      cont_stack = self.catch_cont_map[tag]
    except:
      raise DaoUncaughtThrow(tag)
    return cont_stack.pop()