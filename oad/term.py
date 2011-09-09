# -*- coding: utf-8 -*-

def unify_list(list1, list2, env, occurs_check=False):
  if len(list1)!=len(list2): return
  for x in unify(list1[0], list2[0], env, occurs_check):
    for y in unify_list(list1[1:], list2[1:], env, occurs_check):
      yield True
def unify(x, y, env, occurs_check=False):
  try: 
    for result in x.unify(y, env, occurs_check):
      yield True
  except AttributeError: 
    try: 
      for result in y.unify(x, env, occurs_check):
        yield True
    except AttributeError: 
      if x==y: yield True
      
def unify_list_rule_head(list1, list2, env):
  if len(list1)!=len(list2): return
  elif len(list1)==0: yield
  elif len(list1)==1: 
    for x in unify_rule_head(list1[0], list2[0], env):
      yield True
  else:
    for x in unify_rule_head(list1[0], list2[0], env):
      for y in unify_list_rule_head(list1[1:], list2[1:], env):
        yield True
        
def unify_rule_head(x, y, env):
  try: 
    for result in x.unify_rule_head(y, env):
      yield True
  except AttributeError: 
    if isinstance(y, Var): 
      env.bindings[y] = x
      yield True
    else:
      if x==y: yield True
      
def deref(x, env):
  try: return x.deref(env)
  except AttributeError: return x
  
def getvalue(x, env):
  try: return x.getvalue(env)
  except AttributeError: return x

  
def copy(x, memo):
  try: return x.copy(memo)
  except AttributeError: return x

def copy_rule_head(x, env):
  try: return x.copy_rule_head(env)
  except AttributeError: return x

def contain_var(x, y):
  try: return x.contain_var(x, y)
  except: return False
  
def closure(x, env):
  try: return x.closure(env)
  except AttributeError: return x 
  
class Var: 
  def __init__(self, name, index=0): 
    self.name = name
    self.index0 = self.index = index
    self.binding = None
    
  def __call__(self, *exps): return Apply(self, *exps)
  
  def apply(self, evaluator, *exps):
    return self.getvalue(evaluator.env).apply(evaluator, *exps)
  
  def unify(self, other, env, occurs_check=False):
    self = self.deref(env)
    other = deref(other, env)
    if isinstance(self, Var):
      if isinstance(other, Var) and other is self: return
      if occurs_check and contain_var(other, self):return
      self.setvalue(other)
      yield True
      self.binding = None
    elif isinstance(other, Var):
      if occurs_check and contain_var(self, other): return
      other.setvalue(self)
      yield True
      other.binding = None
    else:
      for result in unify(self, other, env, occurs_check):
        yield True

  def unify_rule_head(self, other, env):
    self.setvalue(copy_rule_head(other, env))
    yield
    self.binding = None
  def copy_rule_head(self, env):
    try: return env.bindings[self]
    except KeyError:
      env.bindings[self] = newvar = self.new()
      return newvar
    
  def deref(self, env):
    envValue = env[self]
    if not isinstance(envValue, Var): return envValue
    next = envValue.binding
    if next is None: return envValue
    result = deref(next, env)
    if result is not next: self.setvalue(result)
    return result
  def getvalue(self, env):
    result = self.deref(env)
    if not isinstance(result, Var): return getvalue(result, env)
    return result

  def setvalue(self, value):
    if value is not self: self.binding = value

  def copy(self, memo):
    try: return memo[self]
    except KeyError:
      newvar = memo[self] = self.new()
      return newvar
    
  def new(self): 
    self.index += 1
    return self.__class__(self.name, self.index)
  
  def free(self, env): return isinstance(self.deref(env), Var)
  
  def __repr__(self): return '%s%s'%(self.name, '_%s'%self.index0 if self.index0!=0 else '')
  def __eq__(self, other): return self is other
  def __hash__(self): return hash(id(self))
  
  def closure(self, env):
    value = self.getvalue(env)
    if value is self: return self
    else: return ClosureVar(self, value)
  
  def solve(self, evaluator):
    yield self.getvalue(evaluator.env)
    
  def __add__(self, other): 
    from oad.builtins.arith import add
    return add(self, other)
  def __sub__(self, other): 
    from oad.builtins.arith import sub
    return sub(self, other)

class LocalVar(Var):  
  def deref(self, env):
    try: envValue = env.bindings[self]
    except: envValue = env.bindings[self] = Var(self.name, self.index+1)
    if isinstance(envValue, Var): self = envValue
    else: return envValue
    next = self.binding
    if next is None: return self
    result = next.deref(env)
    if result is not next: self.setvalue(result)
    return result

class ClosureVar(Var):
  def __init__(self, var, value):
    self.var, self.value = var, value
    self.name = var.name
    self.binding = None
  
  def unify(self, other, env, occurs_check=False):
    return self.value._unify(other, env, occurs_check)

  def deref(self, env): return self.value.deref(env)
  def getvalue(self, env): return getvalue(self.value, env)
  def setvalue(self, value): self.var.setvalue(value)

  def copy(self, memo):
    try: return memo[self]
    except KeyError:
      newvar = memo[self] = self.__class__(self.name)
      return newvar
    
  def closure(self, env):
    value = self.var.getvalue(env)
    if value is self.var: return self.var
    else: return ClosureVar(self.var, value)
    
  def free(self, env): return isinstance(self.value, Var)
  
  def __repr__(self): return '(%s:%s)'%(self.var, self.value)
  def __eq__(self, other): return self.var is other

class DummyVar(Var):
  def __init__(self, name='_v'): Var.__init__(self, name)
  def unify(self, other, env, occurs_check=False):
    return self._unify(other, occurs_check)
  def unify_rule_head(self, other, env): 
    return self.unify(other, env)
  def deref(self, env): return self
  def getvalue(self, env):
    if self.binding is None: return self
    return  self.binding.getvalue(env)
  def closure(self, env): return self
  def free(self, env): return True  
  def __eq__(self, other): return self.__class__ == other.__class__
   
class Apply:
  def __init__(self, operator, *operand):
    self.operator = operator
    self.operand = operand
  def solve(self, evaluator):
    for x in self.operator.apply(evaluator, *self.operand):
      yield x
  def closure(self, env):
    return Apply(self.operator, *[closure(x, env) for x in self.operand])
  def __repr__(self): 
    return '%s(%s)'%(self.operator, 
                ','.join([repr(e) for e in self.operand]))
  def __and__(self, other):
    from oad.builtins.control import and_
    return and_(self, other)
  
  def __eq__(self, other): 
    return self.operator==other.operator and self.operand==other.operand
  
class Function: pass

class Macro: pass

##class UList:
##  def __init__(self, *elements): 
##    self.elements = elements
##  def __iter__(self): return iter(self.elements)
##  def __len__(self): return len(self.elements)
##  def __getitem__(self, index): 
##    try: 
##      if type(index)==int: return self.elements[index]
##    except: return UList()
##    return UList(self.elements[index])
##  def __repr__(self): return "U(%r)" % self.elements
##  
##  def basic_unify(self, other, occurs_check=False):
##    if self.__class__!=other.__class__ or len(self.elements) != len(other.elements): raise UnifyFail
##    for i in range(len(self.elements)):
##      self.elements[i].unify(other.elements[i], occurs_check)
##
##  def basic_unify_rule_head(self, other):
##    if self.__class__!=other.__class__ or len(self.elements)!=len(other.elements): raise UnifyFail
##    for (arg, arg2) in zip(self.elements, other.elements):
##      arg.unify_rule_head(arg2)     
##    
##  def copy_rule_head(self):
##    elements = []
##    newinstance = False
##    for element in self.elements:
##      copied = element.copy_rule_head()
##      if copied is not element: newinstance = True
##      elements.append(copied)
##    if newinstance: return self.__class__(*elements)
##    else: return self
##
##  def getvalue(self):
##    elements = []
##    newinstance = False
##    for element in self.elements:
##      value = element.getvalue()
##      if value is not element: newinstance = True
##      elements.append(value)
##    if newinstance: return self.__class__(*elements)
##    else: return self
##
##  def copy(self, memo):
##    newelements = [arg.copy(, memo) for arg in self.elements]
##    return self.__class__(*newelements)
##
##  def makeClosure(self): 
##    newelements = [arg.makeClosure() for arg in self.elements]
##    return self.__class__(*newelements)
##  
##  def __eq__(self, other):
##    return self.__class__==other.__class__ and self.elements==other.elements
##
##class Term(UList):
##  def __init__(self, name, elements):
##    UList.__init__(*elements)
##    self.name = name
##  def basic_unify(self, other, occurs_check=False):
##    if self.__class__!=other.__class__ or len(self.elements) != len(other.elements)\
##        or self.name!=other.name: 
##      raise UnifyFail
##    for i in range(len(self.elements)):
##      self.elements[i].unify(other.elements[i], occurs_check)
##
##  def basic_unify_rule_head(self, other):
##    if self.__class__!=other.__class__ or len(self.elements)!=len(other.elements)\
##        or self.name!=other.name: 
##      raise UnifyFail
##    for (arg, arg2) in zip(self.elements, other.elements):
##      arg.unify_rule_head(arg2)         
##  def copy_rule_head(self):
##    elements = []
##    newinstance = False
##    for arg in self.elements:
##      copied = arg.copy_rule_head()
##      if copied is not arg: newinstance = True
##      elements.append(copied)
##    if newinstance: return self.__class__(self.name, elements)
##    else: return self
##
##  def getvalue(self):
##    elements = []
##    newinstance = False
##    for arg in self.elements:
##      value = arg.getvalue()
##      if value is not arg: newinstance = True
##      elements.append(value)
##    if newinstance: return self.__class__(self.name, elements)
##    else: return self
##
##  def copy(self, memo):
##    newelements = [arg.copy(, memo) for arg in self.elements]
##    return self.__class__(self.name, newelements)
##  def makeClosure(self): 
##    newelements = [arg.makeClosure() for arg in self.elements]
##    return self.__class__(self.name, newelements)
##  def __eq__(self, other): return UList.__eq__(self, other) and self.name==other.name
##   

class Cons: 
  def __init__(self, head, tail):
    self.head, self.tail = head, tail
    
  def unify(self, other, env, occurs_check=False):
    if isinstance(other, Var): 
      for x in other.unify(self, env, occurs_check):
        yield True
    else:
      if self.__class__!=other.__class__: return
      for x in unify(self.head, other.head, env, occurs_check):
        for y in unify(self.tail, other.tail, env, occurs_check):
          yield True
          
  def unify_rule_head(self, other, env):
    if isinstance(other, Var): 
      env.bindings[other] = self
      yield True
    else:
      if self.__class__!=other.__class__: return
      for x in unify_rule_head(self.head, other.tail, env):
        for y in unify_rule_head(self.head, other.tail, env):
          yield True
          
  def copy_rule_head(self, env):
    head = copy_rule_head(self.head, env)
    tail = copy_rule_head(self.tail, env)
    if head==self.head and tail==self.tail:
      return self
    return Cons(head, tail)

  def getvalue(self, env):
    head = getvalue(self.head, env)
    tail = getvalue(self.tail, env)
    if head==self.head and tail==self.tail:
      return self
    return Cons(head, tail)

  def copy(self, memo): 
    return Cons(copy(head, memo), copy(tail, memo))
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
      if tail.tail is None: return
      elif isinstance(tail.tail, Cons): 
        tail = tail.tail
      else: 
        yield tail.tail
        return
  def __len__(self): return len([e for e in self])
  def __repr__(self): return 'cons(%s)'%' '.join([repr(e) for e in self])
  
cons = Cons

def conslist(*elements): 
  result = None
  for term in reversed(elements): result = Cons(term, result)
  return result
