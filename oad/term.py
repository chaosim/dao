# -*- coding: utf-8 -*-

from oad.solve import value_cont, mycont, clean_binding

def unify_list(list1, list2, env, occurs_check=False):
  if len(list1)!=len(list2): return
  for x, y in zip(list1, list2):
    try: _ = unify(x, y, env, occurs_check).next()
    except StopIteration: return
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
      
def unify_list_rule_head(values, args, env):
  if len(values)==0: yield set()
  elif len(values)==1: 
    for x in unify_rule_head(values[0], args[0], env): yield x
  else:
    var_set = set()
    for value, arg in zip(values, args):
      try: var_set |= unify_rule_head(value, arg, env).next()
      except StopIteration: return
    yield var_set

def unify_rule_head(value, head, env):
  if isinstance(head, Var): 
    env.bindings[head] = value
    if isinstance(value, Var): yield set([value])
    else: yield set()
  else:
    try: 
      for var_set in value.unify_rule_head(head, env): yield var_set
    except AttributeError: 
      if value==head: yield set()
      
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
  
def closure(exp, env):
  try: return exp.closure(env)
  except AttributeError: 
    if isinstance(exp, list) or isinstance(exp, tuple): 
      return tuple(closure(e, env) for e in exp) 
    else: return exp

class Var:
  def __init__(self, name, index=0): 
    self.name = name
    self.index0 = self.index = index
    
  def __call__(self, *exps): return Apply(self, *exps)
  
  def apply(self, solver, *exps):
    return self.getvalue(solver.env).apply(solver, *exps)
  
  def unify(self, other, env, occurs_check=False):
    self = self.deref(env)
    other = deref(other, env)
    if isinstance(self, Var):
      if isinstance(other, Var) and other is self: return
      if occurs_check and contain_var(other, self):return
      self.setvalue(other, env)
      yield True
      del env.bindings[self]
    elif isinstance(other, Var):
      if occurs_check and contain_var(self, other): return
      other.setvalue(self, env)
      yield True
      del env.bindings[other]
    else:
      for result in unify(self, other, env, occurs_check):
        yield True
      
  def unify_rule_head(self, head, env):
    self.setvalue(copy_rule_head(head, env), env)
    yield set([self])
    self.binding = None
  def copy_rule_head(self, env):
    try: return env.bindings[self]
    except KeyError:
      env.bindings[self] = self.new()
      return env.bindings[self]
    
  def deref(self, env):
    envValue = env[self]
    if not isinstance(envValue, Var): return envValue
    next = env.bindings.get(envValue, None)
    if next is None: return envValue
    if next is self: return next
    result = deref(next, env)
    if result is not next: self.setvalue(result)
    return result
  def getvalue(self, env):
    result = self.deref(env)
    if not isinstance(result, Var): 
      result = getvalue(result, env)
      self.setvalue(result, env)
    return result

  def setvalue(self, value, env):
    if value is not self: env.bindings[self] = value

  def copy(self, memo):
    try: return memo[self]
    except KeyError:
      newvar = memo[self] = self.new()
      return newvar
    
  def new(self): 
    self.index += 1
    return self.__class__(self.name, self.index)
  
  def clean_binding(self): self.binding = None
  
  def free(self, env): return isinstance(self.deref(env), Var)
  
  def __repr__(self): return '%s%s'%(self.name, '_%s'%self.index0 if self.index0!=0 else '')
  def __eq__(self, other): return self is other
  def __hash__(self): return hash(id(self))
  
  def closure(self, env):
    value = self.getvalue(env)
    if value is self: return self
    else: return ClosureVar(self, value)
  
  def cont(self, cont, solver):
    return value_cont(self.getvalue(solver.env), cont)
    
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
    return unify(self.value, other, env, occurs_check)

  def deref(self, env): 
    return deref(self.value, env)
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

def list_value_cont(cont): #  用在单参数函数求cont过程中
  def my_cont(value, solver): yield cont, [value]
  return  my_cont

def make_arguments(*args):
  def my_cont(value, solver):
    pass
  return my_cont

from oad.solve import to_sexpression
class Apply:
  def __init__(self, operator, *operand):
    self.operator = operator
    self.operand = operand
  def to_sexpression(self):
    return (to_sexpression(self.operator),)+tuple(to_sexpression(e) for e in self.operand)
  def cont(self, cont, solver):
    @mycont(cont)
    def evaluate_cont(op, solver): 
      return op.evaluate_cont(self.operand, cont, solver)
    return solver.cont(self.operator, evaluate_cont)

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
  
class Function: 
  def evaluate_cont(self, exps, cont, solver):
    def evaluate_arguments(exps, cont):
        if len(exps)==0: return cont([], solver)
        else:
          @mycont(cont)
          def argument_cont(value, solver):
            @mycont(cont)
            def gather_cont(values, solver):
                for c, v in cont([value]+values, solver): yield c, v
            return evaluate_arguments(exps[1:], gather_cont)
          return solver.cont(exps[0], argument_cont)(True, solver)
    @mycont(cont)
    def apply_cont(values, solver): 
      return self.apply(solver, values, cont)
    return evaluate_arguments(exps,apply_cont)

class Macro: 
  def evaluate_cont(self, exps, cont, solver):
    exps1 = [(closure(exp, solver.env)) for exp in exps]
    return self.apply(solver, exps1, cont)


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
    if self.__class__!=other.__class__: return
    for x in unify_rule_head(self.head, other.tail, env):
      for y in unify_rule_head(self.head, other.tail, env):
        yield x|y
          
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
    return Cons(copy(self.head, memo), copy(self.tail, memo))
  def closure(self, env): 
    head = closure(self.head, env)
    tail = closure(self.tail, env)
    if head==self.head and tail==self.tail:
      return self
    return Cons(head, tail)
  
  def clean_binding(self): 
    clean_binding(self.head)
    clean_binding(self.tail)

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
