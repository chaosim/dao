class Error(Exception): pass

class UnifyFail(Error): pass

class Subst:
  def __init__(self, bindings={}):
    self.bindings = bindings
  def __getitem__(self, var): 
    try: return self.bindings[var]
    except: return None
  def __setitem__(self, var, value): self.bindings[var] = value
  def __delitem__(self, var): del self.bindings[var]
  def __repr__(self): return str(self.bindings)
subst = Subst()

class EnvHolder:
  def __getitem__(self, var): return self.evaluator.env[var]
  def __setitem__(self, var, value): self.evaluator.env[var] = value
  def __getattr__(self, attr): return getattr(self.evaluator.env, attr)
env = EnvHolder()

def succeed(): yield
def fail(): 
  if 0: yield

def unify_list(list1, list2, occurs_check=False):
  if len(list1)!=len(list2): return
  for x in unify(list1[0], list2[0], occurs_check):
    for y in unify_list(list1[1:], list2[1:], occurs_check):
      yield 
def unify(x, y, occurs_check=False):
  try: return x.unify(y, occurs_check)
  except AttributeError: 
    try: 
      return y.unify(x, occurs_check)
    except AttributeError: 
      if x==y: return succeed()
      else: return fail()
      
def unify_list_rule_head(list1, list2):
  if len(list1)!=len(list2): return
  elif len(list1)==0: yield
  elif len(list1)==1: 
    for x in unify_rule_head(list1[0], list2[0]):
      yield
  else:
    for x in unify_rule_head(list1[0], list2[0]):
      for y in unify_list_rule_head(list1[1:], list2[1:]):
        yield 
def unify_rule_head(x, y):
  try: return x.unify_rule_head(y)
  except AttributeError: 
    if isinstance(y, Var): 
      env.bindings[y] = x
      return succeed()
    else:
      if x==y: return succeed()
      else: return fail()
      
def deref(x):
  try: return x.deref()
  except AttributeError: return x
  
def getvalue(x):
  try: return x.getvalue()
  except AttributeError: return x

  
def copy(x, memo):
  try: return x.copy(memo)
  except AttributeError: return x

def copy_rule_head(x):
  try: return x.copy_rule_head()
  except AttributeError: return x

def contain_var(x, y):
  try: return x.contain_var(x, y)
  except: return False
  
def closure(x):
  try: return x.closure()
  except AttributeError: return x 
  
class Var: 
  created_after_choice_point = None
  def __init__(self, name, index=0): 
    self.name = name
    self.index0 = self.index = index
    
  def __call__(self, *exps): 
    return Apply(self, *exps)
  def apply(self, evaluator, *exps):
    return self.getvalue().apply(evaluator, *exps)
  def unify(self, other, occurs_check=False):
    self = self.deref()
    other = deref(other)
    if isinstance(self, Var):
      if isinstance(other, Var) and other is self: return
      if occurs_check and contain_var(other, self):return
      self.setvalue(other)
      yield
    if isinstance(other, Var):
      if occurs_check and contain_var(self, other): return
      other.setvalue(self)
      yield
    else:
      for result in unify(self, other, occurs_check):
        yield

  def unify_rule_head(self, other):
    self.setvalue(copy_rule_head(other))
    yield
  def copy_rule_head(self):
    try: return env.bindings[self]
    except KeyError:
      env.bindings[self] = newvar = self.new()
      return newvar
    
  def deref(self):
    envValue = env[self]
    if isinstance(envValue, Var): self = envValue
    else: return envValue
    next = subst[self]
    if next is None: return self
    result = deref(next)
    if result is not next: self.setvalue(result)
    return result
  def getvalue(self):
    result = self.deref()
    if not isinstance(result, Var): return getvalue(result)
    return result

  def setvalue(self, value):
    if value is not self: subst[self] = value
    else: del subst[self]

  def copy(self, memo):
    try: return memo[self]
    except KeyError:
      newvar = memo[self] = self.new()
      return newvar
    
  def new(self): 
    self.index += 1
    return self.__class__(self.name, self.index)
  
  def free(self): return isinstance(self.deref(), Var)
  
  def __repr__(self): return '%s%s'%(self.name, '_%s'%self.index0 if self.index0!=0 else '')
  def __eq__(self, other): return self is other
  def __hash__(self): return hash(id(self))
  
  def closure(self):
    value = self.getvalue()
    if value is self: return self
    else: return ClosureVar(self, value)
  
  def solve(self, evaluator):
    yield self.getvalue()
  def __add__(self, other): 
    from oad.builtins.arith import add
    return add(self, other)
  def __sub__(self, other): 
    from oad.builtins.arith import sub
    return sub(self, other)

class LocalVar(Var):  
  def deref(self):
    try: envValue = env.bindings[self]
    except: envValue = env.bindings[self] = Var(self.name, self.index+1)
    if isinstance(envValue, Var): self = envValue
    else: return envValue
    next = subst[self]
    if next is None: return self
    result = next.deref()
    if result is not next: self.setvalue(result)
    return result

class ClosureVar(Var):
  def __init__(self, var, value):
    self.var, self.value = var, value
    self.name = var.name
  
  def unify(self, other, occurs_check=False):
    return self.value._unify(other, occurs_check)

  def deref(self): return self.value.deref()
  def getvalue(self): return self.value.getvalue()
  def setvalue(self, value): self.var.setvalue(value)

  def copy(self, memo):
    try: return memo[self]
    except KeyError:
      newvar = memo[self] = self.__class__(self.name)
      return newvar
    
  def closure(self):
    value = self.var.getvalue()
    if value is self.var: return self.var
    else: return ClosureVar(self.var, value)
    
  def free(self): return isinstance(self.value, Var)
  
  def __repr__(self): return '(%s:%s)'%(self.var, self.value)
  def __eq__(self, other): return self.var is other

class DummyVar(Var):
  def __init__(self, name='_v'): Var.__init__(self, name)
  def unify(self, other, occurs_check=False):
    return self._unify(other, occurs_check)
  def unify_rule_head(self, other): 
    return self.unify(other)
  def deref(self): return self
  def getvalue(self):
    if subst[self] is None: return self
    return  subst[self].getvalue()
  def closure(self): return self
  def free(self): return True  
  def __eq__(self, other): return self.__class__ == other.__class__
   
class Apply:
  def __init__(self, operator, *operand):
    self.operator = operator
    self.operand = operand
  def solve(self, evaluator):
    for x in self.operator.apply(evaluator, *self.operand):
      yield x
  def closure(self):
    return Apply(self.operator, *[closure(x) for x in self.operand])
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

SUCCESS = True
##class Nil(USinglet):
##  def __str__(self): return 'NIL'
##  def __repr__(self): return "Nil()"
##  def __len__(self): return 0
##  def __iter__(self): 
##    if 0: yield  
##NIL = Nil()

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
    
  def copy_rule_head(self):
    head = copy_rule_head(self.head)
    tail = copy_rule_head(self.tail)
    if head==self.head and tail==self.tail:
      return self
    return Cons(head, tail)

  def getvalue(self):
    head = getvalue(self.head)
    tail = getvalue(self.tail)
    if head==self.head and tail==self.tail:
      return self
    return Cons(head, tail)

  def copy(self, memo): 
    return Cons(copy(head, memo), copy(tail, memo))
  def closure(self): 
    head = closure(self.head)
    tail = closure(self.tail)
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

##
##def pairp(x): return isinstance(x, Cons)
##def car(t): return t.head
##def cdr(t): return t.tail
##def cadr(t): return t.tail.head
##def caddr(t): return t.tail.tail.head
