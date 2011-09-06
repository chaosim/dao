class Error(Exception): pass

class UnifyFail(Error): pass

##def unify_list(list1, list2):
##  if len(list1)!=list2: return
##  for x in unify(list1[0], list2[0]):
##    for y in unify_list(list1[1:], list2[1:]):
##      yield True
##    
##  
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

def unify(x, y, trail, occurs_check=False):
  try: x.unify(y, trail, occurs_check)
  except AttributeError: 
    try: y.unify(x, trail, occurs_check)
    except AttributeError:
      if x!=y: raise UnifyFail
      
def unify_rule_head(x, y, trail):
  try: x.unify_rule_head(y, trail)
  except AttributeError: 
    if isinstance(y, Var): env.bindings[y] = x
    else:
      if x!=y: raise UnifyFail
      
def deref(x, trail):
  try: return x.deref(trail)
  except AttributeError: return x
  
def getvalue(x, trail):
  try: return x.getvalue(trail)
  except AttributeError: return x

  
def copy(x, trail, memo):
  try: return x.copy(trail, memo)
  except AttributeError: return x

def copy_rule_head(x, trail):
  try: return x.copy_rule_head(trail)
  except AttributeError: return x

def contain_var(x, y):
  try: return x.contain_var(x, y)
  except: return False
  
def closure(x, trail):
  try: return x.closure(trail)
  except AttributeError: return x 
  
class Var: 
  created_after_choice_point = None
  def __init__(self, name, index=0): 
    self.name = name
    self.index0 = self.index = index
    
  def __call__(self, *exps): 
    return Apply(self, *exps)
  def apply(self, evaluator, *exps):
    return self.getvalue(evaluator.trail).apply(evaluator, *exps)
  def unify(self, other, trail, occurs_check=False):
    self = self.deref(trail)
    other = deref(other, trail)
    if isinstance(self, Var):
      if isinstance(other, Var) and other is self: return
      if occurs_check and contain_var(other, self, trail): raise UnifyFail()
      self.setvalue(other, trail)
      return
    if isinstance(other, Var):
      if occurs_check and contain_var(self, other, trail): raise UnifyFail()
      other.setvalue(self, trail)
      return
    unify(self, other, trail, occurs_check)

  def unify_rule_head(self, other, trail):
    self.setvalue(copy_rule_head(other, trail), trail)
  def copy_rule_head(self, trail):
    try: return env.bindings[self]
    except KeyError:
      env.bindings[self] = newvar = self.new()
      return newvar
    
  def deref(self, trail):
    envValue = env[self]
    if isinstance(envValue, Var): self = envValue
    else: return envValue
    next = subst[self]
    if next is None: return self
    result = deref(next, trail)
    if result is not next: self.setvalue(result, trail)
    return result
  def getvalue(self, trail):
    result = self.deref(trail)
    if not isinstance(result, Var): return getvalue(result, trail)
    return result

  def setvalue(self, value, trail):
    trail.add(self)
    if value is not self: subst[self] = value
    else: del subst[self]

  def copy(self, trail, memo):
    try: return memo[self]
    except KeyError:
      newvar = memo[self] = self.new()
      return newvar
    
  def new(self): 
    self.index += 1
    return self.__class__(self.name, self.index)
  
  def free(self, trail): return isinstance(self.deref(trail), Var)
  
  def __repr__(self): return '%s%s'%(self.name, '_%s'%self.index0 if self.index0!=0 else '')
  def __eq__(self, other): return self is other
  def __hash__(self): return hash(id(self))
  
  def closure(self, trail):
    value = self.getvalue(trail)
    if value is self: return self
    else: return ClosureVar(self, value)
  
  def solve(self, evaluator):
    yield self.getvalue(evaluator.trail)
  def __add__(self, other): 
    from oad.builtins.arith import add
    return add(self, other)
  def __sub__(self, other): 
    from oad.builtins.arith import sub
    return sub(self, other)

class LocalVar(Var):  
  def deref(self, trail):
    try: envValue = env.bindings[self]
    except: envValue = env.bindings[self] = Var(self.name, self.index+1)
    if isinstance(envValue, Var): self = envValue
    else: return envValue
    next = subst[self]
    if next is None: return self
    result = next.deref(trail)
    if result is not next: self.setvalue(result, trail)
    return result

class ClosureVar(Var):
  def __init__(self, var, value):
    self.var, self.value = var, value
    self.name = var.name
  
  def unify(self, other, trail, occurs_check=False):
    return self.value._unify(other, trail, occurs_check)

  def deref(self, trail): return self.value.deref(trail)
  def getvalue(self, trail): return self.value.getvalue(trail)
  def setvalue(self, value, trail): self.var.setvalue(value, trail)

  def copy(self, trail, memo):
    try: return memo[self]
    except KeyError:
      newvar = memo[self] = self.__class__(self.name)
      return newvar
    
  def closure(self, trail):
    value = self.var.getvalue(trail)
    if value is self.var: return self.var
    else: return ClosureVar(self.var, value)
    
  def free(self, trail): return isinstance(self.value, Var)
  
  def __repr__(self): return '(%s:%s)'%(self.var, self.value)
  def __eq__(self, other): return self.var is other

class DummyVar(Var):
  def __init__(self, name='_v'): Var.__init__(self, name)
  def unify(self, other, trail, occurs_check=False):
    return self._unify(other, trail, occurs_check)
  def unify_rule_head(self, other, trail): self.unify(other, trail)
  def deref(self, trail): return self
  def getvalue(self, trail):
    if subst[self] is None: return self
    return  subst[self].getvalue(trail)
  def closure(self, trail): return self
  def free(self, trail): return True  
  def __eq__(self, other): return self.__class__ == other.__class__
   
class Apply:
  def __init__(self, operator, *operand):
    self.operator = operator
    self.operand = operand
  def solve(self, evaluator):
    for x in self.operator.apply(evaluator, *self.operand):
      yield x
  def closure(self, trail):
    return Apply(self.operator, *[closure(x, trail) for x in self.operand])
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
##  def basic_unify(self, other, trail, occurs_check=False):
##    if self.__class__!=other.__class__ or len(self.elements) != len(other.elements): raise UnifyFail
##    for i in range(len(self.elements)):
##      self.elements[i].unify(other.elements[i], trail, occurs_check)
##
##  def basic_unify_rule_head(self, other, trail):
##    if self.__class__!=other.__class__ or len(self.elements)!=len(other.elements): raise UnifyFail
##    for (arg, arg2) in zip(self.elements, other.elements):
##      arg.unify_rule_head(arg2, trail)     
##    
##  def copy_rule_head(self, trail):
##    elements = []
##    newinstance = False
##    for element in self.elements:
##      copied = element.copy_rule_head(trail)
##      if copied is not element: newinstance = True
##      elements.append(copied)
##    if newinstance: return self.__class__(*elements)
##    else: return self
##
##  def getvalue(self, trail):
##    elements = []
##    newinstance = False
##    for element in self.elements:
##      value = element.getvalue(trail)
##      if value is not element: newinstance = True
##      elements.append(value)
##    if newinstance: return self.__class__(*elements)
##    else: return self
##
##  def copy(self, trail, memo):
##    newelements = [arg.copy(trail, memo) for arg in self.elements]
##    return self.__class__(*newelements)
##
##  def makeClosure(self, trail): 
##    newelements = [arg.makeClosure(trail) for arg in self.elements]
##    return self.__class__(*newelements)
##  
##  def __eq__(self, other):
##    return self.__class__==other.__class__ and self.elements==other.elements
##
##class Term(UList):
##  def __init__(self, name, elements):
##    UList.__init__(*elements)
##    self.name = name
##  def basic_unify(self, other, trail, occurs_check=False):
##    if self.__class__!=other.__class__ or len(self.elements) != len(other.elements)\
##        or self.name!=other.name: 
##      raise UnifyFail
##    for i in range(len(self.elements)):
##      self.elements[i].unify(other.elements[i], trail, occurs_check)
##
##  def basic_unify_rule_head(self, other, trail):
##    if self.__class__!=other.__class__ or len(self.elements)!=len(other.elements)\
##        or self.name!=other.name: 
##      raise UnifyFail
##    for (arg, arg2) in zip(self.elements, other.elements):
##      arg.unify_rule_head(arg2, trail)         
##  def copy_rule_head(self, trail):
##    elements = []
##    newinstance = False
##    for arg in self.elements:
##      copied = arg.copy_rule_head(trail)
##      if copied is not arg: newinstance = True
##      elements.append(copied)
##    if newinstance: return self.__class__(self.name, elements)
##    else: return self
##
##  def getvalue(self, trail):
##    elements = []
##    newinstance = False
##    for arg in self.elements:
##      value = arg.getvalue(trail)
##      if value is not arg: newinstance = True
##      elements.append(value)
##    if newinstance: return self.__class__(self.name, elements)
##    else: return self
##
##  def copy(self, trail, memo):
##    newelements = [arg.copy(trail, memo) for arg in self.elements]
##    return self.__class__(self.name, newelements)
##  def makeClosure(self, trail): 
##    newelements = [arg.makeClosure(trail) for arg in self.elements]
##    return self.__class__(self.name, newelements)
##  def __eq__(self, other): return UList.__eq__(self, other) and self.name==other.name
##   

class Cons: 
  def __init__(self, head, tail):
    self.head, self.tail = head, tail
    
  def copy_rule_head(self, trail):
    head = copy_rule_head(self.head, trail)
    tail = copy_rule_head(self.tail, trail)
    if head==self.head and tail==self.tail:
      return self
    return Cons(head, tail)

  def getvalue(self, trail):
    head = getvalue(self.head, trail)
    tail = getvalue(self.tail, trail)
    if head==self.head and tail==self.tail:
      return self
    return Cons(head, tail)

  def copy(self, trail, memo): 
    return Cons(copy(head, trail, memo), copy(tail, trail, memo))
  def closure(self, trail): 
    head = closure(self.head, trail)
    tail = closure(self.tail, trail)
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
