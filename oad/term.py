# -*- coding: utf-8 -*-

from oad.solve import value_cont, mycont, clean_binding

def unify_list(list1, list2, env, occurs_check=False):
  if len(list1)!=len(list2): return
  if len(list1)==0: yield True
  if len(list1)==1: 
    for _ in unify_list(list1[0], list2[0], env, occurs_check):
      yield True
  for _ in unify_list(list1[0], list2[0], env, occurs_check):
    for _ in unify_list(list1[1:], list2[1:], env, occurs_check):
      yield True
  
def unify(x, y, env, occurs_check=False):
  try: return x.unify(y, env, occurs_check)
  except AttributeError: 
    try: return y.unify(x, env, occurs_check)
    except AttributeError: return (True,) if x==y else ()
      
##def unify_list_rule_head(values, args, callee_env, caller_env):
##  if len(values)==0: yield set()
##  elif len(values)==1: 
##    for x in unify_rule_head(values[0], args[0], callee_env, caller_env): yield x
##  else:
##    var_set = set()
##    for value, arg in zip(values, args):
##      try: var_set |= unify_rule_head(value, arg, callee_env, caller_env).next()
##      except StopIteration: return
##    yield var_set
##
def unify_list_rule_head(values, args, callee_env, caller_env, varset):
  if len(values)==0: yield varset
  elif len(values)==1: 
    for varset1 in unify_rule_head(values[0], args[0], callee_env, caller_env, varset): 
      yield varset1
  else:
    for var_set1 in unify_rule_head(values[0], args[0], callee_env, caller_env, varset):
      for var_set2 in unify_rule_head(values[1:], args[1:], callee_env, caller_env, var_set1):
        yield var_set2

def unify_rule_head(value, head, callee_env, caller_env, varset):
  if isinstance(head, Var): 
    if isinstance(value, Var):
      caller_env.bindings[value] = head.copy_rule_head(callee_env)
      yield varset | set([value])
      del caller_env.bindings[value]
    else:
      callee_env.bindings[head] = value
      yield varset
  else:
    try: 
      for var_set1 in value.unify_rule_head(head, callee_env, caller_env, varset): 
        yield var_set1
    except AttributeError: 
      if (isinstance(head, list) or isinstance(head, tuple)) \
          and (isinstance(value, list) or isinstance(value, tuple)):
        if len(value)!=len(head): return
        for var_set in unify_list_rule_head(value, head, callee_env, caller_env, varset): 
          yield var_set
      elif value==head: yield varset
      
def deref(x, env):
  try: return x.deref(env)
  except AttributeError: 
    if isinstance(x, list): return [deref(e, env) for e in x]
    elif isinstance(x, tuple): return tuple(deref(e, env) for e in x)
    else: return x
  
def getvalue(x, env):
  try: return x.getvalue(env)
  except AttributeError: 
    if isinstance(x, list): return [getvalue(e, env) for e in x]
    elif isinstance(x, tuple): return tuple(getvalue(e, env) for e in x)
    else: return x
  
def copy(x, memo):
  try: return x.copy(memo)
  except AttributeError: 
    if isinstance(x, list): return [getvalue(e, memo) for e in x]
    elif isinstance(x, tuple): return tuple(getvalue(e, memo) for e in x)
    else: return x

def copy_rule_head(arg_exp, env):
  try: return arg_exp.copy_rule_head(env)
  except AttributeError: 
    if isinstance(arg_exp, list): 
      return [copy_rule_head(e, env) for e in arg_exp]
    elif isinstance(arg_exp, tuple):
      return tuple(copy_rule_head(e, env) for e in arg_exp)
    else: return arg_exp

def contain_var(x, y):
  try: return x.contain_var(x, y)
  except: return False
  
def closure(exp, env):
  try: return exp.closure(env)
  except AttributeError: 
    if isinstance(exp, list) or isinstance(exp, tuple): 
      return tuple(closure(e, env) for e in exp) 
    else: return exp

var_index = 1
class Var:
  def __init__(self, name, index=0): 
    self.name = name
    self.index = index
    
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
      try: del env.bindings[self]
      except: pass
    elif isinstance(other, Var):
      if occurs_check and contain_var(self, other): return
      other.setvalue(self, env)
      yield True
      try: del env.bindings[self]
      except: pass
    else:
      for result in unify(self, other, env, occurs_check):
        yield True
      
  def unify_rule_head(self, head, callee_env, caller_env, varset):
    caller_env.bindings[self] = copy_rule_head(head, callee_env)
    yield varset | set([self])
    del caller_env.bindings[self]
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
    global var_index
    var_index += 1
    return self.__class__(self.name, var_index)
  
  def clean_binding(self): self.binding = None
  
  def free(self, env): return isinstance(self.deref(env), Var)
  
  def __repr__(self): return '%s%s'%(self.name, '_%s'%self.index if self.index!=0 else '')
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

class LocalVar(Var):   # 好像不再必要了。  
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
  
  def unify(self, other, env, occurs_check=False):
    return unify(self.value, other, env, occurs_check)

  def deref(self, env): return self
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
  def __init__(self, name='_v', index=0): Var.__init__(self, name)
  def unify_rule_head(self, other, callee_env, caller_env, varset): 
    for x in self.unify(other, callee_env):
      yield varset | set([self])
##  def unify(self, other, env, occurs_check=False):
##    self.setvalue(other, env)
##    yield True
  def deref(self, env): return self
  def getvalue(self, env):
    binding = env[self]
    if binding is self: return binding
    return getvalue(binding, env)
  def closure(self, env): return self
  def free(self, env): return True  
  def __eq__(self, other): return self.__class__ == other.__class__

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
  def __add__(self, other):
    from oad.special import begin
    return begin(self, other)
  def __or__(self, other):
    from oad.builtins.control import or_
    return or_(self, other)
  
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
          
  def unify_rule_head(self, other, callee_env, caller_env, varset):
    if self.__class__!=other.__class__: return
    for varset1 in unify_rule_head(self.head, other.head, callee_env, caller_env, varset):
      for varset2 in unify_rule_head(self.tail, other.tail, callee_env, caller_env, varset1):
        yield varset2
          
  def copy_rule_head(self, env):
    head = copy_rule_head(self.head, env)
    tail = copy_rule_head(self.tail, env)
    if head==self.head and tail==self.tail: return self
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
  def __repr__(self): return 'nil'

nil = Nil()

def conslist(*elements): 
  result = nil
  for term in reversed(elements): result = Cons(term, result)
  return result
