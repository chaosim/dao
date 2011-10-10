# -*- coding: utf-8 -*-

from oad.solve import value_cont, mycont

def apply_generators(generators): # one shot generators, such as unify, set/restore
  length = len(generators)
  if length==0: 
    yield True
    return
  i = 0
  while i <length:
    try:
      generators[i].next()
      if i==length-1: yield True
      else: i += 1
    except StopIteration:
      if i==0: return
      i -= 1
    except GeneratorExit: raise

def unify_list(list1, list2, env, occurs_check=False):
  if len(list1)!=len(list2): return
  if len(list1)==0: yield True
  if len(list1)==1: 
    for _ in unify_list(list1[0], list2[0], env, occurs_check):
      yield True
  for _ in apply_generators(tuple(unify(x, y, env, occurs_check) 
                            for x, y in zip(list1, list2))):
    yield True
  
def unify(x, y, env, occurs_check=False):
  try: x_unify = x.unify
  except AttributeError: 
    try: y_unify = y.unify 
    except AttributeError: return (True,) if x==y else ()
    return y_unify(x, env, occurs_check)
  return x_unify(y, env, occurs_check)    

def unify_list_rule_head(values, args, env, subst):
  if len(values)==0: yield True
  elif len(values)==1: 
    for _ in unify_rule_head(values[0], args[0], env, subst): 
      yield True
  else:
    for _ in apply_generators(tuple(unify_rule_head(x, y, env, subst) 
                              for x, y in zip(values, args))):
      yield True

def unify_rule_head(value, head, env, subst):
  if isinstance(head, Var): 
    if isinstance(value, Var):
      subst[value] = head.copy_rule_head(env)
      yield True
    else:
      try: 
        old = env.bindings[head]
        env.bindings[head] = value
        yield True
        env.bindings[head] = old
      except:
        env.bindings[head] = value
        yield True
        del env.bindings[head]
  else:
    try: 
      for _ in value.unify_rule_head(head, env, subst): 
        yield True
    except AttributeError: 
      if (isinstance(head, list) or isinstance(head, tuple)) \
          and (isinstance(value, list) or isinstance(value, tuple)):
        if len(value)!=len(head): return
        for _ in unify_list_rule_head(value, head, env, subst): 
          yield True
      elif value==head: yield True
      
def deref(x, env):
  try: x_deref = x.deref
  except AttributeError: 
    if isinstance(x, list): return [deref(e, env) for e in x]
    elif isinstance(x, tuple): return tuple(deref(e, env) for e in x)
    else: return x
  return x_deref(env)
  
def getvalue(x, env):
  try: x_getvalue = x.getvalue
  except AttributeError: 
    if isinstance(x, list): return [getvalue(e, env) for e in x]
    elif isinstance(x, tuple): return tuple(getvalue(e, env) for e in x)
    else: return x
  return x_getvalue(env)

def copy(x, memo):
  try: x_copy = x.copy
  except AttributeError: 
    if isinstance(x, list): return [getvalue(e, memo) for e in x]
    elif isinstance(x, tuple): return tuple(getvalue(e, memo) for e in x)
    else: return x
  return x_copy(memo)

def copy_rule_head(arg_exp, env):
  try: arg_exp_copy_rule_head = arg_exp.copy_rule_head
  except AttributeError: 
    if isinstance(arg_exp, list): 
      return [copy_rule_head(e, env) for e in arg_exp]
    elif isinstance(arg_exp, tuple):
      return tuple(copy_rule_head(e, env) for e in arg_exp)
    else: return arg_exp
  return arg_exp_copy_rule_head(env)

def match_list(list1, list2):
  if len(list1)!=len(list2): return False
  for x, y in zip(list1, list2):
    if not match(x, y): return False
  return True

# match(var, nonvar): True,
# match(nonvar, var): False
def match(x, y):
  try: x_match = x.match(y)
  except AttributeError: 
    if (isinstance(x, list) or isinstance(x, tuple)) or\
       isinstance(y, list) and isinstance(y, tuple):
      return match_list(x, y)
    else: return x==y
  return x_match(y)

def contain_var(x, y):
  try: return x.contain_var(x, y)
  except: return False
  
def closure(exp, env):
  try: exp_closure = exp.closure
  except AttributeError: 
    if isinstance(exp, list) or isinstance(exp, tuple): 
      return tuple(closure(e, env) for e in exp) 
    else: return exp
  return exp_closure(env)

var_index = 1
class Var:
  def __init__(self, name, index=0): 
    self.name = name
    self.index = index
    
  def __call__(self, *exps): return Apply(self, *exps)
  
  def apply(self, solver, *exps):
    return self.getvalue(solver.env).apply(solver, *exps)
  
  def match(self, other): return True
        
  def unify(self, other, env, occurs_check=False):
    self = self.deref(env)
    other = deref(other, env)
    if isinstance(self, Var):
      if isinstance(other, Var) and other is self: 
        yield True
        return
      if occurs_check and contain_var(other, self):return
      self.setvalue(other, env)
      yield True
      try: del env.bindings[self] # for DummyVar
      except: pass
    elif isinstance(other, Var):
      if occurs_check and contain_var(self, other): return
      other.setvalue(self, env)
      yield True
      try: del env.bindings[other] # for DummyVar
      except: pass
    else:
      for result in unify(self, other, env, occurs_check):
        yield True
      
  def unify_rule_head(self, head, env, subst):
    subst[self] = copy_rule_head(head, env)
    yield True
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
    if result is not next: self.setvalue(result, env)
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

class DummyVar(Var):
  def __init__(self, name='_v', index=0): Var.__init__(self, name)
  def unify_rule_head(self, other, callee_env, caller_env, varset): 
    for x in self.unify(other, callee_env):
      yield varset | set([self])
  def deref(self, env): return self
  def getvalue(self, env):
    binding = env[self]
    if binding is self: return binding
    return getvalue(binding, env)
  def closure(self, env): return self
  def free(self, env): return True  
  def __eq__(self, other): return self.__class__ == other.__class__

def vars(names): return [Var(x.strip()) for x in names.split(',')]
def dummies(names): return [DummyVar(x.strip()) for x in names.split(',')]

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

class Apply:
  def __init__(self, operator, *operand):
    self.operator = operator
    self.operand = operand

  def cont(self, cont, solver):
    @mycont(cont)
    def evaluate_cont(op, solver): 
      return op.evaluate_cont(self.operand, cont, solver)
    return solver.cont(self.operator, evaluate_cont)
  def ___parse___(self, parser):
    self.operator = parser.parse(self.operator)
    self.operand = parser.parse(self.operand)
    return self

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
    return self.__class__==other.__class__ and self.operator==other.operator and self.operand==other.operand
  
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
