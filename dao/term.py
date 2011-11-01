# -*- coding: utf-8 -*-

# depend dao.solve only.

from dao.solve import value_cont, mycont
from dao import base
from dao.base import unify, unify_list, copy_rule_head
from dao.base import deref, getvalue
from dao.base import copy, closure, match, match_list, apply_generators

# below is for dinpy.
from dao.solve import run_mode, interactive
from dao.solve import interactive_solver, interactive_tagger, interactive_parser

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
      
def signature(x):
  '''signature return a binary tuple, first value tell whether x is Var,
second value is a hashable value'''
  if isinstance(x, Var): return Var
  elif isinstance(x, list) or isinstance(x, tuple): 
    return (False, (tuple, len(x)))
  elif isinstance(x, Cons): 
    return (False, (Cons, len(x)))
  else:
    try: 
      hash(x)
      return x
    except: return type(x)

def rule_head_signatures(head):
  return tuple((i,signature(x)) for i, x in enumerate(head)) 

# =================================================================
# low level classes used in dao.
# Var, ClosureVar, DummyVar, Cons, Nil and nil
# Command, CommandCall, Function, Macro

class Command:
  ''' the base class for all the callable object in the dao system.'''
  def __call__(self, *args):
    return CommandCall(self, *args)
  def first_set(self, solver, memo, *args):
    self = getvalue(self, solver.env)    
    try: self_first_func = self.first_set_func
    except: return set([base.null]) # return solver.universal_set
    return self.first_set_func(self, solver, memo, *args)
  def __hash__(self): return id(self)
  def __eq__(self, other): return id(self)==id(other)

class Symbol: pass

class Var(Command):
  def __init__(self, name): 
    self.name = name
    
  def apply(self, solver, *exps):
    return self.getvalue(solver.env).apply(solver, *exps)
  
  def match(self, other): return True
        
  def unify(self, other, env, occurs_check=False):
    if self is other: 
      yield True
      return
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
      env.bindings[self] = RuleHeadCopyVar(self)
      return env.bindings[self]
    
  def deref(self, env):
    envValue = env[self]
    if not isinstance(envValue, Var): return envValue
    next = env.bindings.get(envValue, None)
    if next is None: return envValue
    if next is self: return next
    result = deref(next, env)
    if result is not envValue and not isinstance(envValue, RuleHeadCopyVar): 
      self.setvalue(result, env)
    return result
  def getvalue(self, env):
    result = self.deref(env)
    if not isinstance(result, Var): 
      result = getvalue(result, env)
    return result

  def setvalue(self, value, env):
    env.bindings[self] = value

  def copy(self, memo):
    try: return memo[self]
    except KeyError:
      newvar = memo[self] = self.new()
      return newvar
    
  def new(self): 
    return self.__class__(self.name)
  
  def free(self, env): return isinstance(self.deref(env), Var)
  
  def __repr__(self): return '%s'%self.name
  def __eq__(self, other): return self is other
  def __hash__(self): return hash(id(self))
  
  def closure(self, env):
    value = self.getvalue(env)
    if value is self: return self
    else: return ClosureVar(self, value)
  
  def cont(self, cont, solver):
    return value_cont(self.getvalue(solver.env), cont)
    
  def __add__(self, other): 
    from dao.builtins.arith import add
    return add(self, other)
  def __sub__(self, other): 
    from dao.builtins.arith import sub
    return sub(self, other)
  def __radd__(self, other): 
    from dao.builtins.arith import add
    return add(other, self)
  def __rsub__(self, other): 
    from dao.builtins.arith import sub
    return sub(other, self)

class RuleHeadCopyVar(Var):
  var2index = {}
  def __init__(self, var):
    self.name = var.name
    self.index = self.var2index.get(var, 0)
    self.var2index[var] = self.index+1
  def __repr__(self): return '$%s_%s'%(self.name, self.index)
  
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

class CommandCall(Command):
  def __init__(self, operator, *operand):
    self.operator = operator
    self.operand = operand
    try: self.first_set_func = operator.first_set_func
    except: pass
    
  def cont(self, cont, solver):
    @mycont(cont)
    def evaluate_cont(op, solver): 
      return op.evaluate_cont(self.operand, cont, solver)
    return solver.cont(self.operator, evaluate_cont)
  def ___parse___(self, parser):
    self.operator = parser.parse(self.operator)
    self.operand = parser.parse(self.operand)
    return self
  def tag_loop_label(self, tagger):
    self.operator = tagger.tag_loop_label(self.operator)
    self.operand = tagger.tag_loop_label(self.operand)
    return self

  def closure(self, env):
    return CommandCall(self.operator, *[closure(x, env) for x in self.operand])
  
  def first_set(self, solver, memo):
    return self.operator.first_set(solver, memo, *self.operand)
  
  def __repr__(self): 
    if run_mode() is interactive:
      code = interactive_parser().parse(self)
      code = interactive_tagger().tag_loop_label(code)
      result = interactive_solver().eval(code)
      return repr(result) if result is not None else ''
    return '%s(%s)'%(self.operator, 
                ','.join([repr(e) for e in self.operand]))
  def __and__(self, other):
    from dao.builtins.control import and_p
    return and_p(self, other)
  def __add__(self, other):
    from dao.special import begin
    return begin(self, other)
  def __or__(self, other):
    from dao.builtins.control import or_p
    return or_p(self, other)
  def __hash__(self): return id(self)
  def __eq__(self, other): 
    return isinstance(other, self.__class__) and self.operator==other.operator and self.operand==other.operand
  
class Function(Command): 
  def evaluate_cont(self, exps, cont, solver):
    def evaluate_arguments(exps, cont):
        if len(exps)==0: 
          return cont([], solver)
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
    return evaluate_arguments(exps, apply_cont)

class Macro(Command): 
  def evaluate_cont(self, exps, cont, solver):
    exps1 = [(closure(exp, solver.env)) for exp in exps]
    return self.apply(solver, exps1, cont)

# ----------------------------------
# Cons, cons, Nil, nil, conslist, cons2tuple

class Cons: 
  def __init__(self, head, tail):
    self.head, self.tail = head, tail
    
  def unify(self, other, env, occurs_check=False):
    if isinstance(other, Var): 
      for x in other.unify(self, env, occurs_check):
        yield True
    else:
      if not isinstance(other, Cons): return
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
  def __len__(self): return 0
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
