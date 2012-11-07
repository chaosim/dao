# -*- coding: utf-8 -*-

pytype = type

# depend dao.solve only.
from dao.solve import BaseCommand, mycont, value_cont

from dao.base import deref, getvalue, copy, copy_rule_head
from dao.base import apply_generators, unify, unify_list, match
from dao.base import closure

from dao import special
from dao import vop

#from dao.cont import ValueCont
#from dao import vop
#from dao import type

# ==============================================

# important function's definitions
# unify, deref, getvalue, match, closure, unify_rule_head
# copy, copy_rule_head, signature

def unify_rule_head(value, head, solver, subst):
  if isinstance(head, Var): 
    env = solver.env
    if isinstance(value, Var):
      subst[value] = head.copy_rule_head(env)
      return True
    else:
      try: 
        old = env.bindings[head]
        env.bindings[head] = value
        old_fcont = solver.fcont
        @mycont(old_fcont)
        def fcont(value, solver):
          env.bindings[var] = old
          solver.scont = old_fcont
        solver.fcont = fcont
        return True
      except:
        env.bindings[head] = value
        old_fcont = solver.fcont
        @mycont(old_fcont)
        def fcont(value, solver):
          try:  # what's wrong here? why does KeyError raise?
            del env.bindings[var]
          except: pass
          solver.scont = old_fcont
        solver.fcont = fcont
        return True
  else:
    try: 
      return value.unify_rule_head(head, solver, subst)
    except AttributeError: 
      if (isinstance(head, list) or isinstance(head, tuple)) \
          and (isinstance(value, list) or isinstance(value, tuple)):
        if len(value)!=len(head): return False
        return unify_list_rule_head(value, head, solver, subst) 
      elif value==head: return True

def unify_list_rule_head(values, args, solver, subst):
  # don't need to check the equality of the length of arguments
  # has been done in rules.apply for finding rule list
  for x, y in zip(values, args):
    if not unify_rule_head(x, y, solver, subst): return False
  return True
      
def signature(x):
  if isinstance(x, Var): return Var
  elif isinstance(x, list) or isinstance(x, tuple): 
    return (False, (tuple, len(x)))
  elif isinstance(x, Cons): 
    return (False, (Cons, len(x)))
  else:
    try: 
      hash(x)
      return x
    except: return id(x)

def rule_head_signatures(head):
  return tuple((i,signature(x)) for i, x in enumerate(head)) 

# =================================================================
# low level classes used in dao.
# Var, ClosureVar, DummyVar, Cons, Nil and nil
# Command, CommandCall, Function, Macro

from dao.solve import to_sexpression

# below is for dinpy.
from dao.solve import run_mode, interactive
from dao.solve import interactive_solver, interactive_tagger, interactive_parser

class CommandCall(BaseCommand):
  def __init__(self, operator, *operand):
    self.operator = operator
    self.operand = operand
    #if not isinstance(operator, Var):
      #self.is_global = operator.is_global
    #else: self.is_global = False
    
  def __call__(self, *args):
    return CommandCall(self, *args)
  
  def to_sexpression(self):
    return (to_sexpression(self.operator),)+tuple(to_sexpression(x) for x in self.operand)
    
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
  
  def __repr__(self): 
    if run_mode() is interactive:
      code = interactive_parser().parse(self)
      code = interactive_tagger().tag_loop_label(code)
      code = to_sexpression(code)
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
  
  def __eq__(self, other): 
    return isinstance(other, self.__class__) and self.operator==other.operator and self.operand==other.operand

def hash_parse_state(parse_state):
  try: return parse_state[1]
  except: return id(parse_state)


class Symbol: pass

_varcache = {}
def var(name):
  return _varcache.setdefault(name, Var(name))

class Var(BaseCommand):
  def __init__(self, name): 
    self.name = name
    
  def __call__(self, *args):
    return CommandCall(self, *args)
  
  def apply(self, solver, *exps):
    return self.getvalue(solver.env, {}).apply(solver, *exps)
  
  def match(self, other): return True
  
  def get_type(self, solver):
    return solver.env[self]
  
  def unify(self, other, solver, occurs_check=False):
    env = solver.env
    self = self.deref(env)
    other = deref(other, env)
    if isinstance(self, Var):
      if isinstance(other, Var) and other is self: 
        return True
      if occurs_check and contain_var(other, self): return False
      self.setvalue(other, env)
      old_fcont = solver.fcont
      @mycont(old_fcont)
      def fcont(value, solver):
        try: del env.bindings[self] # for DummyVar
        except: pass
        solver.scont = old_fcont
      solver.fcont = fcont
      return True
    elif isinstance(other, Var):
      if occurs_check and contain_var(self, other): return
      other.setvalue(self, env)
      old_fcont = solver.fcont
      @mycont(old_fcont)
      def fcont(value, solver):
        try: del env.bindings[other] # for DummyVar
        except: pass
        solver.scont = old_fcont
      solver.fcont = fcont
      return True
    else:
      return unify(self, other, solver, occurs_check)
      
  def unify_rule_head(self, head, solver, subst):
    subst[self] = copy_rule_head(head, solver.env)
    return True
  def copy_rule_head(self, env):
    try: return env.bindings[self]
    except KeyError:
      env.bindings[self] = RuleHeadCopyVar(self)
      return env.bindings[self]
    
  def deref(self, env):
    envValue = env[self]
    if envValue is self: return envValue
    if not isinstance(envValue, Var): return envValue
    return deref(envValue, env)
  
  def getvalue(self, env, memo):
    try: return memo[self]
    except:
      result = self.deref(env)
      if not isinstance(result, Var): 
        memo[self] = result
        result = getvalue(result, env, memo)
      memo[self ] = result
      return result
  
  def take_value(self, env):
    envValue = env[self]
    if not isinstance(envValue, Var): 
      result =  envValue
    else:
      next = env.bindings.get(envValue, None)
      if next is None: result =  envValue
      elif next is self: result =  next
      else: result = deref(next, env)
    if not isinstance(result, Var): 
      return take_value(result, env)
    return result
    
  def setvalue(self, value, env):
    env.bindings[self] = value

  def copy(self, memo):
    try: return memo[self]
    except KeyError:
      newvar = memo[self] = self.new()
      return newvar
    
  def free(self, env): return isinstance(self.deref(env), Var)
  
  def __repr__(self):  return '%s'%self.name
  def __eq__(self, other): return self is other
  def __hash__(self): return hash(id(self))
  
  def alpha(self, compiler):
    var = compiler.alpha_env[self]
    if var is self:
      var = compiler.new_var(self.name)
    return var
    
  def cont(self, cont, solver):
    return value_cont(self.getvalue(solver.env, {}), cont)
  

class RuleHeadCopyVar(Var):
  class_index = 1
  def __init__(self, var):
    self.name = var.name
    self.index = RuleHeadCopyVar.class_index
    RuleHeadCopyVar.class_index += 1
  def __repr__(self): return '$%s_%s'%(self.name, self.index)

_dummycache = {}
def dummy(name):
  return _dummycache.setdefault(name, DummyVar(name))

class DummyVar(Var):
  def __init__(self, name='_v'): Var.__init__(self, name)
  
  def unify_rule_head(self, head, env, subst):
    subst[self] = copy_rule_head(head, env)
    yield True
      
  def deref(self, env): return self
  
  def getvalue(self, env, memo):
    try: return memo[self]
    except:
      result = env[self]
      if result is not self: 
        result = getvalue(result, env, memo)
      memo[self] = result
      return result
  
  def take_value(self, env):
    binding = env[self]
    if binding is self: return binding
    return take_value(binding, env)
  
  def closure(self, env):
    return self
  
  def free(self, env): return True  
  
class NullVar(Var):
  def __init__(self): pass
  
  def unify_rule_head(self, head, env, subst):
    yield True
      
  def unify(self, other, solver, occurs_check=False):
    return True
    
  def deref(self, env): return self
  
  def getvalue(self, env, memo):
    return self
  
  def take_value(self, env):
    return self
  
  def closure(self, env):
    return self
  
  def free(self, env): return True  
  
  def __eq__(self, other): return True
  
  def __repr__(self):  return '__'
  
__null_var = NullVar()

def vars(names): return [var(x.strip()) for x in names.split(',')]
def dummies(names): return [dummy(x.strip()) for x in names.split(',')]
def nullvars(count): return [__null_var for x in range(count)]

class ClosureVar(Var):
  def __init__(self, var, value):
    self.var, self.value = var, value
    self.name = var.name
  
  def unify(self, other, solver, occurs_check=False):
    return unify(self.value, other, solver, occurs_check)

  def deref(self, env): return self
  
  def getvalue(self, env, memo):
    try: return memo[self]
    except:
      result = getvalue(self.value, env, memo)
      memo[self] = result
      return result
  def setvalue(self, value): self.var.setvalue(value)

  def copy(self, memo):
    try: return memo[self]
    except KeyError:
      newvar = memo[self] = self.__class__(self.name)
      return newvar
    
  def closure(self, env):
    value = self.var.getvalue(env, {})
    if value is self.var: return self.var
    else: return ClosureVar(self.var, value)
    
  def free(self, env): return isinstance(self.value, Var)
  
  def __repr__(self): return '(%s:%s)'%(self.var, self.value)
  def __eq__(self, other): return self.var is other

# ----------------------------------
# Cons, cons, Nil, nil, conslist, cons2tuple

class Cons: 
  def __init__(self, head, tail):
    self.head, self.tail = head, tail
    
  def unify(self, other, solver, occurs_check=False):
    if isinstance(other, Var): 
      return other.unify(self, solver, occurs_check)
    else:
      if self.__class__!=other.__class__: return False
      return unify(self.head, other.head, solver, occurs_check) and\
             unify(self.tail, other.tail, solver, occurs_check)

  def match(self, other):
    if self.__class__!=other.__class__: return False
    return match(self.head, other.head) and match(self.tail, other.tail)

  def unify_rule_head(self, other, solver, subst):
    if self.__class__!=other.__class__: return
    return unify_rule_head(self.head, other.head, solver, subst) and\
           unify_rule_head(self.tail, other.tail, solver, subst)
          
  def copy_rule_head(self, env):
    head = copy_rule_head(self.head, env)
    tail = copy_rule_head(self.tail, env)
    if head==self.head and tail==self.tail: return self
    return Cons(head, tail)

  def getvalue(self, env, memo):
    head = getvalue(self.head, env, memo)
    tail = getvalue(self.tail, env, memo)
    if head==self.head and tail==self.tail:
      return self
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
