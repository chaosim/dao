# -*- coding: utf-8 -*-

# depend dao.solve only.

from dao.solve import value_cont, mycont, BaseCommand

# below is for dinpy.
from dao.solve import run_mode, interactive
from dao.solve import interactive_solver, interactive_tagger, interactive_parser

from dao.base import deref, getvalue, copy, copy_rule_head
from dao.base import apply_generators, unify, unify_list, match
from dao.base import closure

# ==============================================

# important function's definitions
# unify, deref, getvalue, match, closure, unify_rule_head
# copy, copy_rule_head, signature

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

def unify_list_rule_head(values, args, env, subst):
  # don't need to check the equality of the length of arguments
  # has been done in rules.apply for finding rule list
  
  for _ in apply_generators(tuple(unify_rule_head(x, y, env, subst) 
                            for x, y in zip(values, args))):
    yield True
      
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
    except: return id(x)

def rule_head_signatures(head):
  return tuple((i,signature(x)) for i, x in enumerate(head)) 

# =================================================================
# low level classes used in dao.
# Var, ClosureVar, DummyVar, Cons, Nil and nil
# Command, CommandCall, Function, Macro

def hash_parse_state(parse_state):
  try: return parse_state[1]
  except: return id(parse_state)

class Command(BaseCommand):
  ''' the base class for all the callable object in the dao system.'''
  memorable = False
  
  def __call__(self, *args):
    return CommandCall(self, *args)
  
  def run(self, solver, cont, values):
    signatures = rule_head_signatures(values)
    
    if solver.parse_state is None or not self.memorable:
      for c, v in self.apply(solver, cont, values, signatures):
        yield c, v
      return
      
    sign_state  = ((self, signatures), hash_parse_state(solver.parse_state))
    sign_state2cont = solver.sign_state2cont.setdefault(sign_state, [])
    
    # TODO: greedy, nongreedy, lazy mode
    # lazy : reverse the order of sign_state2cont
    memo = False
    i = 0
    for path, c in sign_state2cont:
      if cont==sign_state2cont: 
        memo = True
        break
      if len(solver.call_path)<=path: # lazy: >
        i += 1
      continue
      for x, y in zip(path, solver.call_path):
        if x!=y: break
      else: break 
      i += 1
    if not memo:
        sign_state2cont.insert(i, (solver.call_path, cont))
    
    memo_results = solver.sign_state2results.get(sign_state)
    env = solver.env
    if memo_results is not None:
      for _, c in sign_state2cont:
        if c.cont_order>cont.cont_order: continue
        for result_head, reached_parse_state, value in memo_results:
          solver.env = env.extend()
          for _ in unify(values, result_head, solver.env):
            solver.parse_state = reached_parse_state
            yield c, value
      solver.env = env 
      
    @mycont(cont)
    def memo_result_cont(value, solver):
      result_head = getvalue(values, solver.env, {})
      result = result_head, solver.parse_state, value
      solver.sign_state2results.setdefault(sign_state, []).append(result)
      # TODO: prevent backtracking for greedy
      for _, c in sign_state2cont:
        yield c, value
        
    if len(sign_state2cont)==1: 
      for c, v in self.apply(solver, memo_result_cont, values, signatures):
        yield c, v

class Symbol: pass

_varcache = {}
def var(name):
  return _varcache.setdefault(name, Var(name))

class Var(Command):
  def __init__(self, name): 
    self.name = name
    
  def apply(self, solver, *exps):
    return self.getvalue(solver.env, {}).apply(solver, *exps)
  
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
    
  def new(self): 
    return self.__class__(self.name)
  
  def free(self, env): return isinstance(self.deref(env), Var)
  
  def __repr__(self): return '%s'%self.name
  def __eq__(self, other): return self is other
  def __hash__(self): return hash(id(self))
  
  def closure(self, env):
    value = self.getvalue(env, {})
    if value is self: return self
    else: return ClosureVar(self, value)
  
  def cont(self, cont, solver):
    return value_cont(self.getvalue(solver.env, {}), cont)
    
  def __add__(self, other): 
    from dao.builtins.arith import add
    return (add, self, other)
  def __sub__(self, other): 
    from dao.builtins.arith import sub
    return (sub, self, other)
  def __radd__(self, other): 
    from dao.builtins.arith import add
    return (add, other, self)
  def __rsub__(self, other): 
    from dao.builtins.arith import sub
    return (sub, other, self)

class RuleHeadCopyVar(Var):
  var2index = {}
  def __init__(self, var):
    self.name = var.name
    self.index = self.var2index.get(var, 0)
    self.var2index[var] = self.index+1
  def __repr__(self): return '$%s_%s'%(self.name, self.index)

_dummycache = {}
def dummy(name):
  return _dummycache.setdefault(name, DummyVar(name))

class DummyVar(Var):
  def __init__(self, name='_v', index=0): Var.__init__(self, name)
  
  def unify_rule_head(self, other, callee_env, caller_env, varset): 
    for x in self.unify(other, callee_env):
      yield varset | set([self])
      
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
  
  def closure(self, env): return self
  
  def free(self, env): return True  
  def __eq__(self, other): return self.__class__ == other.__class__

def vars(names): return [var(x.strip()) for x in names.split(',')]
def dummies(names): return [dummy(x.strip()) for x in names.split(',')]

class ClosureVar(Var):
  def __init__(self, var, value):
    self.var, self.value = var, value
    self.name = var.name
  
  def unify(self, other, env, occurs_check=False):
    return unify(self.value, other, env, occurs_check)

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

from dao.solve import to_sexpression

class CommandCall(Command):
  def __init__(self, operator, *operand):
    self.operator = operator
    self.operand = operand
    if not isinstance(operator, Var):
      self.is_global = operator.is_global
    else: self.is_global = False
    
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
  
def evaluate_arguments(solver, cont, exps):
  if len(exps)==0: 
    yield cont, ()
  else:
    @mycont(cont)
    def argument_cont(value, solver):
      @mycont(cont)
      def gather_cont(values, solver):
          for c, v in cont((value,)+values, solver): 
            yield c, v
      return evaluate_arguments(solver, gather_cont, exps[1:])
    yield solver.cont(exps[0], argument_cont), True
      
class Function(Command):
  memorable = True
  
  def evaluate_cont(self, solver, cont, exps):
    @mycont(cont)
    def apply_cont(values, solver): 
      return self.run(solver, cont, values)
    return evaluate_arguments(solver, apply_cont, exps)

class Macro(Command): 
  
  memorable = True
  
  def evaluate_cont(self, solver, cont, exps):
    exps1 = [(closure(exp, solver.env)) for exp in exps]
    return self.run(solver, cont, exps1)

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

