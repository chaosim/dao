# -*- coding: utf-8 -*-

class UniversalSet:
  def __and__(self, other):
    if not isinstance(other, UniversalSetWithNull): return other
    else: return self
  def __rand__(self, other):
    if not isinstance(other, UniversalSetWithNull): return other
    else: return self
  def __or__(self, other):
    if isinstance(other, UniversalSetWithNull): return other
    else: return self
  def __ror__(self, other):
    if isinstance(other, UniversalSetWithNull): return other
    else: return self
  def __sub__(self, other):
    return ReverseSet(other)
  def __contains__(self, other):
    if other is null: return False
    return True
  
uniset = UniversalSet()

class UniversalSetWithNull:
  def __and__(self, other):
    return other
  def __rand__(self, other):
    return other
  def __or__(self, other):
    return self
  def __ror__(self, other):
    return self
  def __sub__(self, other):
    return ReverseSet(other)
  def __contains__(self, other):
    return True

unisetnull = UniversalSetWithNull()

class ReverseSet:
  def __init__(self, base_set):
    self.base_set = base_set
  def __and__(self, other):
    if isinstance(other, UniversalSet):
      return self
    elif isinstance(other, ReverseSet):
      return ReverseSet(self.base|other.base)    
    return other
  def __or__(self, other):
    if isinstance(other, UniversalSet):
      return other
    elif isinstance(other, ReverseSet):
      return ReverseSet(self.base&other.base)    
    return other
  def __ror__(self, other):
    if isinstance(other, UniversalSet):
      return other
    elif isinstance(other, ReverseSet):
      return ReverseSet(self.base&other.base)    
    return other
  def __contains__(self, other):
    return False if other in self.base_set else True

class Eoi: pass

eoi = Eoi()

class Null: pass

null = Null()

nullset = set([null])

def next_element(parse_state):
  try: 
    if parse_state[1]>=len(parse_state[0]): return eoi
    else: return parse_state[0][parse_state[1]]
  except:
    try: parse_state_next_element = parse_state.next_element
    except: return parse_state
    return parse_state_next_element()
  
#------------------------------------------
# for preprocess before Solver.solve

class Parser: 
  def parse(self, exp):
    try: exp_parse = exp.___parse___
    except: 
      if isinstance(exp, list):
        return [self.parse(e) for e in exp]
      elif isinstance(exp, tuple):
        return tuple(self.parse(e) for e in exp)
      else: return exp
    try: return exp_parse(self)
    except TypeError: return exp

def preparse(exp): 
  return Parser().parse(exp)

class LoopExitNextTagger:
  ''' use tagger to preprocess before solve expression'''
  surfix = '$'
  def __init__(self): 
    self.new_label_id = 1
    self.labels = {}
  def make_label(self, label):
    if label is None: 
      label = '$'+str(self.new_label_id)
      self.new_label_id += 1
    return label
  def push_label(self, control_struct_type, label):
    self.labels.setdefault(control_struct_type, []).append(label)
    self.labels.setdefault(None,[]).append(label)
  def pop_label(self, control_struct_type):
    self.labels[control_struct_type].pop()
    self.labels[None].pop()

  def tag_loop_label(self, exp):
    try: exp_tag_loop_label = exp.tag_loop_label
    except: 
      if isinstance(exp, list):
        return [self.tag_loop_label(e) for e in exp]
      elif isinstance(exp, tuple):
        return tuple(self.tag_loop_label(e) for e in exp)
      else: return exp
    try: return exp_tag_loop_label(self)
    except TypeError: return exp

def tag_loop_label(exp): 
  return LoopExitNextTagger().tag_loop_label(exp)

def dao_repr(exp):
    try: exp_____repr____ = exp.____repr____
    except: 
      if isinstance(exp, list) or isinstance(exp, tuple):
        return ','.join([dao_repr(e) for e in exp])
      else: return repr(exp)
    try: return exp_____repr____()
    except TypeError: return repr(exp)

# ------------------------------------------------------------

# important function's definitions for unification and match:
# deref, getvalue, closure, match, unify
# copy, copy_rule_head

# one shot generators, such as unify, set/restore
def apply_generators(generators): 
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

# implemented by using apply_generators
def unify_list(list1, list2, env, occurs_check=False):
  '''unify list1 with list2 in env.'''
  
  if len(list1)!=len(list2): return
  if len(list1)==0: yield True
  if len(list1)==1: 
    for _ in unify(list1[0], list2[0], env, occurs_check):
      yield True
  for _ in apply_generators(tuple(unify(x, y, env, occurs_check) 
                            for x, y in zip(list1, list2))):
    yield True
  
def unify(x, y, env, occurs_check=False):
  try: x_unify = x.unify
  except AttributeError: 
    try: y_unify = y.unify 
    except AttributeError: 
      if (isinstance(x, list) or isinstance(x, tuple))\
          and (isinstance(y, list) or isinstance(y, tuple)):
        for _ in unify_list(x, y, env, occurs_check):
          yield True
      elif x==y: yield True
      return
    for _ in y_unify(x, env, occurs_check):
      yield True
    return
  for _ in x_unify(y, env, occurs_check):
    yield True

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
  try: x_match = x.match
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

