# -*- coding: utf-8 -*-

def classeq(x, y):
  return x.__class__==y.__class__

def is_subclass(sub, sup):
  try: 
    if sup in sub.__bases__: return True
  except: return False
  for klass in sub.__bases__:
    if is_subclass(klass, sup): return True

def classname(obj):
  return obj.__class__.__name__.split('.')[-1]

def is_var(obj):
  try: obj.deref
  except: return False
  else: return True
  
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
  elif length==1:
    for _ in generators[0]:
      yield True
    return
  
  i = 0
  while i <length:
    try:
      generators[i].next()
      if i==length-1:
        yield True
        return
      else: i += 1
    except StopIteration:
      if i==0: return
      i -= 1

def unify_after_compile(x, y):
  if x==y: yield True
  
def unify(x, y, solver, occurs_check=False):
  try: x_unify = x.unify
  except AttributeError: 
    try: y_unify = y.unify 
    except AttributeError: 
      if (isinstance(x, list) or isinstance(x, tuple))\
          and (isinstance(y, list) or isinstance(y, tuple)):
        return unify_list(x, y, solver, occurs_check)
      else: return x==y
    return y_unify(x, solver, occurs_check)
  return x_unify(y, solver, occurs_check)

def unify_list(list1, list2, solver, occurs_check=False):
  '''unify list1 with list2 in solver.'''
  if len(list1)!=len(list2): return False
  for x, y in zip(list1, list2):
    if not unify(x, y, solver, occurs_check): return False 
  return True
    
def deref(x, env):
  try: x_deref = x.deref
  except AttributeError: 
    if isinstance(x, list): return [deref(e, env) for e in x]
    elif isinstance(x, tuple): return tuple(deref(e, env) for e in x)
    else: return x
  return x_deref(env)
  
def getvalue(x, env, memo):
  try: x_getvalue = x.getvalue
  except AttributeError: 
    if isinstance(x, list): return [getvalue(e, env, memo) for e in x]
    elif isinstance(x, tuple): return tuple(getvalue(e, env, memo) for e in x)
    else: return x
  return x_getvalue(env, memo)

# one shot generators with return result list
def apply_generators_list(generators): 
  length = len(generators)
  if length==0: 
    yield []
    return
  i = 0
  result = []
  while i <length:
    try:
      result.append(generators[i].next())
      if i==length-1: 
        yield result
        return
      else: i += 1
    except StopIteration:
      if i==0: return
      i -= 1
    except GeneratorExit: raise

def peek_value(exp, env):
  try: exp_take_value = exp.peek_value
  except AttributeError: 
    if isinstance(exp, list):
      return [peek_value(e) for e in exp]
    elif isinstance(exp, tuple): 
      return tuple(peek_value(e) for e in exp)
    else: return exp
  return exp_take_value(env)


def copy(exp, memo):
  try: exp_copy = exp.copy
  except AttributeError: 
    if isinstance(exp, list): return [getvalue(e, memo) for e in exp]
    elif isinstance(exp, tuple): return tuple(getvalue(e, memo) for e in exp)
    else: return exp
  return exp_copy(memo)

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

def apply_generator_fun_list(fun_args_list):
  i = 0
  length = len(fun_args_list)
  gen_list[0] = [fun_args_list[0][0](*fun_args_list[0][1:])]+[None]*(length-1)
  result = [None]*length
  while 1:
    try: 
      result[i] = gen_list[i].next()
      if i==length-1:
        yield result
      else:
        i += 1
        gen_list[i] = fun_args_list[i][0](*fun_args_list[i][1:])
    except StopIteration:
      if i==0: return
      else: i -= 1
      