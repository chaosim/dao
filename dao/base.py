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

