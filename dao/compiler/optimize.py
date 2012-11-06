# -*- coding: utf-8 -*-
''' code for optimization:
 (lambda (): body)() => body
 (lambda (...): body)(args) => body # if paramaters is not used in body
 (lambda (params): body)(args) => (lambda (params') body')(args') # inline args which are used only once.
'''

import dao.compiler.interlang as il
from dao.compiler.compile import CompileTypeError

class OptimizationData:
  def __init__(self):
    self.ref_count = {}
    self.called_count = {}
    self.occur_count = {}
    
  def __repr__(self):
    return repr(self.ref_count)

def analyse_before_optimize(exp, data):
  if isinstance(exp, il.Lamda):
    try: exp.seen
    except:
      exp.seen = True
      data.occur_count[exp] = data.occur_count.setdefault(exp, 0)+1
      for x in exp.body:
        analyse_before_optimize(x, data)
  
  elif isinstance(exp, il.Clamda):
    data.occur_count[exp] = data.occur_count.setdefault(exp, 0)+1
    for x in exp.body:
      analyse_before_optimize(x, data)
  
  elif isinstance(exp, il.Function):
    data.occur_count[exp] = data.occur_count.setdefault(exp, 0)+1
    for x in exp.body:
      analyse_before_optimize(x, data)
  
  elif isinstance(exp, il.CFunction):
    data.occur_count[exp] = data.occur_count.setdefault(exp, 0)+1
    for x in exp.body:
      analyse_before_optimize(x, data)
  
  elif isinstance(exp, il.Var):
    data.ref_count[exp] = data.ref_count.setdefault(exp, 0)+1
  
  elif isinstance(exp, il.BinaryOperation):
    return exp

  elif  isinstance(exp, il.Apply):
    data.called_count[exp.caller] = data.called_count.setdefault(exp.caller, 0)+1
    analyse_before_optimize(exp.caller, data)
    for arg in exp.args:
      analyse_before_optimize(arg, data)
  
  elif  isinstance(exp, il.Return):
    for arg in exp.args:
      analyse_before_optimize(arg, data)
  
  elif  isinstance(exp, il.Assign):
    analyse_before_optimize(exp.exp, data)
  
  elif  isinstance(exp, il.If):
    analyse_before_optimize(exp.test, data)
    analyse_before_optimize(exp.then, data)
    analyse_before_optimize(exp.else_, data)
  
  elif  isinstance(exp, il.If2):
    analyse_before_optimize(exp.test, data)
    analyse_before_optimize(exp.then, data)
  
  elif isinstance(exp, il.Unify):
    analyse_before_optimize(exp.left, data)
    analyse_before_optimize(exp.right, data)
    analyse_before_optimize(exp.cont, data)
    analyse_before_optimize(exp.fcont, data)

  elif isinstance(exp, tuple):
    for x in exp:
      analyse_before_optimize(x, data)
  
  elif isinstance(exp, list):
    return
  
  elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
    return
    
  elif exp is None:
    return
  
  else: raise CompileTypeError(exp)
  
MAX_EXTEND_CODE_SIZE = 10

def code_size(exp):
  if isinstance(exp, il.Lamda):
    return code_size(exp.body)+len(exp.params)+2
  
  if isinstance(exp, il.Var):
    return 1
    
  elif  isinstance(exp, il.Apply):
    return code_size(exp.caller)+sum([code_size(x) for x in exp.args])
        
  elif  isinstance(exp, il.Return):
    return sum([code_size(x) for x in exp.args])
  
  elif  isinstance(exp, il.Assign): # var = value, exp.exp should be a single var, 
                                    # which is the continuation param which ref to the value
    return code_size(exp.value)+2
  
  elif  isinstance(exp, il.If):
    return 3 + code_size(exp.test) + code_size(exp.then_) + code_size(exp.else_)
  
  elif isinstance(exp, il.Unify):
    return code_size(exp.left) + code_size(exp.right) + code_size(exp.cont) + code_size(exp.fcont)
  
  elif isinstance(exp, list):
    return sum([code_size(x) for x in exp])
  
  elif isinstance(exp, tuple):
    return sum([code_size(x) for x in exp])
  
  elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
    return 1
  
  elif exp is None:
    return 1
  
  else: raise CompileTypeError(exp)

def side_effects(exp):
  if isinstance(exp, il.Lamda):
    return False
  
  elif isinstance(exp, il.Var):
    return False
    
  elif  isinstance(exp, il.Apply):
    if isinstance(exp.caller, il.Lamda):
      if lambda_side_effects(exp.caller): return True
    elif isinstance(exp.caller, il.Var): return True
    elif exp.caller.have_side_effects: return True
    else: return False # after cps, all of value have been solved before called, 
                       # so have no side effects.
        
  elif  isinstance(exp, il.Return):
    return False
  
  elif  isinstance(exp, il.Assign): # var = value, exp.exp should be a single var, 
                                    # which is the continuation param which ref to the value
    return True
  
  elif  isinstance(exp, il.If):
    return not side_effects(exp.test) and\
           not side_effects(exp.then_) and\
           not side_effects(exp.else_)
  
  elif isinstance(exp, il.Unify):
    return False
  
  elif isinstance(exp, list):
    return False
  
  elif isinstance(exp, tuple):
    return False
  
  elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
    return False
  
  elif exp is None:
    return None
  
  else: raise CompileTypeError(exp)

def lambda_side_effects(exp):
  return side_effects(exp.body)

def subst(exp, bindings):
  if isinstance(exp, il.Lamda):
    return il.Lamda(exp.params, *subst(exp.body, bindings))
  
  elif isinstance(exp, il.Var):
    try: return bindings[exp]
    except: return exp
    
  elif isinstance(exp, il.Assign):
    return il.Assign(exp.var, subst(exp.exp, bindings))
      
  elif isinstance(exp, il.StatementList):
    return il.StatementList(tuple(subst(x, bindings) for x in exp.statements))
      
  elif isinstance(exp, il.BinaryOperation):
    return exp

  elif  isinstance(exp, il.Apply):
    return exp.__class__(subst(exp.caller, bindings), 
                 tuple(subst(arg, bindings) for arg in exp.args))
  
  elif  isinstance(exp, il.Return):
    return il.Return(*tuple(subst(arg, bindings) for arg in exp.args))
  
  #elif  isinstance(exp, il.Assign): # var = value, exp.exp should be a single var, 
                                    ## which is the continuation param which ref to the value
    #return il.set_contents(exp.var, exp.exp)
  
  elif  isinstance(exp, il.If):
    return il.If(subst(exp.test, bindings), 
                 subst(exp.then, bindings), subst(exp.else_, bindings))
  
  elif  isinstance(exp, il.If2):
    return il.If2(subst(exp.test, bindings), subst(exp.then, bindings))
  
  elif isinstance(exp, il.Unify):
    return il.Unify(subst(exp.left, bindings), subst(exp.right, bindings),
                 subst(exp.cont, bindings), subst(exp.fcont, bindings))
  
  elif isinstance(exp, list):
    return [subst(x, bindings) for x in exp]
  
  elif isinstance(exp, tuple):
    return tuple(subst(x, bindings) for x in exp)
  
  elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
    return exp
  
  elif exp is None:
    return None
  
  else: raise CompileTypeError(exp)

def optimize(exp, data):
  changed = True
  while changed:
    exp, changed = optimize_once(exp, data)
  return exp

def optimize_once(exp, data):
  if isinstance(exp, il.Lamda):
    body, changed = optimize_once(exp.body, data)
    return il.Lamda(exp.params, *body), changed
  
  elif isinstance(exp, il.Var):
    return exp, False
  
  elif  isinstance(exp, il.Apply):
    
    if isinstance(exp.caller, il.Lamda):
      #1. ((lambda () body))  =>  body 
      if len(exp.caller.params)==0: 
        return optimize(il.statements(exp.caller.body), data), True
      
      #2. (lamda x: ...x...)(y) => (lambda : ... y ...)() 
      bindings = {}
      args = exp.args
      new_params, new_args = (), ()
      for i, p in enumerate(exp.caller.params):
        arg = args[i]
        if side_effects(arg):
          new_params += (p,)
          new_args += (arg,)
          continue
        else:
          ref_count = data.ref_count.get(p, 0)
          if ref_count==0:
            continue
          elif ref_count==1:
            bindings[p] = arg
          else:
            if code_size(arg)*ref_count>MAX_EXTEND_CODE_SIZE: 
              # a(...y...), and a is (lamda ...x...: ...x...), 
              #then convert as above if code size is ok. 
              new_params += (p,)
              new_args += (arg,)
            else: 
              bindings[p] = arg
      
      if new_params:
        if bindings:
          return il.Apply(il.Lamda(new_params, optimize(subst(exp.caller.body, bindings), data)), 
                          optimize(new_args, data)), True
        else:
          if len(new_params)!=len(exp.caller.params):
            il.Apply(il.Lamda(new_params, optimize(exp.caller.body, data)), optimize(new_args, data)), True            
          else:
            caller_body, changed1 = optimize_once(exp.caller.body, data)
            args, changed2 = optimize_once(new_args, data)
            return il.Apply(il.Lamda(new_params, caller_body), args), changed1 or changed2
      else:
        if bindings:
          return optimize(subst(il.statements(exp.caller.body), bindings), data), True
        else:
          return optimize(il.statements(exp.caller.body), data), True               
    else: 
      changed = False
      caller, changed1 = optimize_once(exp.caller, data)
      args, changed2 = optimize_once(exp.args, data)
      return exp.__class__(caller, args), changed1 or changed2
      
  elif  isinstance(exp, il.Return):
    if len(exp.args)==1 and isinstance(exp.args[0], il.Return):
      args = exp.args[0].args
    else:
      for arg in exp.args: 
        if isinstance(arg, il.Return): 
          raise CompileError
      args = exp.args
    changed = False
    result = []
    for x in args:
      x, x_changed = optimize_once(x, data)
      result.append(x)
      changed = changed or x_changed
    return il.Return(*result), changed
  
  elif  isinstance(exp, il.StatementList):
    changed = False
    result = []
    for x in exp.statements:
      x, x_changed = optimize_once(x, data)
      result.append(x)
      changed = changed or x_changed
    return il.statements(tuple(result)), changed
      
  elif  isinstance(exp, il.Assign):
    return exp, False

  elif isinstance(exp, il.BinaryOperation):
    return exp, False
  
  elif  isinstance(exp, il.If):
    changed = False
    result = exp
    if isinstance(result.then, il.If): # (if a (if a b c) d)
      if result.then.test==result.test:
        result = il.If(result.test, result.then.then, result.else_)
        changed = True
    if isinstance(result.else_, il.If): # (if a b (if a c d))
      if result.else_.test==result.test:
        result = il.If(result.test, result.then, result.else_.else_)
        changed = True
    test, test_changed = optimize_once(exp.test, data)
    then, then_changed = optimize_once(exp.then, data)
    else_, else__changed = optimize_once(exp.else_, data)
    result = il.If(test, then, else_)
    #if isinstance(result.test, il.Let):
      #result = il.Let(result.bindings, il.If(il.StatementList(let.body), 
                                             #result.then, result.else_))
    return result, changed or test_changed or then_changed or else__changed
  
  elif  isinstance(exp, il.If2):
    return il.If2(optimize(exp.test, data), optimize(exp.then, data))
  
  elif isinstance(exp, il.Unify):
    left, left_changed = optimize_once(exp.left, data)
    right, right_changed = optimize_once(exp.right, data)
    cont, cont_changed = optimize_once(exp.cont, data)
    fcont, fcont_changed = optimize_once(exp.fcont, data)
    return il.Unify(left, right, cont, fcont), left_changed or right_changed or cont_changed or fcont_changed
  
  elif isinstance(exp, tuple):
    changed = False
    result = []
    for x in exp:
      x, x_changed = optimize_once(x, data)
      result.append(x)
      changed = changed or x_changed
    return tuple(result), changed
  
  elif isinstance(exp, list):
    return exp, False
  
  elif isinstance(exp, int) or isinstance(exp, float) or\
       isinstance(exp, str) or isinstance(exp, unicode):
    return exp, False
  
  elif exp is None:
    return None, False  
    
  else: raise CompileTypeError(exp)
  
# eliminating free variables(Lambda lifting or closure conversion)
# http://en.wikipedia.org/wiki/Closure_conversion

#Beta Conversion

#-Conversion primarily consists of the process of substituting a bound variable in the body of a lambda abstraction 
# by the argument passed to the function whenever it is applied. This process is called -reduction.
#In the context of functional programming languages, inline expansion is usually followed by the beta-reduction transformation.
def beta(exp):
  pass
