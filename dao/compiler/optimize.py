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
  
  if isinstance(exp, il.Var):
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
  
  if isinstance(exp, il.Var):
    try: return bindings[exp]
    except: return exp
    
  elif isinstance(exp, il.BinaryOperation):
    return exp

  elif  isinstance(exp, il.Apply):
    return il.Apply(subst(exp.caller, bindings), 
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
  if isinstance(exp, il.Lamda):
    return il.Lamda(exp.params, *optimize(exp.body, data))
  
  if isinstance(exp, il.Var):
    return exp
  
  elif  isinstance(exp, il.Apply):
    #1. ((lambda () body))  =>  body 
    if isinstance(exp.caller, il.Lamda):
      for p in exp.caller.params:
        if data.ref_count.get(p, 0)!=0:
          break
      else:
        return optimize(exp.caller.body, data)
      
      #2. (lamda x: ...x...)(y) => (lambda : ... y ...)() 
      bindings = {}
      new_params, new_args = (), ()
      for i, p in enumerate(exp.caller.params):
        if data.ref_count.get(p, 0)==1:
          bindings[p] = exp.args[i]
        else:
          ref_count = data.ref_count.get(p, 0)
          arg = exp.args[i]
          if code_size(arg)*ref_count<MAX_EXTEND_CODE_SIZE and not side_effects(arg) :
            new_params += (p,)
            new_args += (arg,)
      if bindings:
        if new_params:
          return il.Apply(il.Lamda(new_params, optimize(subst(exp.caller.body, bindings), data)), 
                          optimize(new_args, data))
        else: 
          return optimize(subst(exp.caller.body, bindings), data)
      else:
        return il.Lamda(optimize(exp.caller, data), optimize(exp.args, data))
    else: return il.Lamda(exp.caller, optimize(exp.args, data))
    # a(...y...), and a is (lamda ...x...: ...x...), then convert as above if code size is ok. 
      
  elif  isinstance(exp, il.Return):
    return il.Return(*optimize(exp.args, data))
  
  elif  isinstance(exp, il.StatementList):
    return il.StatementList(tuple(optimize(x, data) for x in exp.statements))
      
  elif  isinstance(exp, il.Assign):
    return exp
    #converted_var = optimize(exp.var)
    #env.lefts.add(exp.var)
    #return il.Assign(converted_var, optimize(exp.exp, data))
  
  elif  isinstance(exp, il.If):
    if isinstance(result.then, il.If): # (if a (if a b c) d)
      if result.then.test==result.test:
        result = il.If(result.test, result.then.then, result.else_)
    if isinstance(result.else_, il.If): # (if a b (if a c d))
      if result.else_.test==result.test:
        result = il.If(result.test, result.then, result.eles_.else_)
    result = il.If(optimize(exp.test, data), 
                optimize(exp.then, data), optimize(exp.else_, data))
    if isinstance(result.test, il.Let):
      result = il.Let(result.bindings, il.If(il.StatementList(let.body), 
                                             result.then, result.else_))
    return result
  
  elif  isinstance(exp, il.If2):
    return il.If2(optimize(exp.test, data), optimize(exp.then, data))
  
  elif isinstance(exp, il.Unify):
    return il.Unify(optimize(exp.left, data), optimize(exp.right, data),
                 optimize(exp.cont, data), optimize(exp.fcont, data))
  
  elif isinstance(exp, tuple):
    return tuple(optimize(x, data) for x in exp)
  
  elif isinstance(exp, list):
    return exp
  
  elif isinstance(exp, int) or isinstance(exp, float) or\
       isinstance(exp, str) or isinstance(exp, unicode):
    return exp
  
  elif exp is None:
    return None  
    
  else: raise CompileTypeError(exp)
  
# eliminating free variables(Lambda lifting or closure conversion)
# http://en.wikipedia.org/wiki/Closure_conversion

#Beta Conversion

#-Conversion primarily consists of the process of substituting a bound variable in the body of a lambda abstraction 
# by the argument passed to the function whenever it is applied. This process is called -reduction.
#In the context of functional programming languages, inline expansion is usually followed by the beta-reduction transformation.
def beta(exp):
  pass
