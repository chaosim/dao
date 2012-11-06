# -*- coding: utf-8 -*-

import dao.compiler.interlang as il

class CompileTypeError: 
  def __init__(self, exp):
    self.exp = exp
    
  def __repr__(self):
    return '%s'%repr(self.exp)

class VariableNotBound: 
  def __init__(self, var):
    self.var = var
    
  def __repr__(self):
    return '%s'%repr(self.var)

v, fc = il.Var('v'), il.Var('fc')

class Compiler:
  def __init__(self):
    pass
  
  def cps(self, exp, cont, fcont):
    self.cut_rules_cont = [fcont]
    self.cut_or_fcont = [fcont]
    return self.cps_convert(exp, cont, fcont)
    
  def cps_convert(self, exp, cont, fcont):
    try: 
      exp_cps_convert = exp.cps_convert
      
    except: 
      if isinstance(exp, il.Lamda):
        k = Var('k')
        return cont(il.Lamda((k,)+exp.params, self.cps_convert_exps(exp.body, k, fcont)), fcont)
      elif isinstance(exp, il.Apply):
        # see The 90 minute Scheme to C compiler by Marc Feeley
        if isinstance(exp.caller, il.Lamda):
          args = exp.args
          fun = self.cps_convert_exps(exp.caller.body, cont, fcont)
          for var, arg in reversed(zip(exp.caller.params, args)):
            fun = self.cps_convert(arg, il.Clamda(var, fc, fun), fcont)
          return fun
        else:
          function = il.Var('function')
          vars = tuple(il.Var('a'+repr(i)) for i in range(len(exp.args)))
          fun = il.Apply(function, (cont,)+vars)
          for var, exp in reversed(zip((function,)+vars, (exp.caller,)+exp.args)):
            fun = self.cps_convert(exp, il.Clamda(var, fc, fun), fcont)
          return fun
        
      elif isinstance(exp, il.Var):
        return cont(exp, fcont)
      
      elif isinstance(exp, tuple) or  isinstance(exp, list) or\
         isinstance(exp, int) or isinstance(exp, float) or\
         isinstance(exp, str) or isinstance(exp, unicode):
        return cont(exp, fcont)
      else: raise CompileTypeError(exp)
      
    return exp_cps_convert(self, cont, fcont)
  
  def cps_convert_exps(self, exps, cont, fcont):
    if not exps: return il.Clamda(v, fc, cont(il.tuple(), fc))
    if len(exps)==1:
      return self.cps_convert(exps[0], cont, fcont)
    else:
      return self.cps_convert(exps[0], 
                  il.Clamda(v, fc, self.cps_convert_exps(exps[1:], cont, fcont)), fcont)
 
#α-conversion
#Alpha-conversion, sometimes known as alpha-renaming,[11] allows bound variable names to be changed. 
#For example, alpha-conversion of λx.x might yield λy.y. 
#Terms that differ only by alpha-conversion are called α-equivalent. Frequently in uses of lambda calculus, 
#α-equivalent terms are considered to be equivalent.
#The precise rules for alpha-conversion are not completely trivial. First, when alpha-converting an abstraction, 
#the only variable occurrences that are renamed are those that are bound to the same abstraction. 
#For example, an alpha-conversion of λx.λx.x could result in λy.λx.x, but it could not result in λy.λx.y. 
#The latter has a different meaning from the original.
#Second, alpha-conversion is not possible if it would result in a variable getting captured by a different abstraction. 
#For example, if we replace x with y in λx.λy.x, we get λy.λy.y, which is not at all the same.
#In programming languages with static scope, alpha-conversion can be used to make name resolution simpler 
#by ensuring that no variable name masks a name in a containing scope 
#(see alpha renaming to make name resolution trivial).

#Some compilers include an alpha-conversion stage to rename all program variables 
#such that variable names become unique. 
#(This simplifies subsequent processing somewhat.)

class AlphaConvertEnvironment:
  def __init__(self, outer=None, newvar_map=None):
    self.lefts = set() #left of assign after alpha convert
    self.bindings = {}
    self.outer = outer
    if outer is None: newvar_map = {}
    self.newvar_map = newvar_map
  
  def extend(self):
    return AlphaConvertEnvironment(self, self.newvar_map)
  
  def __getitem__(self, var):
    try:
      return self.bindings[var]
    except:
      if self.outer is not None:
        return self.outer[var]
      else:
        raise VariableNotBound(var)
     
  def new_var(self, var):
    try: 
      suffix = ''+repr(self.newvar_map[var.name])
      self.newvar_map[var.name] += 1
      return var.__class__(var.name+suffix)
    except:
      self.newvar_map[var.name] = 1
      return var
  
    # if the definition for same method of different class can be put in one place,
    # then the if_elif can be avoided, meanwhile the code is more understandable.
  def alpha_convert(self, exp):
    '''alpha convert expresson based on alpha equation, renaming all variables in different scopes to different names.
    calculating the references and assigns of variables in the meanwhile.
    # todo: code size and line number '''
    
    if isinstance(exp, il.Lamda):
      try:
        exp.before_alpha_convert
        return exp
      except: exp.before_alpha_convert  = (exp.params, exp.body)
      
      new_env = self.extend()
      for p in exp.params: 
        new_env.bindings[p] = new_env.new_var(p)
      exp.params = tuple(new_env[p] for p in exp.params)
      exp.body = tuple(new_env.alpha_convert(x) for x in exp.body)
      exp.variables = new_env.bindings.values()
      exp.lefts = new_env.lefts # prepare for assign convert
      return exp
    
    elif isinstance(exp, il.Var):
      return self[exp]
    
    elif isinstance(exp, il.LogicVar):
      return self
    
    elif  isinstance(exp, il.Apply):
      return exp.__class__(self.alpha_convert(exp.caller), 
                   tuple(self.alpha_convert(arg) for arg in exp.args))
    
    elif  isinstance(exp, il.Return):
      return il.Return(*tuple(self.alpha_convert(arg) for arg in exp.args))
    
    elif  isinstance(exp, il.Assign):
      try: converted_var = self[exp.var]
      except VariableNotBound:
        converted_var = self.bindings[exp.var] = self.new_var(exp.var)
      self.lefts.add(converted_var)
      return il.Assign(converted_var, self.alpha_convert(exp.exp))
    
    elif  isinstance(exp, il.If):
      return il.If(self.alpha_convert(exp.test), self.alpha_convert(exp.then), 
                   self.alpha_convert(exp.else_))
    
    elif  isinstance(exp, il.If2):
      return il.If2(self.alpha_convert(exp.test), self.alpha_convert(exp.then))
    
    elif isinstance(exp, il.Unify):
      return il.Unify(self.alpha_convert(exp.left), self.alpha_convert(exp.right),
                   self.alpha_convert(exp.cont), self.alpha_convert(exp.fcont))
    
    elif isinstance(exp, il.BinaryOperation):
      return exp

    elif isinstance(exp, tuple):
      return tuple(self.alpha_convert(x) for x in exp)
    
    elif isinstance(exp, list):
      return [self.alpha_convert(x) for x in exp]
    
    elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
      return exp
    
    elif exp is None:
      return None
    
    else: raise CompileTypeError(exp)

  def __repr__(self):
    result = ''
    while self is not None:
      result += repr(self.bindings)
      self = self.outer
    return result
       
def assign_convert(exp, alpha_env, env):
  # alpha_env is the AlphaConvertEnvironment after alpha convert
  
  if isinstance(exp, il.Lamda):
    try:
      exp.before_assign_convert
      return exp
    except: exp.assign_convert = (exp.params, exp.body)
    
    new_env = env.extend()
    for p in exp.lefts: 
      new_env.bindings[p] = alpha_env.new_var(p)
    make_cells = tuple((new_env[p], il.make_cell(p)) for p in exp.lefts)
    exp.body = (il.let(make_cells, *tuple(assign_convert(x, alpha_env, new_env) for x in exp.body)),)
    return exp
  
  if isinstance(exp, il.Var):
    if exp in env:
      return il.contents(env[exp])
    else: return exp
    
  elif  isinstance(exp, il.Apply):
    return exp.__class__(assign_convert(exp.caller, alpha_env, env), 
                 tuple(assign_convert(arg, alpha_env, env) for arg in exp.args))
  
  elif  isinstance(exp, il.Return):
    return il.Return(*tuple(assign_convert(arg, alpha_env, env) for arg in exp.args))
  
  elif  isinstance(exp, il.Assign): # var = value, exp.exp should be a single var, 
                                    # which is the continuation param which ref to the value
    return il.set_contents(env[exp.var], exp.exp)
  
  elif  isinstance(exp, il.If):
    return il.If(assign_convert(exp.test, alpha_env, env), 
                 assign_convert(exp.then, alpha_env, env), assign_convert(exp.else_, alpha_env, env))
  
  elif  isinstance(exp, il.If2):
    return il.If2(assign_convert(exp.test, alpha_env, env), assign_convert(exp.then, alpha_env, env))
  
  elif isinstance(exp, il.Unify):
    return il.Unify(assign_convert(exp.left, alpha_env, env), assign_convert(exp.right, alpha_env, env),
                 assign_convert(exp.cont, alpha_env, env), assign_convert(exp.fcont, alpha_env, env))
  
  elif isinstance(exp, list):
    return [assign_convert(x, alpha_env, env) for x in exp]
  
  elif isinstance(exp, tuple):
    return tuple(assign_convert(x, alpha_env, env) for x in exp)
  
  elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
    return exp
  
  elif exp is None:
    return None
  
  else: raise CompileTypeError(exp)


def trampoline(exp):
  if isinstance(exp, il.Lamda):
    try:
      exp.assign_convert
      return exp
    except: exp.assign_convert = True
    
    new_env = env.extend()
    for p in exp.lefts: 
      new_env.bindings[p] = alpha_env.new_var(p)
    make_cells = tuple((new_env[p], il.make_cell(p)) for p in exp.lefts)
    return il.Lamda(exp.params,
                 il.let(make_cells, tuple(assign_convert(x, alpha_env, new_env) for x in exp.body)))
  
  if isinstance(exp, il.Var):
    if exp in env:
      return il.contents(env[exp])
    else: return exp
    
  elif  isinstance(exp, il.Apply):
    return exp.__class__(assign_convert(exp.caller, alpha_env, env), 
                 tuple(assign_convert(arg, alpha_env, env) for arg in exp.args))
  
  elif  isinstance(exp, il.Return):
    return il.Return(*tuple(assign_convert(arg, alpha_env, env) for arg in exp.args))
  
  elif  isinstance(exp, il.Assign): # var = value, exp.exp should be a single var, 
                                    # which is the continuation param which ref to the value
    return il.set_contents(env[exp.var], exp.exp)
  
  elif  isinstance(exp, il.If):
    return il.If(assign_convert(exp.test, alpha_env, env), 
                 assign_convert(exp.then, alpha_env, env), assign_convert(exp.else_, alpha_env, env))
  
  elif  isinstance(exp, il.If2):
    return il.If2(assign_convert(exp.test, alpha_env, env), assign_convert(exp.then, alpha_env, env))
  
  elif  isinstance(exp, il.Tuple):
    return il.Tuple(exp.elements, alpha_env, env)
  
  elif isinstance(exp, il.Unify):
    return il.Unify(assign_convert(exp.left, alpha_env, env), assign_convert(exp.right, alpha_env, env),
                 assign_convert(exp.cont, alpha_env, env), assign_convert(exp.fcont, alpha_env, env))
  
  elif isinstance(exp, list):
    return [assign_convert(x, alpha_env, env) for x in exp]
  
  elif isinstance(exp, tuple):
    return tuple(assign_convert(x, alpha_env, env) for x in exp)
  
  elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
    return exp
  
  elif exp is None:
    return None
  
  else: raise CompileTypeError(exp)
  
