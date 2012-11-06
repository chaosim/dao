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
      return il.Return(tuple(self.alpha_convert(arg) for arg in exp.args))
    
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
    return il.Apply(assign_convert(exp.caller, alpha_env, env), 
                 tuple(assign_convert(arg, alpha_env, env) for arg in exp.args))
  
  elif  isinstance(exp, il.Return):
    return il.Return(tuple(assign_convert(arg, alpha_env, env) for arg in exp.args))
  
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
    return il.Apply(assign_convert(exp.caller, alpha_env, env), 
                 tuple(assign_convert(arg, alpha_env, env) for arg in exp.args))
  
  elif  isinstance(exp, il.Return):
    return il.Return(tuple(assign_convert(arg, alpha_env, env) for arg in exp.args))
  
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
  
def collocate(defs, exp):
  if defs:
    return il.StatementList(defs + (exp,))
  else: 
    return exp

def is_statement(exp):
  try: return exp.is_statement
  except:
    if isinstance(exp, il.Element):
      return False
    if isinstance(exp, list) or isinstance(exp, tuple) or\
      ( isinstance(exp, int) or isinstance(exp, float)
        or isinstance(exp, str) or isinstance(exp, unicode)):
      return False
  raise CompileTypeError(exp)
  
def to_code(exp):
  exp = pythonize(exp, AlphaConvertEnvironment())
  coder = CodeGenerator()
  return coder.to_code(exp)

def pythonize_list(exps, env):
  defs = ()
  exps2 = ()    
  for x in exps:
    exp = pythonize(x, env)
    if isinstance(exp, il.Function):
      defs += (exp,)
      exps2 += (exp.name,)
    else:
      exps2 += (exp,)
  return defs, exps2

def pythonize(exp, env):
  
  if isinstance(exp, il.Lamda):
    body_exps = ()
    body_is_statement = False
    for x in exp.body:
      x = pythonize(x, env)
      if is_statement(x):
        body_is_statement = True
      body_exps += (x,)
    if not body_is_statement:
      return il.Lamda(exp.params, *body_exps)
    else:
      return il.Function(env.new_var(il.Var('function')), exp.params, *body_exps)
    
  elif isinstance(exp, il.Var): return exp
    
  elif isinstance(exp, il.LogicVar): return exp
    
  elif  isinstance(exp, il.Apply):
    caller = pythonize(exp.caller, env)
    defs = ()
    if isinstance(caller, il.Function):
      defs += (caller,)
      caller = caller.name
    defs1, args = pythonize_list(exp.args, env)
    defs += defs1
    return collocate(defs, exp.__class__(caller,args))
  
  elif  isinstance(exp, il.BinaryOperation):
    return exp
  
  elif  isinstance(exp, il.Return):
    defs, args = pythonize_list(exp.args, env)
    return collocate(defs, il.Return(*args))
  
  elif  isinstance(exp, il.Assign): # var = value, exp.exp should be a single var, 
                                    # which is the continuation param which ref to the value
    return exp
  
  elif  isinstance(exp, il.If):
    defs, (test, then, else_) = pythonize_list((exp.test, exp.then, exp.else_), env)
    return collocate(defs, il.If(test, then, else_))
  
  elif  isinstance(exp, il.If2):
    defs, (test, then) = pythonize_list((exp.test, exp.then), env)
    return collocate(defs, il.If2(test, then))
  
  elif isinstance(exp, il.Unify):
    defs, (left, right, cont, fcont) = pythonize_list((exp.left, exp.right, exp.cont, exp.fcont), env)
    return collocate(defs, il.Unify(left, right, cont, fcont))
  
  elif isinstance(exp, list):
    return exp
  
  elif isinstance(exp, tuple):
    return exp
  
  elif isinstance(exp, int) or isinstance(exp, float) or \
       isinstance(exp, str) or isinstance(exp, unicode):
    return exp
  
  elif exp is None:
    return None
  
  else: raise CompileTypeError(exp)
  
class CodeGenerator: 
  def __init__(self, indent_space='  ', language='python'):
    self.language = language
    self.indent_space = indent_space
    self.var_index_map = {'function':0}
    self.var_index = 0
    self.lambda_stack = []
    
  def indent(self, code, level=1):
    lines = code.split('\n')
    lines = tuple(self.indent_space*level + line for line in lines)
    return '\n'.join(lines)
  
  def newvar(self, kind='function'):
    if kind=='function':
      self.var_index_map[kind] += 1
      return 'function'+repr(self.var_index_map[kind])
    self.var_index += 1
    return 'x'+repr(self.var_index)
    
  # if the definition for same method of different class can be put in one place,
  # then the if_elif can be avoided, meanwhile the code is more understandable.
  
  def to_code(self, exp):
    
    if isinstance(exp, il.Function):
      head = "def %s(%s):\n" % (exp.name, ', '.join(self.to_code_list(exp.params)))
      self.lambda_stack.append(exp)
      result =  head + self.indent('\n'.join(self.to_code_list(exp.body)))
      self.lambda_stack.pop()
      return result
      
    elif isinstance(exp, il.Lamda):
      head = "lambda %s: " % ', '.join(self.to_code_list(exp.params))
      self.lambda_stack.append(exp)
      result = head + '(%s)'%', '.join(self.to_code_list(exp.body))
      self.lambda_stack.pop()
      return result
        
    elif isinstance(exp, il.BinaryOperation): # MUST be BEFORE subclass.
      return exp.operator
      
    elif isinstance(exp, il.BinaryOperationApply):
      return '%s%s%s'%(self.to_code(exp.args[0]), 
                          self.to_code(exp.caller), 
                          self.to_code(exp.args[1]))
    
    elif  isinstance(exp, il.Apply): # MUST be AFTER subclass.
      if isinstance(exp.caller, il.Lamda):
        return "(%s)"%self.to_code(exp.caller) + '(%s)'%', '.join([self.to_code(x) for x in exp.args])
      else:
        return self.to_code(exp.caller) + '(%s)'%', '.join([self.to_code(x) for x in exp.args])        

    elif  isinstance(exp, il.StatementList):
      return  '\n'.join([self.to_code(x) for x in exp.statements])
      
    elif  isinstance(exp, il.Return):
      if self.lambda_stack and isinstance(self.lambda_stack[-1], il.Function):
        return  'return %s' % ', '.join([self.to_code(x) for x in exp.args])
      else:
        return  ', '.join([self.to_code(x) for x in exp.args])
    
    elif  isinstance(exp, il.Assign):
      return  '%s = %s' % (self.to_code(exp.var), self.to_code(exp.exp))
    
    elif  isinstance(exp, il.If):
      if self.lambda_stack and isinstance(self.lambda_stack[-1], il.Function):
        return 'if %s: \n%s\nelse:\n%s' % (self.to_code(exp.test), self.indent(self.to_code(exp.then)), 
                                         self.indent(self.to_code(exp.else_)))        
      else:
        return '%s if %s else %s' % (self.to_code(exp.then), self.to_code(exp.test), 
                                         self.to_code(exp.else_))        
    
    elif  isinstance(exp, il.If2):
      return 'if %s: \n%s\n' % (self.to_code(exp.test), self.indent(self.to_code(exp.then)))

    elif isinstance(exp, il.Unify):
      return 'unify(%s, %s, %s, %s)' % (self.to_code(exp.left), self.to_code(exp.right), 
                                       self.to_code(exp.cont), self.to_code(exp.fcont))
    
    elif isinstance(exp, il.Var):
      return exp.name
    
    elif isinstance(exp, il.LogicVar):
      return  "LogicVar('%s')"%exp.name
    
    elif isinstance(exp, list):
      return '[%s]'%', '.join(tuple(self.to_code(x) for x in exp))
    
    elif isinstance(exp, tuple):
      return '(%s)'%', '.join(tuple(self.to_code(x) for x in exp))
    
    elif isinstance(exp, int) or isinstance(exp, float) or\
         isinstance(exp, str) or isinstance(exp, unicode):
      return repr(exp)
    
    elif exp is None:
      return 'None'
    
    else:
      return repr(exp)
   
  def to_code_list(self, items, in_lambda=True):
    return [self.to_code(x) for x in items]  