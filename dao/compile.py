# -*- coding: utf-8 -*-

'''code for compilation:
alpha convert -> cps convert -> assign-convert 
-> optimization 
-> tail recursive convert -> trampoline 
-> pythonize -> generate code
'''

from dao import interlang as il

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
  
  def cps(self, exp, cont):
    return self.cps_convert(exp, cont)
    
  def cps_convert(self, exp, cont):
    try: 
      exp_cps_convert = exp.cps_convert
      
    except:       
      if isinstance(exp, tuple) or  isinstance(exp, list) or\
         isinstance(exp, int) or isinstance(exp, float) or\
         isinstance(exp, str) or isinstance(exp, unicode):
        return cont(exp)
      else: raise CompileTypeError(exp)
      
    return exp_cps_convert(self, cont)
  
  def cps_convert_exps(self, exps, cont):
    if not exps: return il.Clamda(v, cont(il.tuple()))
    if len(exps)==1:
      return self.cps_convert(exps[0], cont)
    else:
      return self.cps_convert(exps[0], il.Clamda(v, self.cps_convert_exps(exps[1:], cont)))
 
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
        
    try: 
      exp_alpha_convert = exp.alpha_convert

    except:       
      if isinstance(exp, list):
        return [self.alpha_convert(x) for x in exp]
      
      elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
        return exp
      
      elif exp is None:
        return None
    
      else: raise CompileTypeError(exp)
      
    return exp_alpha_convert(self)

  def __repr__(self):
    result = ''
    while self is not None:
      result += repr(self.bindings)
      self = self.outer
    return result
       
def assign_convert(exp, alpha_env, env):
  # alpha_env is the AlphaConvertEnvironment after alpha convert
      
  try: 
    exp_assign_convert = exp.assign_convert

  except:       
    if isinstance(exp, list):
      return [assign_convert(x, alpha_env, env) for x in exp]
    
    elif isinstance(exp, tuple):
      return tuple(assign_convert(x, alpha_env, env) for x in exp)
    
    elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
      return exp
    
    elif exp is None:
      return None
    
    else: raise CompileTypeError(exp)
    
  return exp_assign_convert(alpha_env, env)

# -*- coding: utf-8 -*-
''' code for optimization:
 (lambda (): body)() => body
 (lambda (...): body)(args) => body # if paramaters is not used in body
 (lambda (params): body)(args) => (lambda (params') body')(args') # inline args which are used only once.
'''

from dao.compile import CompileTypeError

class OptimizationData:
  def __init__(self):
    self.ref_count = {}
    self.called_count = {}
    self.occur_count = {}
    
  def __repr__(self):
    return repr(self.ref_count)

def analyse_before_optimize(exp, data):  
  try: 
    exp_analyse_before_optimize = exp.analyse_before_optimize

  except:       
    if isinstance(exp, tuple):
      for x in exp:
        analyse_before_optimize(x, data)
    
    elif isinstance(exp, list):
      return
    
    elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
      return
      
    elif exp is None:
      return
    
    else: raise CompileTypeError(exp)
  
  return exp_analyse_before_optimize(data)
  
MAX_EXTEND_CODE_SIZE = 10

def code_size(exp):
  try: 
    exp_code_size = exp.code_size

  except:       
    if isinstance(exp, list):
      return sum([code_size(x) for x in exp])
    
    elif isinstance(exp, tuple):
      return sum([code_size(x) for x in exp])
    
    elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
      return 1
    
    elif exp is None:
      return 1
    
    else: raise CompileTypeError(exp)
  
  return exp_code_size()

def side_effects(exp):
  try: 
    exp_side_effects = exp.side_effects

  except:       
    if isinstance(exp, list):
      return False
    
    elif isinstance(exp, tuple):
      return False
    
    elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
      return False
    
    elif exp is None:
      return None
    
    else: raise CompileTypeError(exp)
  
  return exp_side_effects()

def lambda_side_effects(exp):
  return side_effects(exp.body)

def subst(exp, bindings):  
  try: 
    exp_subst = exp.subst

  except:       
    if isinstance(exp, list):
      return [subst(x, bindings) for x in exp]
    
    elif isinstance(exp, tuple):
      return tuple(subst(x, bindings) for x in exp)
    
    elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
      return exp
    
    elif exp is None:
      return None
    
    else: raise CompileTypeError(exp)
    
  return exp_subst(bindings)

def optimize(exp, data):
  changed = True
  while changed:
    exp, changed = optimize_once(exp, data)
  return exp

def optimize_once(exp, data):
  try: 
    exp_optimize_once = exp.optimize_once

  except:       
    if isinstance(exp, tuple):
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
  
  return exp_optimize_once(data)
  
def trampoline(exp):
  raise NotImplemented('trampline is not coded')

def is_statement(exp):
  try: return exp.is_statement
  except:
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
  try: 
    exp_pythonize = exp.pythonize

  except:       
    if isinstance(exp, list):
      return exp
    
    elif isinstance(exp, tuple):
      return exp
    
    elif isinstance(exp, int) or isinstance(exp, float) or \
         isinstance(exp, str) or isinstance(exp, unicode):
      return exp
    
    elif exp is None:
      return None
    
    else: raise CompileTypeError(exp)
    
  return exp_pythonize(env)
  
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
    try: 
      exp_to_code = exp.to_code
  
    except:       
      if isinstance(exp, list):
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
      
    return exp_to_code(self)
   
  def to_code_list(self, items, in_lambda=True):
    return [self.to_code(x) for x in items]  