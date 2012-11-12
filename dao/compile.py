# -*- coding: utf-8 -*-

'''code for compilation:
alpha convert -> cps convert -> assign-convert 
-> optimization 
-> tail recursive convert -> trampoline 
-> pythonize -> generate code
'''

from dao.compilebase import CompileTypeError, VariableNotBound

#α-conversion
def alpha_convert(exp, env):
  '''alpha convert expresson based on alpha equation, renaming all variables in different scopes to different names.
  calculating the references and assigns of variables in the meanwhile.
  # todo: code size and line number '''
      
  try: 
    exp_alpha_convert = exp.alpha_convert

  except:       
    if isinstance(exp, list):
      return [alpha_convert(x, env) for x in exp]
    
    elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
      return exp
    
    elif exp is None:
      return None
  
    else: raise CompileTypeError(exp)
    
  return exp_alpha_convert(env)

def cps_convert(compiler, exp, cont):
  try: 
    exp_cps_convert = exp.cps_convert
    
  except:       
    if isinstance(exp, tuple) or  isinstance(exp, list) or\
       isinstance(exp, int) or isinstance(exp, float) or\
       isinstance(exp, str) or isinstance(exp, unicode):
      return cont(exp)
    else: raise CompileTypeError(exp)
    
  return exp_cps_convert(compiler, cont)

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

def optimization_analisys(exp, data):  
  try: 
    exp_optimization_analisys = exp.optimization_analisys

  except:       
    if isinstance(exp, tuple):
      for x in exp:
        optimization_analisys(x, data)
    
    elif isinstance(exp, list):
      return
    
    elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
      return
      
    elif exp is None:
      return
    
    else: raise CompileTypeError(exp)
  
  return exp_optimization_analisys(data)
  
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
    
def to_code(exp):
  exp = pythonize(exp, AlphaConvertEnvironment())
  coder = CodeGenerator()
  return coder.to_code(exp)

def to_code(coder, exp):
  try: 
    exp_to_code = exp.to_code

  except:       
    if isinstance(exp, list):
      return '[%s]'%', '.join(tuple(to_code(coder, x) for x in exp))
    
    elif isinstance(exp, tuple):
      return '(%s)'%', '.join(tuple(to_code(coder, x) for x in exp))
    
    elif isinstance(exp, int) or isinstance(exp, float) or\
         isinstance(exp, str) or isinstance(exp, unicode):
      return repr(exp)
    
    elif exp is None:
      return 'None'
    
    else:
      return repr(exp)
    
  return exp_to_code(coder)
 
def to_code_list(coder, items, in_lambda=True):
  return [coder.to_code(x) for x in items]