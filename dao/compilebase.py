# -*- coding: utf-8 -*-

from dao.base import classeq

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

class DaoNotImplemented(Exception):
  def __init__(self, message):
    self.message = message
    
  def __repr__(self): 
    return self.message

class Compiler:
  def __init__(self):
    self.newvar_map = {} #{'name':index}

  def new_var(self, var):
    try: 
      suffix = str(self.newvar_map[var.name])
      self.newvar_map[var.name] += 1
      return var.__class__(var.name+suffix)
    except:
      self.newvar_map[var.name] = 1
      return var
  
class Environment:
  def __init__(self, outer=None):
    self.bindings = {}
    self.outer = outer
  
  def extend(self):
    return Environment(self)
  
  def __getitem__(self, var):
    try:
      return self.bindings[var]
    except:
      outer = self.outer
      while outer is not None:
        try: return self.outer.bindings[var]
        except: outer = outer.outer
    raise VariableNotBound(var)
  
  def __setitem__(self, var, value):
    self.bindings[var] = value
     
  def __repr__(self):
    result = ''
    while self is not None:
      result += repr(self.bindings)
      self = self.outer
    return result
       
class OptimizationData:
  def __init__(self):
    self.ref_count = {}
    self.called_count = {}
    self.occur_count = {}
    
  def __repr__(self):
    return repr(self.ref_count)

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
  
#α-conversion
def alpha_convert(exp, env, compiler):
  '''alpha convert expresson based on alpha equation, renaming all variables in different scopes to different names.
  calculating the references and assigns of variables in the meanwhile.
  # todo: code size and line number '''
  try: 
    exp_alpha_convert = exp.alpha_convert
  except:       
    if isinstance(exp, tuple):
      # tuple is not transparent to dao.
      return tuple(alpha_convert(x, env, compiler) for x in exp)
    elif isinstance(exp, list):
      # list is just a value transparent to dao.
      return exp
    elif isinstance(exp, int) or isinstance(exp, float) or\
         isinstance(exp, str) or isinstance(exp, unicode):
      return exp
    elif exp is None:
      return None
    else: raise CompileTypeError(exp)
  return exp_alpha_convert(env, compiler)

def cps_convert(compiler, exp, cont):
  try: 
    exp_cps_convert = exp.cps_convert
  except:       
    if isinstance(exp, tuple):
      return cont(exp)
    elif isinstance(exp, list):
      return cont(exp)
    elif isinstance(exp, int) or isinstance(exp, float) or\
       isinstance(exp, str) or isinstance(exp, unicode):
      return cont(exp)
    elif exp is None:
      return cont(None)
    else: raise CompileTypeError(exp)
    
  return exp_cps_convert(compiler, cont)

def assign_convert(exp, env, compiler):
  # alpha_env is the Environment after alpha convert
  try: 
    exp_assign_convert = exp.assign_convert
  except:       
    if isinstance(exp, list):
      return exp
    elif isinstance(exp, tuple):
      return tuple(assign_convert(x, env, compiler) for x in exp)
    elif isinstance(exp, int) or isinstance(exp, float) or\
         isinstance(exp, str) or isinstance(exp, unicode):
      return exp
    elif exp is None:
      return None
    else: raise CompileTypeError(exp)
  return exp_assign_convert(env, compiler)

def find_assign_lefts(exp):
  try: 
    exp_find_assign_lefts = exp.find_assign_lefts
  except:       
    if isinstance(exp, list) or isinstance(exp, tuple) or\
       isinstance(exp, int) or isinstance(exp, float) or\
       isinstance(exp, str) or isinstance(exp, unicode) or\
       exp is None:
      return set()
    else: raise CompileTypeError(exp)
  return exp_find_assign_lefts()

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
    if isinstance(exp, list):
      return
    elif isinstance(exp, tuple):
      for x in exp:
        optimization_analisys(x, data)
      return
    elif isinstance(exp, int) or isinstance(exp, float) or\
         isinstance(exp, str) or isinstance(exp, unicode):
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
    elif isinstance(exp, int) or isinstance(exp, float) or\
         isinstance(exp, str) or isinstance(exp, unicode):
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
    elif isinstance(exp, int) or isinstance(exp, float) or\
         isinstance(exp, str) or isinstance(exp, unicode):
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
      return exp
    elif isinstance(exp, tuple):
      return tuple(subst(x, bindings) for x in exp)
    elif isinstance(exp, int) or isinstance(exp, float) or\
         isinstance(exp, str) or isinstance(exp, unicode):
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
    if isinstance(exp, list):
      return exp, False
    elif isinstance(exp, tuple):
      changed = False
      result = []
      for x in exp:
        x, x_changed = optimize_once(x, data)
        result.append(x)
        changed = changed or x_changed
      return tuple(result), changed
    elif isinstance(exp, int) or isinstance(exp, float) or\
         isinstance(exp, str) or isinstance(exp, unicode):
      return exp, False
    elif exp is None:
      return None, False  
    else: raise CompileTypeError(exp)
  return exp_optimize_once(data)
  
def tail_recursive_convert(exp):
  return exp
  #raise DaoNotImplemented('tail_recursive_convert')
  
def trampoline(exp):
  return exp
  #raise DaoNotImplemented('trampoline')

def insert_return_yield(exp, klass):
  try: 
    exp_insert_return_yield = exp.insert_return_yield
  except:
    if isinstance(exp, list) or isinstance(exp, tuple) or\
       isinstance(exp, int) or isinstance(exp, float) or \
       isinstance(exp, str) or isinstance(exp, unicode) or\
       exp is None:
      return klass(exp)
    else: raise CompileTypeError(exp)    
  return exp_insert_return_yield(klass)
    
def pythonize(exp, env, compiler):
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
  return exp_pythonize(env, compiler)
    
def generate_code(exp):
  exp = pythonize(exp, Environment())
  coder = CodeGenerator()
  return to_code(coder, exp)

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
  return [to_code(coder, x) for x in items]