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

class Compiler:
  def __init__(self):
    pass
  
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
    
  # if the definition for same method of different class can be put in one place,
  # then the if_elif can be avoided, meanwhile the code is more understandable.
