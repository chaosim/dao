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
    self.exit_block_cont_map = {}
    self.next_block_cont_map = {}

  def new_var(self, var):
    try: 
      suffix = str(self.newvar_map[var.name])
      self.newvar_map[var.name] += 1
      return var.__class__(var.name+suffix)
    except:
      self.newvar_map[var.name] = 1
      return var
  
class Environment:
  '''environment for compile, especilly for alpha convert, block/exit/continue'''
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
    
  def get_inner_block_label(self):
    if self.outer is None:
      raise BlockError("should not escape from top level outside of block.")
    else: return self.outer.get_inner_block_label()
    
  def get_block_label(self, old_label):   
    if self.outer is None:
      raise BlockError("Block %s is not found."%old_label)
    else: 
      return self.outer.get_block_label(old_label)
    
  def __repr__(self):
    result = ''
    while self is not None:
      result += repr(self.bindings)
      self = self.outer
    return result
       
class BlockEnvironment(Environment):
  def __init__(self, old_label, new_label):
    self.old_label = old_label
    self.new_label = new_label
    
  def get_inner_block_label(self):
    return self.new_label
  
  def get_block_label(self, old_label):
    if old_label==self.old_label:
      return self.new_label
    elif self.outer is None:
      raise BlockError("Block %s is not found."%old_label)
    else: 
      return self.outer.get_block_label(old_label)

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
    
  def indent(self, code, level=1):
    lines = code.split('\n')
    lines = tuple(self.indent_space*level + line for line in lines)
    return '\n'.join(lines)
    
  
MAX_EXTEND_CODE_SIZE = 10

def lambda_side_effects(exp):
  return side_effects(exp.body)

def optimize(exp, data):
  changed = True
  while changed:
    exp, changed = exp.optimize_once(data)
  return exp

def generate_code(exp):
  exp = exp.pythonize_exp(Environment(), Compiler())
  coder = CodeGenerator()
  return exp.to_code(coder)

def to_code_list(coder, items, in_lambda=True):
  return tuple(x.to_code(coder) for x in items)