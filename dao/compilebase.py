# -*- coding: utf-8 -*-

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
    
  def __repr__(self):
    result = ''
    while self is not None:
      result += repr(self.bindings)
      self = self.outer
    return result

class Compiler:
  def __init__(self, indent_space='  ', language='python'):
    self.newvar_map = {} #{'name':index}
    
    # for block/exit/continue
    self.block_label_stack = []
    self.exit_block_cont_map = {}
    self.next_block_cont_map = {}
    
    # for optimization
    self.ref_count = {} # variable's reference count
    self.called_count = {} # lambda's reference count
    self.occur_count = {}
    self.recursive_call_path = []
    
    # for code generation
    self.language = language # object language
    self.indent_space = indent_space # indent width for python code

  def new_var(self, var):
    try: 
      suffix = str(self.newvar_map[var.name])
      self.newvar_map[var.name] += 1
      return var.__class__(var.name+suffix)
    except:
      self.newvar_map[var.name] = 1
      return var
    
  def get_inner_block_label(self):
    if self.block_label_stack:
      return self.block_label_stack[-1][1]
    else: 
      raise BlockError("should not escape from top level outside of all block.")
    
  def get_block_label(self, old_label): 
    for i in range(len(self.block_label_stack)):
      if old_label==self.block_label_stack[-(i+1)][0]:
        return self.block_label_stack[-(i+1)][1]
    raise BlockError("Block %s is not found."%old_label)
    
  def indent(self, code, level=1):
    '''python's famous indent'''
    lines = code.split('\n')
    lines = tuple(self.indent_space*level + line for line in lines)
    return '\n'.join(lines)    
  
MAX_EXTEND_CODE_SIZE = 10
