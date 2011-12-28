# -*- coding: utf-8 -*-

from dao.command import Command, Function, Macro
#from dao.term import CommandCall
from dao.solvebase import mycont, BUILTIN_FUNCTION, BUILTIN_PREDICATE, BUILTIN_MACRO
from dao.compiler import type

class Builtin: 
  def __init__(self, function, name, symbol, is_global):
    if name is None: name = function.__name__
    self.function = function
    self.name = name
    self.symbol = symbol if symbol else name
    self.is_global = is_global
    
  def copy(self): return self.__class__(self.function, self.name)
  def __hash__(self): return hash(self.function)
  def __eq__(self, other): 
    return isinstance(other, self.__class__) and self.function==other.function
  def __repr__(self): return '<%s>'%(self.name)
  
  # compile
  
  def code(self): return self.name
  
_memorable = False

class BuiltinFunction(Builtin, Function):
  command_type = BUILTIN_FUNCTION
  memorable = _memorable
  type = type.builtin_function
  def __call__(self, *args):
    return CommandCall(self, *args)
  
  def get_argument(self, arg, cont, solver):
    solver.scont = solver.cont(arg, cont)
    return True
  
  @classmethod
  def compile_argument(cls, arg, cont, compiler):
    compiler.scont = compiler.cont(arg, cont)
    return compiler.scont
    
  def apply(self, solver, values, signatures):
    #solver.scont = cont
    return self.function(*values)  
    
class BuiltinPredicateCont:
  def __init__(self, operator, args):
    self.operator, self.args = operator, args
  def code(self):
    return '''
def builtin_predicate_fun():
  for values in apply_generator_fun_list([%s]):
    for x in %s(*values):
      yield x
for x in builtin_predicate_fun():
  print x
'''%(', '.join([code(x) for x in self.args]), code(self.operator))
    
class BuiltinPredicate(Builtin, Function):
  command_type = BUILTIN_PREDICATE
  memorable = _memorable
  type = type.builtin_predicate
  def __call__(self, *args):
    return CommandCall(self, *args)
  def apply(self, solver, values, signatures):
    return self.function(solver, *values)
    
class BuiltinMacroCont:
  def __init__(self, operator, args):
    self.operator, self.args = operator, args
  def code(self):
    return '''
def builtin_macro_fun():
  for x in %s(%s):
    yield x
for x in builtin_macro_fun():
  print x
'''%(
      code(self.operator), ', '.join([code(x) for x in self.args]))
    
class BuiltinMacro(Builtin, Macro):
  type = type.builtin_macro
  command_type = BUILTIN_MACRO
  memorable = _memorable
  def __call__(self, *args):
    return CommandCall(self, *args)
  def apply(self, solver, args, signatures):
    return self.function(solver, *args)
    
def builtin(klass):
  def builtin(name=None, symbol=None, **kw):
    def makeBuiltin(func):
      if name is None: name1 = func.__name__
      else: name1 = name
      is_global = kw.get('is_global', False)
      b = klass(func, name1, symbol, is_global)
      return b
    return makeBuiltin
  return builtin

def memo(builtin):
  builtin.memorable = True
  return builtin

def nomemo(builtin):
  builtin.memorable = False
  return builtin

def set_type(type):
  def set_type_func(builtin):
    builtin.type = type
    return builtin
  return set_type_func


function = builtin(BuiltinFunction)
predicate = builtin(BuiltinPredicate)
macro = builtin(BuiltinMacro)
