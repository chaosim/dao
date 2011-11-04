# -*- coding: utf-8 -*-

from dao.term import Var, Function, Macro, CommandCall, Command

builtins = []

class Builtin: 
  def __init__(self, function, name=None, symbol=None):
    if name is None: name = function.__name__
    self.function = function
    self.name = name
    self.symbol = symbol if symbol else name
  def copy(self): return self.__class__(self.function, self.name)
  def __hash__(self): return hash(self.function)
  def __eq__(self, other): 
    return isinstance(other, self.__class__) and self.function==other.function
  def __repr__(self): return '<%s>'%(self.name)

builtin_memorable = False

class BuiltinFunction(Builtin, Function):
  memorable = builtin_memorable
  def __call__(self, *exps):
    return CommandCall(self, *exps)
  def apply(self, solver, cont, values, signatures):
    yield cont, self.function(*values)
    
class BuiltinFunction2(Builtin, Function):
  memorable = builtin_memorable
  def __call__(self, *exps):
    return CommandCall(self, *exps)
  def apply(self, solver, cont, values, signatures):
    return self.function(solver, cont, *values)
  
class BuiltinMacro(Builtin, Macro):
  memorable = builtin_memorable
  def __call__(self, *exps):
    return CommandCall(self, *exps)
  def apply(self, solver, cont, exps, signatures):
    return self.function(solver, cont, *exps)
  
def builtin(klass):
  def builtin(name=None, symbol=None):
    def makeBuiltin(func):
      if name is None: name1 = func.__name__
      else: name1 = name
      b = klass(func, name1, symbol)
      builtins.append(b)
      return b
    return makeBuiltin
  return builtin

def memo(builtin):
  builtin.memorable = True
  return builtin

def nomemo(builtin):
  builtin.memorable = False
  return builtin

function = builtin(BuiltinFunction)
function2 = builtin(BuiltinFunction2)
macro = builtin(BuiltinMacro)
