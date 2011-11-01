# -*- coding: utf-8 -*-

from dao.term import Var, Function, Macro, CommandCall, Command
from dao.base import uniset

builtins = []

class Builtin: 
  def __init__(self, function, name=None, symbol=None):
    if name is None: name = function.__name__
    self.function = function
    self.name = name
    self.symbol = symbol if symbol else name
  def copy(self): return self.__class__(self.function, self.name)
  def __eq__(self, other): 
    return isinstance(other, self.__class__) and self.function==other.function
  def __repr__(self): return '<%s>'%(self.name)

class BuiltinFunction(Builtin, Function):
  def apply(self, solver, values, cont):
    yield cont, self.function(*values)
    
class BuiltinFunction2(Builtin, Function):
  def apply(self, solver, values, cont):
    return self.function(solver, cont, *values)
  
class BuiltinMacro(Builtin, Macro):
  def apply(self, solver, exps, cont):
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

def first_set(func):
  def set_builtin(builtin):
    builtin.first_set_func = func
    return builtin
  return set_builtin

function = builtin(BuiltinFunction)
function2 = builtin(BuiltinFunction2)
macro = builtin(BuiltinMacro)