# -*- coding: utf-8 -*-

from oad.term import Var, Function, Macro, Apply

builtins = []

class Builtin: 
  def __init__(self, function, name=None):
    if name is None: name = function.__name__
    self.function = function
    self.name = name
  def __eq__(self, other): 
    return self.__class__==other.__class__ and self.function==other.function
  def __repr__(self): return '<%s>'%(self.name)

class BuiltinFunction(Builtin, Function):
  def __call__(self, *exps):
    return Apply(self, *exps)
  def apply(self, solver, values, cont):
    yield cont, self.function(*values)
    
class BuiltinFunction2(Builtin, Function):
  def __call__(self, *exps):
    return Apply(self, *exps)
  def apply(self, solver, values, cont):
    return self.function(solver, cont, *values)
  
class BuiltinMacro(Builtin, Macro):
  def __call__(self, *exps):
    return Apply(self, *exps)
  def apply(self, solver, exps, cont):
    return self.function(solver, cont, *exps)
  
def builtin(klass):
  def builtin(name=None):
    def makeBuiltin(func):
      if name is None: name1 = func.__name__
      else: name1 = name
      b = klass(func, name1)
      builtins.append(b)
      return b
    return makeBuiltin
  return builtin

function = builtin(BuiltinFunction)
function2 = builtin(BuiltinFunction2)
macro = builtin(BuiltinMacro)
