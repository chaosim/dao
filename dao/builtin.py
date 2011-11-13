# -*- coding: utf-8 -*-

from dao.term import Command, Function, Macro, CommandCall
from dao.solve import mycont

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

_memorable = False

class BuiltinFunction(Builtin, Function):
  memorable = _memorable
  def __call__(self, *exps):
    return CommandCall(self, *exps)
  
  def evaluate_arguments(self, solver, cont, exps):
    if len(exps)==0: 
      yield cont, ()
    else:
      @mycont(cont)
      def argument_cont(value, solver):
        @mycont(cont)
        def gather_cont(values, solver):
            for c, v in cont((value,)+values, solver): 
              yield c, v
        return self.evaluate_arguments(solver, gather_cont, exps[1:])
      yield solver.cont(exps[0], argument_cont), True
      
  def apply(self, solver, cont, values, signatures):
    yield cont, self.function(*values)
    
class BuiltinPredicate(Builtin, Function):
  memorable = _memorable
  def __call__(self, *exps):
    return CommandCall(self, *exps)
  def apply(self, solver, cont, values, signatures):
    return self.function(solver, cont, *values)
  
class BuiltinMacro(Builtin, Macro):
  memorable = _memorable
  def __call__(self, *exps):
    return CommandCall(self, *exps)
  def apply(self, solver, cont, exps, signatures):
    return self.function(solver, cont, *exps)
  
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

function = builtin(BuiltinFunction)
predicate = builtin(BuiltinPredicate)
macro = builtin(BuiltinMacro)
