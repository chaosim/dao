# -*- coding: utf-8 -*-

from dao.term import Command, Function, Macro, CommandCall
from dao.solve import mycont

# compile
from dao.compiler import code

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

class BuiltinFunctionCont:
  def __init__(self, operator, operands):
    self.operator, self.operands = operator, operands
  def code(self):
    return '%s(%s)'%(code(self.operator), ', '.join([code(x) for x in self.operands]))
    
class BuiltinFunction(Builtin, Function):
  memorable = _memorable
  def __call__(self, *exps):
    return CommandCall(self, *exps)
  
  def evaluate_arguments(self, solver, cont, exps):
    if len(exps)==0: 
      solver.scont = cont
      return ()
    else:
      @mycont(cont)
      def argument_cont(value, solver):
        @mycont(cont)
        def gather_cont(values, solver):
          solver.scont = cont
          return (value,)+values
        return self.evaluate_arguments(solver, gather_cont, exps[1:])
      solver.scont = solver.cont(exps[0], argument_cont)
      return True
      
  def apply(self, solver, cont, values, signatures):
    solver.scont = cont
    return self.function(*values)

  # compile
  
  def compile_to_cont(self, cont, compiler):
    return BuiltinFunctionCont(self, cont)
  
    
class BuiltinPredicateCont:
  def __init__(self, operator, operands):
    self.operator, self.operands = operator, operands
  def code(self):
    return '''
def builtin_predicate_fun():
  for values in apply_generator_fun_list([%s]):
    for x in %s(*values):
      yield x
for x in builtin_predicate_fun():
  print x
'''%(', '.join([code(x) for x in self.operands]), code(self.operator))
    
class BuiltinPredicate(Builtin, Function):
  memorable = _memorable
  def __call__(self, *exps):
    return CommandCall(self, *exps)
  def apply(self, solver, cont, values, signatures):
    return self.function(solver, cont, *values)
  
  def compile_to_cont(self, operands, compiler):
    return BuiltinPredicateCont(self, operands)
  
class BuiltinMacroCont:
  def __init__(self, operator, operands):
    self.operator, self.operands = operator, operands
  def code(self):
    return '''
def builtin_macro_fun():
  for x in %s(%s):
    yield x
for x in builtin_macro_fun():
  print x
'''%(
      code(self.operator), ', '.join([code(x) for x in self.operands]))
    
class BuiltinMacro(Builtin, Macro):
  memorable = _memorable
  def __call__(self, *exps):
    return CommandCall(self, *exps)
  def apply(self, solver, cont, exps, signatures):
    return self.function(solver, cont, *exps)
  def compile_to_cont(self, operands, compiler):
    return BuiltinMacroCont(self, operands)
    
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
