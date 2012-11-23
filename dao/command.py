# -*- coding: utf-8 -*-

from dao.base import classeq, Element

from dao.compilebase import CompileTypeError, VariableNotBound
from dao.interlang import TRUE, FALSE, NONE
import dao.interlang as il

v0, fc0 = il.LocalVar('v'), il.LocalVar('fc')

class Command(Element): pass

class Var(Element):        
  def __init__(self, name):
    self.name = name
        
  def __call__(self, *args):
    return Apply(self, tuple(il.element(arg) for arg in args))
  
  def alpha_convert(self, env, compiler):
    try: 
      return env[self]
    except VariableNotBound: 
      env[self] = result = compiler.new_var(LogicVar(self.name))
      return result
    
  def subst(self, bindings):  
    try: return bindings[self]
    except: return self
      
  def cps_convert(self, compiler, cont):
    return cont(il.Deref(il.Var(self.name)))
  
  def cps_convert_unify(x, y, compiler, cont):
    x = x.interlang()
    y = y.interlang()
    try: 
      y.cps_convert_unify
    except:
      x1 = compiler.new_var(il.LocalVar('x'))
      return il.begin(
        il.Assign(x1, il.Deref(x)), #for LogicVar, could be optimized when generate code.
        il.If(il.IsLogicVar(x1),
           il.begin(il.SetBinding(x1, y),
                 il.append_failcont(compiler, il.DelBinding(x1)),
                 cont(il.TRUE)),
                il.If(il.Eq(x1, y), cont(TRUE), il.failcont(TRUE))))
    x1 = compiler.new_var(il.LocalVar('x'))
    y1 = compiler.new_var(il.LocalVar('y'))
    return begin(
      il.Assign(x1, il.Deref(x)), #for LogicVar, could be optimized when generate code.
      il.Assign(y1, il.Deref(y)),
      il.If(il.IsLogicVar(x1),
         il.begin(il.SetBinding(x1, y1),
               il.append_failcont(compiler, il.DelBinding(x1)),
               cont(il.TRUE)),
         il.begin(
           il.If(il.IsLogicVar(y1),
              il.begin(il.SetBinding(y1, x1),
                    il.append_failcont(compiler, il.DelBinding(y1)),
                    cont(il.TRUE)),
              il.If(il.Eq(x1, y1), cont(il.TRUE), il.failcont(il.TRUE))))))
  
  def cps_convert_call(self, compiler, cont, args):
    # see The 90 minute Scheme to C compiler by Marc Feeley
    function = compiler.new_var(il.LocalVar('function'))
    vars = tuple(compiler.new_var(il.LocalVar('a'+repr(i))) for i in range(len(args)))
    fun = il.Apply(function, (cont,)+vars)
    for var, self in reversed(zip((function,)+vars, (self,)+args)):
      fun = self.cps_convert(compiler, il.Clamda(var, fun))
    return fun
  
  def interlang(self):
    return il.Var(self.name)
  
  def vars(self):
    return set([self])
  
  def __repr__(self):
    return self.name #enough in tests

class LogicVar(Var):  
  def alpha_convert(self, env, compiler):
    return self
  
  def interlang(self):
    return il.LogicVar(self.name)
  
  def cps_convert(self, compiler, cont):
    return cont(il.Deref(il.LogicVar(self.name)))
  
  def to_code(self, coder):
    return "DaoLogicVar('%s')"%self.name
  
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name

class Apply(Element):
  def __init__(self, caller, args):
    self.caller, self.args = caller, args

  def alpha_convert(self, env, compiler):
    return self.__class__(self.caller.alpha_convert(env, compiler), 
                 tuple(arg.alpha_convert(env, compiler) for arg in self.args))
  
  def cps_convert(self, compiler, cont):
    # see The 90 minute Scheme to C compiler by Marc Feeley
    return self.caller.cps_convert_call(compiler, cont, self.args)

  def subst(self, bindings):  
    return self.__class__(self.caller.subst(bindings), 
                 tuple(arg.subst(bindings) for arg in self.args))
      
  def __repr__(self):
    return '%r(%s)'%(self.caller, ', '.join([repr(x) for x in self.args]))
  
class Special(Command):
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    args = tuple(il.element(arg) for arg in args)
    return SpecialCall(self.function, args)
  
  def __repr__(self):
    return self.function.__name__

special = Special

def quasiquote_args(self, args):
  if not args: yield ()
  elif len(args)==1: 
    for x in self.quasiquote(args[0]):
      try: yield x.unquote_splice
      except: yield (x,)
  else:
    for x in self.quasiquote(args[0]):
      for y in self.quasiquote_args(args[1:]):
        try: x = x.unquote_splice
        except: x = (x,)
        yield x+y
          
class CommandCall(il.Element): 
  def __init__(self, function, args):
    self.function, self.args = function, args
    
  def subst(self, bindings):
    return self.__class__(self.function, 
                 tuple(arg.subst(bindings) for arg in self.args))
  
  def quasiquote(self, compiler, cont):
    result = compiler.new_var(il.LocalVar('result'))
    vars = tuple(compiler.new_var(il.LocalVar('a'+repr(i))) for i in range(len(self.args)))
    body = (il.Assign(result, il.empty_list),)+tuple(
      il.If(il.Isinstance(var, il.Klass('UnquoteSplice')),
                  il.AddAssign(result, il.Attr(var, il.Symbol('item'))),
                  il.ListAppend(result, var),
                  ) for var in vars)+(
      cont(il.Call(il.Klass(self.__class__.__name__), il.QuoteItem(self.function), il.MakeTuple(result))),)
    fun = il.begin(*body)
    for var, arg in reversed(zip(vars, self.args)):
      fun = arg.quasiquote(compiler, il.clamda(var, fun))
    return fun
    
  def __eq__(x, y):
    return classeq(x, y) and x.function==y.function and x.args==y.args
  
  def __repr__(self):
    return '%r(%s)'%(self.function, ', '.join([repr(x) for x in self.args]))

class SpecialCall(CommandCall):
    
  def cps_convert(self, compiler, cont):
    return self.function(compiler, cont, *self.args)
  
  def alpha_convert(self, env, compiler):
    return self.__class__(self.function,
                 tuple(arg.alpha_convert(env, compiler) for arg in self.args))
    
  def __repr__(self):
    return '%s(%s)'%(self.function.__name__, 
                     ', '.join(tuple(repr(x) for x in self.args)))

class BuiltinFunction(Command):
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    args = tuple(il.element(arg) for arg in args)
    return BuiltinFunctionCall(self.function, args)
  
  def cps_convert(self, compiler, cont):
    return il.Lamda((params), self.function(*params))
  
class BuiltinFunctionCall(CommandCall):
  def alpha_convert(self, env, compiler):
    return self.__class__(self.function,
                 tuple(arg.alpha_convert(env, compiler) for arg in self.args))
  
  def cps_convert(self, compiler, cont):
    #see The 90 minute Scheme to C compiler by Marc Feeley
    args = self.args
    vars = tuple(compiler.new_var(il.LocalVar('a'+repr(i))) for i in range(len(args)))
    fun = cont(self.function(*vars))
    for var, arg in reversed(zip(vars, args)):
      fun = arg.cps_convert(compiler, il.Clamda(var, fun))
    return fun

  def optimization_analisys(self, data):
    # unquote to interlang level
    return
  
  def to_code(self, coder):
    return 'BuiltinFunctionCall(%s, (%s))'%(repr(self.function), ', '.join([x.to_code(coder) for x in self.args]))
     
  def __repr__(self):
    return '%s(%s)'%(self.function.name, ', '.join([repr(x) for x in self.args]))
