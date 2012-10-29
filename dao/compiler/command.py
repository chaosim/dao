# -*- coding: utf-8 -*-

import dao.compiler.interlang as il

v, fc = il.Var('v'), il.Var('fc')

class Command: pass

class special(Command):
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    return SpecialCall(self.function, args)

class CommandCall: 
  def __init__(self, function, args):
    self.function, self.args = function, args

class SpecialCall(CommandCall):
    
  def compile(self, compiler, cont, fcont):
    return self.function(compiler, cont, fcont, *self.args)
  
@special
def quote(compiler, cont, fcont, exp):
    return cont(exp, fcont)
  
@special
def assign(compiler, cont, fcont, var, exp):
    return compiler.compile(exp, 
            il.clamda(v, fc, il.assign(var, v), il.ret(v, fc)), fcont)
  
@special
def begin(compiler, cont, fcont, *exps):
    return compiler.compile_exps(exps, cont, fcont)
    
@special
def if_(compiler, cont, fcont, test, then, else_):
  if else_ is None:
    return compiler.compile(test, 
           il.clamda(v, fc, il.if_(v, compiler.compile(then, cont, fcont))))
  else:
    return compiler.compile(test, 
           il.clamda(v, fc, il.if_(v, compiler.compile(then, cont, fcont), 
                                    compiler.compile(else_, cont, fcont))),
           fcont)
    
@special
def succeed(compiler, cont, fcont):
  return cont

succeed = succeed()

@special
def fail(compiler, cont, fcont):
  return fcont

fail = fail()

@special
def or_(compiler, cont, fcont, clause1, clause2):
  return il.clamda(v, fc, compiler.compile(clause1, cont, 
                      il.clamda(v, fc, compiler.compile(clause2, cont, fcont))))

@special
def unify(compiler, cont, fcont, x, y):
  try: x_compile_unify = x.compile_unify
  except:
    try: y_compile_unify = y.compile_unify
    except:
      if x==y: return cont
      else: return fcont
    return y_compile_unify(x, compiler, cont, fcont)
  return x_compile_unify(y, compiler, cont, fcont)

class LogicVar: 
  def __init__(self, name):
    self.name = name
    
  def compile_unify(x, y, compiler, cont, fcont):
    return il.clamda(v, fc, 
            il.ret(il.unify(x, y, cont, fcont)))
  
  def __repr__(self): return self.name
  
class BuiltinFunction(Command):
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    return BuiltinFunctionCall(self.function, args)
  
class BuiltinFunctionCall(CommandCall):
  def compile(self, compiler, cont, fcont):
    #see The 90 minute Scheme to C compiler by Marc Feeley
    args = self.args
    vars = tuple(il.Var('a'+repr(i)) for i in range(len(args)))
    fun = il.ret(cont(self.function(vars), fc))
    for var, arg in reversed(zip(vars, args)):
      fun = compiler.compile(arg, il.clamda(var, fc, fun), fcont)
    return fun
     
add = BuiltinFunction(il.add)
