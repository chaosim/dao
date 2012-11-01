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
    
  def cps_convert(self, compiler, cont, fcont):
    return self.function(compiler, cont, fcont, *self.args)
  
@special
def quote(compiler, cont, fcont, exp):
    return cont(exp, fcont)
  
@special
def assign(compiler, cont, fcont, var, exp):
    return compiler.cps_convert(exp, 
            il.Clamda(v, fc, il.Assign(var, v), il.Return(v, fc)), fcont)
  
@special
def begin(compiler, cont, fcont, *exps):
    return compiler.cps_convert_exps(exps, cont, fcont)
    
@special
def if_(compiler, cont, fcont, test, then, else_):
  if else_ is None:
    return compiler.cps_convert(test, 
           il.Clamda(v, fc, il.If2(v, compiler.cps_convert(then, cont, fcont))))
  else:
    return compiler.cps_convert(test, 
           il.Clamda(v, fc, il.If(v, compiler.cps_convert(then, cont, fcont), 
                                    compiler.cps_convert(else_, cont, fcont))),
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
  return il.Clamda(v, fc, compiler.cps_convert(clause1, cont, 
                      il.Clamda(v, fc, compiler.cps_convert(clause2, cont, fcont))))

@special
def unify(compiler, cont, fcont, x, y):
  try: x_cps_convert_unify = x.cps_convert_unify
  except:
    try: y_cps_convert_unify = y.cps_convert_unify
    except:
      if x==y: return cont
      else: return fcont
    return y_cps_convert_unify(x, cont, fcont)
  return x_cps_convert_unify(y, cont, fcont)

  
class BuiltinFunction(Command):
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    return BuiltinFunctionCall(self.function, args)
  
class BuiltinFunctionCall(CommandCall):
  def cps_convert(self, compiler, cont, fcont):
    #see The 90 minute Scheme to C compiler by Marc Feeley
    args = self.args
    vars = tuple(il.Var('a'+repr(i)) for i in range(len(args)))
    #fun = il.Return(cont(self.function(il.il_tuple(*vars)), fc))
    fun = il.Return(cont(self.function(vars), fc))
    for var, arg in reversed(zip(vars, args)):
      fun = compiler.cps_convert(arg, il.Clamda(var, fc, fun), fcont)
    return fun
     
add = BuiltinFunction(il.add)

LogicVar = il.LogicVar

lamda = il.Lamda