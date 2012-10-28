# -*- coding: utf-8 -*-

#from dao.base import classeq
import dao.interlang as il

v, fc = il.Var('v'), il.Var('fc')

class Compiler:
  def __init__(self):
    pass
    
  def compile(self, exp, cont, fcont):
    try: 
      exp_compile = exp.compile
    except: 
      return il.clamda(v, fc, cont(exp, fcont))
    return exp_compile(self, cont, fcont)
  
  def compile_exps(self, exps, cont, fcont):
    if not exps: return il.clamda(v, fc, cont(exps, fc))
    if len(exps)==1:
      return self.compile(exps[0], cont, fcont)
    else:
      return self.compile(exps[0], self.compile_exps(exps[1:], cont, fcont), fcont)

class special:
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    return SpecialCall(self.function, args)

class SpecialCall:
  def __init__(self, function, args):
    self.function, self.args = function, args
    
  def compile(self, compiler, cont, fcont):
    return self.function(compiler, cont, fcont, *self.args)
  
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
                      il.clamda(v, compiler.compile(clause2, cont, fcont))))

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
 