''' many lisp style special forms'''

from dao.command import special, Command, SpecialCall
import dao.interlang as il
from dao.compilebase import CompileTypeError, VariableNotBound
from dao.interlang import cps_convert_exps

from dao.interlang import TRUE, FALSE, NONE

v0, fc0 = il.Var('v'), il.Var('fc')

@special
def quote(compiler, cont, exp):
  return cont(exp)

@special
def eval_(compiler, cont, exp):
  return exp.cps_convert(compiler, il.Done()).cps_convert(compiler, cont)

def assign(var, exp):
  return Assign(il.element(var), il.element(exp))

class Assign(SpecialCall):
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
    
  def subst(self, bindings):
    return Assign(self.var, self.exp.subst(bindings))
    
  def alpha_convert(self, env, compiler):
    try: var = env[self.var]
    except VariableNotBound:
      env[self.var] = var = compiler.new_var(self.var)
    return Assign(var, self.exp.alpha_convert(env, compiler))
  
  def cps_convert(self, compiler, cont):
    v = compiler.new_var(v0)
    return self.exp.cps_convert(compiler, il.clamda(v, il.Assign(self.var, v), cont(NONE)))
    
  def __repr__(self):
    return 'assign(%r, %r)'%(self.var, self.exp)

@special
def begin(compiler, cont, *exps):
    return cps_convert_exps(compiler, exps, cont)
    
@special
def if_(compiler, cont, test, then, else_):
  v = compiler.new_var(v0)
  if else_ is None:
    return test.cps_convert(compiler, 
            il.Clamda(v, il.if2(v, then.cps_convert(compiler, cont))))
  else:
    return test.cps_convert(compiler, 
           il.Clamda(v, il.If(v, then.cps_convert(compiler, cont), 
                                 else_.cps_convert(compiler, cont))))

lamda = il.lamda

def let(bindings, *body):
  bindings = tuple((var, il.element(value)) for var, value in bindings)
  return Let(bindings, begin(*body))

class Let(il.Element):
  def __init__(self, bindings, body):
    self.bindings = bindings
    self.body = body
  
  def alpha_convert(self, env, compiler):
    new_env = env.extend()
    for var, value in self.bindings:
      new_env.bindings[var] = compiler.new_var(var)
    alphaed = self.body.alpha_convert(new_env, compiler) 
    bindings = {new_env[var]:value for var, value in self.bindings}
    return alphaed.subst(bindings)
  
  def subst(self, bindings):
    bindings = tuple((var.subst(bindings), value.subst(bindings))
                     for var, value in self.bindings)
    body = self.body.subst(bindings)
    return Let(bindings, body)
  
  def __repr__(self):
    return 'Let(%r, %r)'%(self.bindings, self.body)
    
def letrec(bindings, *body):
  return Letrec(bindings, body)

class Letrec(il.Element):
  def __init__(self, bindings, body):
    self.bindings = bindings
    self.body = body
  
  def alpha_convert(self, env, compiler):
    new_env = env.extend()
    for var, value in self.bindings:
      new_env.bindings[var] = compiler.new_var(var)
    body = begin(*(tuple(assign(var, value) for var, value in self.bindings)+self.body))
    return body.alpha_convert(new_env, compiler) 
  
  def __repr__(self):
    return 'Letrec(%r, %r)'%(self.bindings, self.body)

@special
def callcc(compiler, cont, function):
  return function(cont)

@special
def callfc(compiler, cont, function):
  Todo_callfc_need_tests
  return function(il.failcont)

def block(label, *exps):
  return Block(label, begin(*tuple(il.element(x) for x in exps)))
               
class Block(il.Element):
  def __init__(self, label, body):
    self.label = label
    self.body = body
    
  def alpha_convert(self, env, compiler):
    label = compiler.new_var(self.label)
    compiler.block_label_stack.append((self.label, label))
    body = self.body.alpha_convert(env, compiler)
    compiler.block_label_stack.pop()
    return Block(label, body)
  
  def subst(self, bindings):
    return Block(self.label, self.body.subst(bindings))
  
  def cps_convert(self, compiler, cont):
    # use cfunction, continue_block means recursive call.
    # tail recursive cfunction can be used to transform to while 1/break/continue.
    v = compiler.new_var(v0)
    compiler.exit_block_cont_map[self.label] = cont
    block_fun = compiler.new_var(il.Var('block_'+self.label.name))
    compiler.next_block_cont_map[self.label] = block_fun(NONE)
    return il.cfunction(block_fun, v, self.body.cps_convert(compiler, cont))(NONE)
  
  def __repr__(self):
    return 'Block(%s, %s)'%(self.label, self.body)
    
def exit_block(label=NONE, value=NONE):
  return ExitBlock(il.element(label), il.element(value))

class ExitBlock(il.Element):
  def __init__(self, label=NONE, value=NONE):
    self.label = label
    self.value = value
    
  def alpha_convert(self, env, compiler):
    if self.label==NONE:
      label = compiler.get_inner_block_label(NONE)
    else:
      label = compiler.get_block_label(self.label)
    return ExitBlock(label, self.value.alpha_convert(env, compiler))
  
  def cps_convert(self, compiler, cont):
    return compiler.exit_block_cont_map[self.label](self.value)

  def __repr__(self):
    return 'exit_block(%s, %s)'%(self.label, self.value)

def continue_block(label=NONE):
  return ContinueBlock(il.element(label))

class BackFilledContinueContinuation:
  def __init__(self, cont_map, label):
    self.cont_map = cont_map
    self.label = label
    
  def optimization_analisys(self, data):
    (self.cont_map[self.label](NONE)).optimization_analisys(data)
  
class ContinueBlock(il.Element):
  def __init__(self, label=NONE):
    self.label = label
    
  def alpha_convert(self, env, compiler):
    if self.label==NONE:
      label = compiler.get_inner_block_label(NONE)
    else:
      label = compiler.get_block_label(self.label)
    return ContinueBlock(label)
  
  def cps_convert(self, compiler, cont):
    return compiler.next_block_cont_map[self.label]
  
  def __repr__(self):
    return 'continue_block(%s, %s)'%(self.label)
