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

class Assign(SpecialCall):
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
    
  def subst(self, bindings):
    return AssignCall(self.var, self.exp.subst(bindings))
    
  def alpha_convert(self, env, compiler):
    try: var = env[self.var]
    except VariableNotBound:
      env[self.var] = var = compiler.new_var(self.var)
    return AssignCall(var, self.exp.alpha_convert(env, compiler))
  
  def cps_convert(self, compiler, cont):
    v = compiler.new_var(v0)
    return self.exp.cps_convert(compiler, il.clamda(v, il.Assign(self.var, v), cont(NONE)))
    
  def __repr__(self):
    return 'assign(%r, %r)'%(self.var, self.exp)

assign = Assign

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
  return Block(label, begin(*tuple(element(x) for x in exps)))
               
class Block(il.Element):
  def __init__(self, label, body):
    self.label = label
    self.body = body
    
  def alpha_convert(self, env, compiler):
    compiler.block_level.append((self.label, compiler.new_var(self.label)))
    return self.body.alpha_convert(env, compiler)
  
  def cps_convert(self, compiler, cont):
    compiler.exit_block_cont_map[label] = cont
    compiler.next_block_cont_map[label] = result = self.body.cps_convert(compiler, cont)
    return result
    
class ExitBlock(il.Element):
  def __init__(self, label=NONE, value=NONE):
    self.label = label
    self.value = value
    
  def alpha_convert(self, env, compiler):
    if self.label==NONE:
      label = env.get_inner_block_label(NONE)
    else:
      label = env.get_block_label(self.label)
    return ExitBlock(label, self.value.alpha_convert(env, compiler))
  
  def cps_convert(self, compiler, cont):
    return compiler.exit_block_cont_map[self.label](self.value)
  
class ContinueBlock(il.Element):
  def __init__(self, label=NONE):
    self.label = label
    
  def alpha_convert(self, env, compiler):
    if self.label==NONE:
      label = env.get_inner_block_label(NONE)
    else:
      label = env.get_block_label(self.label)
    return ContinueBlock(label)
  
  def cps_convert(self, compiler, cont):
    return compiler.next_block_cont_map[self.label](NONE)