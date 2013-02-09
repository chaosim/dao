''' many lisp style special forms'''

from dao.base import Element
from dao.compilebase import CompileTypeError, VariableNotBound
from dao.command import special, Command, CommandCall, SpecialCall, Apply, Var, LogicVar, element
import dao.interlang as il
from dao.interlang import TRUE, FALSE, NONE

@special
def quote(compiler, cont, exp):
  return cont(il.ExpressionWithCode(exp, il.Lamda((), exp.cps(compiler, il.equal_cont))))

@special
def eval_(compiler, cont, exp):
  v = compiler.new_var(il.ConstLocalVar('v'))
  return exp.cps(compiler, il.clamda(v, cont(il.EvalExpressionWithCode(v))))

def begin(*exps):
  exps = tuple(element(e) for e in exps)
  if len(exps)==1: return exps[0]
  else:
    result = []
    for exp in exps:
      if isinstance(exp, SpecialCall) and exp.command is Begin:
        result += list(exp.args)
      else:
        result.append(exp)
    return Begin(*result)        
  
def Begin_fun(compiler, cont, *exps):
    return cps_convert_exps(compiler, exps, cont)

Begin = special(Begin_fun)

@special
def if_(compiler, cont, test, then, else_):
  v = compiler.new_var(il.ConstLocalVar('v'))
  return test.cps(compiler, 
           il.Clamda(v, il.If(v, then.cps(compiler, cont), 
                                 else_.cps(compiler, cont))))

def cps_convert_exps(compiler, exps, cont):
  v = compiler.new_var(il.ConstLocalVar('v'))
  if not exps: return il.PassStatement()
  if len(exps)==1:
    return exps[0].cps(compiler, cont)
  else:
    return exps[0].cps(compiler, 
                  il.Clamda(v, cps_convert_exps(compiler, exps[1:], cont)))

from define import LamdaVar

@special
def callcc(compiler, cont, function):
  body = function.body.subst({function.params[0]: LamdaVar(function.params[0].name)})
  k = compiler.new_var(il.ConstLocalVar('cont'))
  params = tuple(x.interlang() for x in function.params)
  function1 = il.Lamda((k,)+params, body.cps(compiler, k))
  k1 = compiler.new_var(il.ConstLocalVar('cont'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  return function1(cont, il.Lamda((k1, v), cont(v)))

@special
def callfc(compiler, cont, function):
  Todo_callfc_need_tests
  return function(il.failcont)

def block(label, *exps):
  return Block(label, begin(*tuple(element(x) for x in exps)))
               
class Block(Element):
  def __init__(self, label, body):
    self.label = label
    self.body = body
    
  def alpha(self, env, compiler):
    label = compiler.new_var(self.label)
    compiler.block_label_stack.append((self.label, label))
    body = self.body.alpha(env, compiler)
    compiler.block_label_stack.pop()
    return Block(label, body)
  
  def subst(self, bindings):
    return Block(self.label, self.body.subst(bindings))
  
  def cps(self, compiler, cont):
    # use cfunction, continue_block means recursive call.
    # tail recursive cfunction can be used to transform to while 1/break/continue.
    v = compiler.new_var(il.ConstLocalVar('v'))
    v1 = compiler.new_var(il.ConstLocalVar('v'))
    v2 = compiler.new_var(il.ConstLocalVar('v'))
    block_fun = compiler.new_var(il.ConstLocalVar('block_'+self.label.name))
    compiler.exit_block_cont_map[self.label.name] = il.clamda(v1, cont(v1))
    compiler.continue_block_cont_map[self.label.name] = il.clamda(v2, block_fun(v2))
    return il.cfunction(block_fun, v, self.body.cps(compiler, cont))(il.NONE)
  
  def __repr__(self):
    return 'Block(%s, %s)'%(self.label, self.body)
    
def exit_block(label=NONE, value=NONE):
  return ExitBlock(element(label), element(value))

class ExitBlock(il.Element):
  def __init__(self, label=NONE, value=NONE):
    self.label = label
    self.value = value
    
  def alpha(self, env, compiler):
    if self.label==NONE:
      label = compiler.get_inner_block_label(NONE)
    else:
      label = compiler.get_block_label(self.label)
    return ExitBlock(label, self.value.alpha(env, compiler))
  
  def cps(self, compiler, cont):
    v = compiler.new_var(il.ConstLocalVar('v'))
    return self.value.cps(compiler,
            il.clamda(v, compiler.protect_cont(NONE), 
                         compiler.exit_block_cont_map[self.label.name](v, )))

  def __repr__(self):
    return 'exit_block(%s, %s)'%(self.label, self.value)

def continue_block(label=NONE):
  return ContinueBlock(element(label))

class ContinueBlock(il.Element):
  def __init__(self, label=NONE):
    self.label = label
    
  def alpha(self, env, compiler):
    if self.label==NONE:
      label = compiler.get_inner_block_label(NONE)
    else:
      label = compiler.get_block_label(self.label)
    return ContinueBlock(label)
  
  def cps(self, compiler, cont):
    return il.begin(compiler.protect_cont(NONE), 
                    compiler.continue_block_cont_map[self.label.name](il.NONE))
  
  def __repr__(self):
    return 'continue_block(%s)'%(self.label)

@special
def catch(compiler, cont, tag, *form):
  v = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  k = compiler.new_var(il.LocalVar('cont'))
  return tag.cps(compiler, il.clamda(v,
    il.Assign(k, il.clamda(v2, cont(v2))),
    il.PushCatchCont(v, k),
    begin(*form).cps(compiler, cont)))
  
@special
def throw(compiler, cont, tag, form):
  v = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  return tag.cps(compiler, 
      il.clamda(v,
          form.cps(compiler, 
            il.clamda(v2, 
                      compiler.protect_cont(NONE),
                      il.FindCatchCont(v)(v2)))))
  
@special
def unwind_protect(compiler, cont, form, *cleanup):
  v = compiler.new_var(il.ConstLocalVar('v'))
  v1 = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  old_protect_cont = compiler.protect_cont
  compiler.protect_cont = il.clamda(v, NONE)
  cleanup_protect = begin(*cleanup).cps(compiler, old_protect_cont)
  compiler.protect_cont.body = cleanup_protect
  cleanup_cont = il.clamda(v1, begin(*cleanup).cps(compiler, il.clamda(v2, cont(v1))))
  result = form.cps(compiler, cleanup_cont)
  compiler.protect_cont = old_protect_cont
  return result