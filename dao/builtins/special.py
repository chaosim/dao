''' many lisp style special forms'''

from dao.base import Element
from dao.compilebase import CompileTypeError, VariableNotBound
from dao.command import special, Command, CommandCall, SpecialCall, Apply, Var, LogicVar
import dao.interlang as il
from dao.interlang import TRUE, FALSE, NONE, element

v0, fc0 = il.LocalVar('v'), il.LocalVar('fc')

@special
def quote(compiler, cont, exp):
  return cont(il.ExpressionWithCode(exp, il.Lamda((), exp.cps_convert(compiler, il.equal_cont))))

@special
def eval_(compiler, cont, exp):
  v = compiler.new_var(v0)
  return exp.cps_convert(compiler, il.clamda(v, cont(il.EvalExpressionWithCode(v))))

def begin(*exps):
  if len(exps)==1: return exps[0]
  else:
    result = []
    for exp in exps:
      if isinstance(exp, SpecialCall) and exp.function.func_name=='Begin':
        result += list(exp.args)
      else:
        result.append(exp)
    return Begin(*result)        
  
@special
def Begin(compiler, cont, *exps):
    return cps_convert_exps(compiler, exps, cont)

@special
def if_(compiler, cont, test, then, else_):
  v = compiler.new_var(v0)
  return test.cps_convert(compiler, 
           il.Clamda(v, il.If(v, then.cps_convert(compiler, cont), 
                                 else_.cps_convert(compiler, cont))))

def cps_convert_exps(compiler, exps, cont):
  v = compiler.new_var(v0)
  if not exps: return il.PassStatement()
  if len(exps)==1:
    return exps[0].cps_convert(compiler, cont)
  else:
    return exps[0].cps_convert(compiler, 
                  il.Clamda(v, cps_convert_exps(compiler, exps[1:], cont)))

from define import LamdaVar

@special
def callcc(compiler, cont, function):
  body = function.body.subst({function.params[0]: LamdaVar(function.params[0].name)})
  k = compiler.new_var(il.LocalVar('cont'))
  params = tuple(x.interlang() for x in function.params)
  function1 = il.Lamda((k,)+params, body.cps_convert(compiler, k))
  k1 = compiler.new_var(il.LocalVar('cont'))
  v = compiler.new_var(v0)
  return function1(cont, il.Lamda((k1, v), cont(v)))

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
    v1 = compiler.new_var(v0)
    v2 = compiler.new_var(v0)
    block_fun = compiler.new_var(il.LocalVar('block_'+self.label.name))
    compiler.exit_block_cont_map[self.label.name] = il.clamda(v1, cont(v1))
    compiler.continue_block_cont_map[self.label.name] = il.clamda(v2, block_fun(v2))
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
    return il.begin(compiler.protect_cont(NONE), 
                    compiler.exit_block_cont_map[self.label.name](self.value, ))

  def __repr__(self):
    return 'exit_block(%s, %s)'%(self.label, self.value)

def continue_block(label=NONE):
  return ContinueBlock(il.element(label))

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
    return il.begin(compiler.protect_cont(NONE), 
                    compiler.continue_block_cont_map[self.label.name](NONE))
  
  def __repr__(self):
    return 'continue_block(%s)'%(self.label)

@special
def catch(compiler, cont, tag, *form):
  v = compiler.new_var(il.LocalVar('v'))
  v2 = compiler.new_var(il.LocalVar('v'))
  return tag.cps_convert(compiler, il.clamda(v,
    il.PushCatchCont(v, il.clamda(v2,
      cont(v2))),
    begin(*form).cps_convert(compiler, cont)))
  
@special
def throw(compiler, cont, tag, form):
  v = compiler.new_var(il.LocalVar('v'))
  v2 = compiler.new_var(il.LocalVar('v'))
  return tag.cps_convert(compiler, 
      il.clamda(v,
          form.cps_convert(compiler, 
            il.clamda(v2, 
                      compiler.protect_cont(NONE),
                      il.FindCatchCont(v)(v2)))))
  
@special
def unwind_protect(compiler, cont, form, *cleanup):
  v = compiler.new_var(il.LocalVar('v'))
  v1 = compiler.new_var(il.LocalVar('v'))
  v2 = compiler.new_var(il.LocalVar('v'))
  old_protect_cont = compiler.protect_cont
  compiler.protect_cont = il.clamda(v, NONE)
  cleanup_protect = begin(*cleanup).cps_convert(compiler, old_protect_cont)
  compiler.protect_cont.body = cleanup_protect
  cleanup_cont = il.clamda(v1, begin(*cleanup).cps_convert(compiler, il.clamda(v2, cont(v1))))
  result = form.cps_convert(compiler, cleanup_cont)
  compiler.protect_cont = old_protect_cont
  return result