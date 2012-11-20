''' many lisp style special forms'''

from dao.base import Element
from dao.command import special, Command, SpecialCall, Apply, Var
from dao.compilebase import CompileTypeError, VariableNotBound
from dao.interlang import TRUE, FALSE, NONE, element
import dao.interlang as il

v0, fc0 = il.Var('v'), il.Var('fc')

@special
def quote(compiler, cont, exp):
  return cont(il.Expression(exp))

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
    return self.exp.cps_convert(compiler, il.clamda(v, il.Assign(self.var.interlang(), v), cont(NONE)))
    
  def __repr__(self):
    return 'assign(%r, %r)'%(self.var, self.exp)

def begin(*exps):
  if len(exps)==1: return exps[0]
  else:
    result = []
    for exp in exps:
      if isinstance(exp, SpecialCall) and exp.function.func_name=='_begin':
        result += list(exp.args)
      else:
        result.append(exp)
    return _begin(*result)        
  
@special
def _begin(compiler, cont, *exps):
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

def cps_convert_exps(compiler, exps, cont):
  v = compiler.new_var(il.Var('v'))
  if not exps: return Clamda(v, cont(il.tuple()))
  if len(exps)==1:
    return exps[0].cps_convert(compiler, cont)
  else:
    return exps[0].cps_convert(compiler, 
                  il.Clamda(v, cps_convert_exps(compiler, exps[1:], cont)))

def lamda(params, *body):
  body = tuple(il.element(x) for x in body)
  return Lamda(params, begin(*body))

class Lamda(Element):
  def __init__(self, params, body):
    self.params, self.body = params, body
    
  def __repr__(self):
    return 'Lamda((%s), %s)'%(', '.join([repr(x) for x in self.params]),
                              repr(self.body))
  
  def new(self, params, body):
    return self.__class__(params, body)
  
  def __call__(self, *args):
    return Apply(self, tuple(il.element(arg) for arg in args))
  
  def alpha_convert(self, env, compiler):
    try:
      self.before_alpha_convert
      return self
    except: self.before_alpha_convert  = self.params, self.body
    
    new_env = env.extend()
    for p in self.params: 
      new_env.bindings[p] = compiler.new_var(p)
    self.params = tuple(new_env[p] for p in self.params)
    self.body = self.body.alpha_convert(new_env, compiler)
    self.variables = new_env.bindings.values()
    return self
    
  def cps_convert(self, compiler, cont):
    k = compiler.new_var(il.Var('cont'))
    params = tuple(x.interlang() for x in self.params)
    return cont(il.Lamda((k,)+params, self.body.cps_convert(compiler, k)))
  
  def cps_convert_call(self, compiler, cont, args):
    # see The 90 minute Scheme to C compiler by Marc Feeley
    fun = self.body.cps_convert(compiler, cont)
    params = tuple(x.interlang() for x in self.params)
    for var, arg in reversed(zip(params, args)):
      fun = arg.cps_convert(compiler, il.Clamda(var, fun))
    return fun
  
  def interlang(self):
    return il.Lamda(tuple(x.interlang() for x in self.params), self.body.interlang())

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
    alphaed_body = self.body.alpha_convert(new_env, compiler)
    assign_bindings = tuple(assign(new_env[var], value) for var, value in self.bindings)
    return begin(*(assign_bindings+(alphaed_body,)))
  
  def subst(self, bindings):
    bindings = tuple((var.subst(bindings), value.subst(bindings))
                     for var, value in self.bindings)
    body = self.body.subst(bindings)
    return Let(bindings, body)
  
  def __repr__(self):
    return 'Let(%r, %r)'%(self.bindings, self.body)
    
def letrec(bindings, *body):
  return Letrec(bindings, body)

class Letrec(Element):
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
  
def alpha_rule_head(head, env, compiler):
  new_env = env.extend()
  head2 = []
  for item in head:
    if isinstance(item, Var):
      new_env.bindings[item] = new_var = compiler.new_var(item)
      head2.append(new_var)
    else:
      head2.append(item)
  return tuple(head2), new_env
      
def rules(*rules):
  result = []
  for rule in rules:
    head = tuple(element(x) for x in rule[0])
    body = begin(*(element(x) for x in rule[1:]))
    result.append((head, body))
  return Rules(result)

from dao.builtins.control import or_
from dao.builtins.term import unify

def unify_list(list1, list2):
  return begin(*tuple(unify(x, y) for x, y, in zip(list1, list2)))
    
class Rules(Element):
  def __init__(self, rules):
    self.rules = rules

  def __call__(self, *args):
    clauses = []
    for head, body in self.rules:
      if len(head)==len(args):
        clauses.append(begin(unify_list(args, head), body))
    return or_(*clauses)
  
  def alpha_convert(self, env, compiler):
    rules = []
    for head, body in self.rules:
      head, new_env = alpha_rule_head(head, env, compiler)
      body = body.alpha_convert(env, compiler)
      rules.append((head, body))
    return Rules(rules)
      
  def cps_convert(self, compiler, cont):
    k = compiler.new_var(il.Var('cont'))
    params = compiler.new_var(il.Var('params'))
    v = compiler.new_var(il.Var('v'))
    failcont = il.failcont
    for head, body in reversed(self.rules):
      rule_body = unify(head, params, il.clamda(v, self.body.cps_convert(compiler, k)), failcont)
      failcont = il.clamda(v, rule_body)
    return cont(il.Lamda((k,)+params, rule_body))
  
  #def cps_convert_call(self, compiler, cont, args):
    #clauses = []
    #for head, body in self.rules:
      #if len(head)==len(args):
        #clauses.append(begin(unify_list(args, head), body))
    #return or_(*clauses)
  
@special
def callcc(compiler, cont, function):
  k = compiler.new_var(il.Var('cont'))
  params = tuple(x.interlang() for x in function.params)
  function1 = il.Lamda((k,)+params, function.body.cps_convert(compiler, k))
  k1 = compiler.new_var(il.Var('cont'))
  v = compiler.new_var(il.Var(v0))
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
    old_unwind_cont_stack_length = compiler.new_var(il.Var('old_unwind_cont_stack_length'))
    block_fun = compiler.new_var(il.Var('block_'+self.label.name))
    return il.cfunction(block_fun, v,
                il.Assign(old_unwind_cont_stack_length, il.unwind_cont_stack_length),
                il.SetExitBlockContMap(il.String(self.label.name),  il.clamda(v1, 
                      il.Unwind(old_unwind_cont_stack_length), cont(v1))),
                il.SetContinueBlockContMap(il.String(self.label.name),  il.clamda(v2, 
                      il.Unwind(old_unwind_cont_stack_length), block_fun(v2))),
                self.body.cps_convert(compiler, cont))(NONE)
  
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
    return il.GetExitBlockCont(il.String(self.label.name))(self.value)

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
    return il.GetContinueBlockCont(il.String(self.label.name))(NONE)
  
  def __repr__(self):
    return 'continue_block(%s, %s)'%(self.label)

@special
def catch(compiler, cont, tag, *form):
  v = compiler.new_var(il.Var('v'))
  v2 = compiler.new_var(il.Var('v'))
  old_unwind_cont_stack_length = compiler.new_var(il.Var('old_unwind_cont_stack_length'))
  return tag.cps_convert(compiler, il.clamda(v,
    il.Assign(old_unwind_cont_stack_length, il.unwind_cont_stack_length),
    il.PushCatchCont(v, il.clamda(v2,
      il.Unwind(old_unwind_cont_stack_length),
      #il.PopCatchCont(v), # do not pop here, when throw to find, the cont is popped.
      cont(v2))),
    begin(*form).cps_convert(compiler, cont)))
  
@special
def throw(compiler, cont, tag, form):
  v = compiler.new_var(il.Var('v'))
  v2 = compiler.new_var(il.Var('v'))
  return tag.cps_convert(compiler, 
      il.clamda(v,
          form.cps_convert(compiler, 
            il.clamda(v2, il.FindCatchCont(v)(v2)))))
  
@special
def unwind_protect(compiler, cont, form, *cleanup):
  v = compiler.new_var(il.Var('v'))
  v1 = compiler.new_var(il.Var('v'))
  v2 = compiler.new_var(il.Var('v'))
  protect_cont = compiler.new_var(il.Var('protect_cont'))
  return il.clamda(v,
    il.Assign(protect_cont, 
      il.clamda(v1, 
          il.pop_unwind_cont,
          begin(*cleanup).cps_convert(compiler, il.clamda(v2, cont(v1))))),
    il.PushUnwindCont(protect_cont),
    form.cps_convert(compiler, protect_cont))(NONE)