''' many lisp style special forms'''

from dao.base import Element, classeq
from dao.compilebase import CompileTypeError, VariableNotBound
from dao.command import special, Command, CommandCall, SpecialCall, Apply
from dao.command import Var, LogicVar, Const, LamdaVar, ConstLamdaVar, MacroVar, ConstMacroVar
from dao.command import element, Cons, Assign
from special import begin
import dao.interlang as il
from dao.interlang import TRUE, FALSE, NONE

def lamda(params, *body):
  body = tuple(element(x) for x in body)
  return Lamda(params, begin(*body))

class Lamda(Element):
  def __init__(self, params, body):
    self.params, self.body = params, body
    
  def __eq__(x, y):
    return classeq(x, y) and x.params==y.params and x.body==y.body
  
  def __repr__(self):
    return 'Lamda((%s), %s)'%(', '.join([repr(x) for x in self.params]),
                              repr(self.body))
  
  def new(self, params, body):
    return self.__class__(params, body)
  
  def __call__(self, *args):
    return Apply(self, tuple(element(arg) for arg in args))
  
  def alpha(self, env, compiler):
    new_env = env.extend()
    for p in self.params: 
      new_env.bindings[p] = compiler.new_var(p)
    self.params = tuple(new_env[p] for p in self.params)
    self.body = self.body.alpha(new_env, compiler)
    self.variables = new_env.bindings.values()
    return self
    
  def has_cut(self):
    return has_cut(self.body)

  def cps(self, compiler, cont):
    k = compiler.new_var(il.ConstLocalVar('cont'))
    params = tuple(x.interlang() for x in self.params)
    if self.has_cut():
      body = wrap_cut(self.body).cps(compiler, k)
    else:
      body = self.body.cps(compiler, k)
    return cont(il.Lamda((k,)+params, body))
  
  def cps_call(self, compiler, cont, args):
    # see The 90 minute Scheme to C compiler by Marc Feeley
    if self.has_cut():
      fun = wrap_cut(self.body).cps(compiler, cont)
    else:
      fun = self.body.cps(compiler, cont)
    params = tuple(x.interlang() for x in self.params)
    for var, arg in reversed(zip(params, args)):
      fun = arg.cps(compiler, il.Clamda(var, fun))
    return fun
  
  def interlang(self):
    return il.Lamda(tuple(x.interlang() for x in self.params), self.body.interlang())

def macro(params, *body):
  body = tuple(element(x) for x in body)
  return Macro(params, begin(*body))

@special
def eval_macro_args(compiler, cont, exp):
  v = compiler.new_var(il.ConstLocalVar('v'))
  return il.Call(exp.interlang(), cont)
  #return cont(il.Call(exp.interlang()))

class Macro(Element):
  def __init__(self, params, body):
    self.params, self.body = params, body
    
  def __eq__(x, y):
    return classeq(x, y) and x.params==y.params and x.body==y.body
  
  def __repr__(self):
    return 'Macro((%s), %s)'%(', '.join([repr(x) for x in self.params]),
                              repr(self.body))
  
  def new(self, params, body):
    return self.__class__(params, body)
  
  def __call__(self, *args):
    return Apply(self, tuple(element(arg) for arg in args))
  
  def alpha(self, env, compiler):
    new_env = env.extend()
    for p in self.params: 
      new_env.bindings[p] = compiler.new_var(p)
    self.params = tuple(new_env[p] for p in self.params)
    for var, new_var in new_env.bindings.items():
      new_env.bindings[var] = eval_macro_args(new_var)
    self.body = self.body.alpha(new_env, compiler)
    return self
    
  def has_cut(self):
    return has_cut(self.body)

  def cps(self, compiler, cont):
    k = compiler.new_var(il.ConstLocalVar('cont'))
    params = tuple(x.interlang() for x in self.params)
    if self.has_cut():
      body = wrap_cut(self.body).cps(compiler, k)
    else:
      body = self.body.cps(compiler, k)
    return cont(il.Lamda((k,)+params, body))
  
  def cps_call(self, compiler, cont, args):
    # see The 90 minute Scheme to C compiler by Marc Feeley
    if self.has_cut():
      fun = wrap_cut(self.body).cps(compiler, cont)
    else:
      fun = self.body.cps(compiler, cont)
    params = tuple(x.interlang() for x in self.params)
    for var, arg in reversed(zip(params, args)):
      k = compiler.new_var(il.ConstLocalVar('cont'))
      fun = direct_interlang(il.Lamda((k, ), arg.cps(compiler, k))
                             ).cps(compiler, il.Clamda(var, fun))
    return fun
  
  def interlang(self):
    return il.Lamda(tuple(x.interlang() for x in self.params), self.body.interlang())

def let(bindings, *body):
  bindings = tuple((var, element(value)) for var, value in bindings)
  return Let(bindings, begin(*body))

class Let(il.Element):
  def __init__(self, bindings, body):
    self.bindings = bindings
    self.body = body
  
  def alpha(self, env, compiler):
    new_env = env.extend()
    for var, value in self.bindings:
      if isinstance(value, Rules):
        if not isinstance(var, Const):
          new_var = compiler.new_var(LamdaVar(var.name))
        else:
          new_var = compiler.new_var(ConstLamdaVar(var.name))
      elif isinstance(value, Lamda):
        if not isinstance(var, Const):
          new_var = compiler.new_var(LamdaVar(var.name))
        else:
          new_var = compiler.new_var(ConstLamdaVar(var.name))
      elif isinstance(value, MacroRules):
        if not isinstance(var, Const):
          new_var = compiler.new_var(MacroVar(var.name))
        else:
          new_var = compiler.new_var(ConstMacroVar(var.name))
      else:
        new_var = compiler.new_var(var)
      if isinstance(var, Const):
        new_var.assigned = True
      env[var ]  = new_var
    alphaed_body = self.body.alpha(new_env, compiler)
    assign_bindings = tuple(Assign(new_env[var], value.alpha(env, compiler)) 
                            for var, value in self.bindings)
    return begin(*(assign_bindings+(alphaed_body,)))
  
  def subst(self, bindings):
    bindings = tuple((var.subst(bindings), value.subst(bindings))
                     for var, value in self.bindings)
    body = self.body.subst(bindings)
    return Let(bindings, body)
  
  def __repr__(self):
    return 'Let(%r, %r)'%(self.bindings, self.body)
    
def letrec(bindings, *body):
  return Letrec(tuple((element(var), element(value)) for var, value in bindings), 
                begin(*tuple(element(exp) for exp in body)))

from dao.command import RecursiveFunctionVar, RecursiveMacroVar
class Letrec(Element):
  def __init__(self, bindings, body):
    self.bindings = bindings
    self.body = body
  
  def alpha(self, env, compiler):
    new_env = env.extend()
    for var, value in self.bindings:
      if isinstance(value, Rules):
        new_var = compiler.new_var(RecursiveFunctionVar(var.name))
      elif isinstance(value, Lamda):
        new_var = compiler.new_var(RecursiveFunctionVar(var.name))
      elif isinstance(value, Macro):
        new_var = compiler.new_var(RecursiveMacroVar(var.name))
      elif isinstance(value, MacroRules):
        new_var = compiler.new_var(RecursiveMacroVar(var.name))
      else:
        new_var = compiler.new_var(var)
      if isinstance(new_var, Const):
        new_var.assigned = True
      new_env.bindings[var] = new_var
    return begin(*(tuple(Assign(new_env[var], value.alpha(new_env, compiler)) 
                         for var, value in self.bindings)
                   +(self.body.alpha(new_env, compiler),)))
  
  def __repr__(self):
    return 'Letrec(%r, %r)'%(self.bindings, self.body)
  
from dao.builtins.control import or_
from dao.builtins.term import unify

from dao.command import direct_interlang

def get_tuple_vars(exps):
  result = set()
  for x in exps:
    result |= x.vars()
  return result

def rules(*rules):
  result = []
  for rule in rules:
    head = tuple(element(x) for x in rule[0])
    body = begin(*(element(x) for x in rule[1:]))
    result.append((head, body))
  return Rules(result)

@special
def wrap_cut(compiler, cont, exp):
  cut_cont = il.ConstLocalVar('cut_cont')
  v = il.ConstLocalVar('v')
  v1 = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  parse_state = compiler.new_var(il.ConstLocalVar('parse_state'))
  bindings = compiler.new_var(il.LocalVar('bindings'))
  fc = compiler.new_var(il.ConstLocalVar('old_failcont'))
  return il.begin(
    il.Assign(cut_cont, il.cut_cont),
    il.Assign(parse_state, il.parse_state),
    il.Assign(bindings, il.Copy(il.bindings)),
    il.Assign(fc, il.failcont),
    il.SetCutCont(il.clamda(v2, 
        il.Assign(il.parse_state, parse_state),
        il.SetBindings(bindings),
        fc(il.FALSE))),
    il.SetFailCont(il.clamda(v1,
      il.SetFailCont(fc),
      il.Assign(il.cut_cont, cut_cont),
      fc(il.FALSE))),
    exp.cps(compiler, il.clamda(v, 
      il.Assign(il.cut_cont, cut_cont),                         
      cont(v))))

from control import has_cut

class Rules(Lamda):
  def __init__(self, rules):
    self.rules = rules
    self.arity = len(rules[0][0])

  def __call__(self, *args):
    return Apply(self, tuple(element(arg) for arg in args))
  
  def alpha(self, env, compiler):
    rules = []
    for head, body in self.rules:
      head, new_env = alpha_rule_head(head, env, compiler)
      body = body.alpha(new_env, compiler)
      rules.append((head, body))
    return Rules(rules)
  
  def has_cut(self):
    for head, body in self.rules:
      if has_cut(body):
        return True
    return False
    
  def cps_call(self, compiler, cont, args):
    if len(args) != self.arity: raise ArityError
    clauses = []
    for head, body in self.rules:
      clauses.append(begin(unify_rule_head(args, head), body))
    if self.has_cut():
      return wrap_cut(or_(*clauses)).cps(compiler, cont)
    else:
      return or_(*clauses).cps(compiler, cont)
  
  def cps(self, compiler, cont):
    k = compiler.new_var(il.ConstLocalVar('cont'))
    params = tuple([compiler.new_var(Const('arg')) 
                    for x in range(self.arity)])
    clauses = []
    for head, body in self.rules:
      head_exps = begin(*tuple(unify_head_item2(param, head_item) 
                          for param, head_item in zip(params, head)))
      clauses.append(begin(head_exps, body))
    if self.has_cut():
      body = wrap_cut(or_(*clauses)).cps(compiler, k)
    else:
      body = or_(*clauses).cps(compiler, k)
    params = tuple(param.interlang() for param in params)
    return cont(il.Lamda((k,)+params, body))
  
  def __repr__(self):
    return 'rules(%s)'%repr(self.rules)
    
def alpha_rule_head(head, env, compiler):
  new_env = env.extend()
  head2 = []
  for item in head:
    head2.append(alpha_rule_head_item(item, new_env, compiler))
  return tuple(head2), new_env

def alpha_rule_head_item(item, env, compiler):
  if isinstance(item, Var):
    if isinstance(item, Const):
      env.bindings[item] = result = compiler.new_var(Const(item.name))
    else:
      env.bindings[item] = result = compiler.new_var(Var(item.name))
    return result
  else:
    if isinstance(item, Cons):
      return Cons(alpha_rule_head_item(item.head, env, compiler),
                  alpha_rule_head_item(item.tail, env, compiler))
    else:
      return item
    
def unify_rule_head(args, head):
  return il.begin(*tuple(unify_head_item1(arg, head_item) 
                      for (arg, head_item) in zip(args, head)))

def unify_head_item1(arg, head_item):
  # for direct call
  if isinstance(head_item, Var):
    if not isinstance(head_item, LogicVar):
      return Assign(head_item, arg)
    else: 
      raise CompileTypeError(head_item)
  else: 
    if isinstance(head_item, Cons):
      raise CompileTypeError(head_item)
    else:
      return unify(arg, head_item)

def do_unify_head_item2(arg, head_item, compiler, cont):
  if not isinstance(head_item, Var):
    # arg should be Var or il.ConsHead
    if isinstance(head_item, Cons):
      arg1 = compiler.new_var(il.ConstLocalVar('arg'))
      return il.begin(il.Assign(arg1, il.Deref(arg)),
                      do_unify_head_item2(il.ConsHead(arg1), head_item.head, compiler, 
                            do_unify_head_item2(il.ConsTail(arg), head_item.tail, compiler, cont)))
    else:
      head_item = head_item.interlang()
      arg = arg.interlang()
      arg1 = compiler.new_var(il.ConstLocalVar('arg'))
      return il.begin(
        il.Assign(arg1, il.Deref(arg)),
        il.If(il.IsLogicVar(arg1),
           il.begin(il.SetBinding(arg1, head_item),
                 il.append_failcont(compiler, il.DelBinding(arg1)),
                 cont(il.TRUE)),
                il.If(il.Eq(arg1, head_item), cont(TRUE), il.failcont(TRUE))))
  else:
    if not isinstance(head_item, LogicVar):
      arg = arg.interlang()
      return il.begin(
        il.Assign(head_item.interlang(), arg),
        cont(il.TRUE))
    else:
      raise CompileTypeError
  
@special
def unify_head_item2(compiler, cont, arg, head_item):
  # for call with rules variable.
  arg = arg.interlang()
  if not isinstance(head_item, Var):
    # arg should be Var
    if isinstance(head_item, Cons):
      v = compiler.new_var(il.ConstLocalVar('v'))
      return do_unify_head_item2(il.ConsHead(arg), head_item.head, compiler, 
                            il.clamda(v, 
                            do_unify_head_item2(il.ConsTail(arg), head_item.tail, 
                                                compiler, cont)))
    else:
      head_item = head_item.interlang()
      arg1 = compiler.new_var(il.ConstLocalVar('arg'))
      return il.begin(
        il.Assign(arg1, il.Deref(arg)),
        il.If(il.IsLogicVar(arg1),
           il.begin(il.SetBinding(arg1, head_item),
                 il.append_failcont(compiler, il.DelBinding(arg1)),
                 cont(il.TRUE)),
                il.If(il.Eq(arg1, head_item), cont(TRUE), il.failcont(TRUE))))
  else:
    if not isinstance(head_item, LogicVar):
      return il.begin(
        il.Assign(head_item.interlang(), arg),
        cont(il.TRUE))
    else:
      raise CompileTypeError
  
from dao.command import expression_with_code

def macrorules(*rules):
  result = []
  for rule in rules:
    head = tuple(element(x) for x in rule[0])
    body = begin(*(element(x) for x in rule[1:]))
    result.append((head, body))
  return MacroRules(result)

from special import eval_

class MacroRules(Element):
  def __init__(self, rules):
    self.rules = rules
    self.arity = len(rules[0][0])

  def __call__(self, *args):
    return Apply(self, tuple(element(arg) for arg in args))
  
  def alpha(self, env, compiler):
    result = []
    for head, body in self.rules:
      head, new_env = alpha_rule_head(head, env, compiler)
      for var, new_var in new_env.bindings.items():
        new_env.bindings[var] = eval_macro_args(new_var)
      body = body.alpha(new_env, compiler)
      result.append((head, body))
    return MacroRules(result)

  def has_cut(self):
    for head, body in self.rules:
      if has_cut(body):
        return True
    return False

  def cps_call(self, compiler, cont, args):
    if self.arity!=len(args): raise
    clauses = []
    for head, body in self.rules:
      clauses.append(begin(unify_macro_head(compiler, cont, args, head), body))
    if self.has_cut():
      return wrap_cut(or_(*clauses)).cps(compiler, cont)
    else:
      return or_(*clauses).cps(compiler, cont)
  
  def cps(self, compiler, cont):
    k = compiler.new_var(il.ConstLocalVar('cont'))
    params = tuple([compiler.new_var(il.ConstLocalVar('arg')) 
                    for x in range(self.arity)])
    clauses = []
    for head, body in self.rules:
      head_exps = begin(*tuple(unify_macro_head_item2(param, head_item) 
                           for param, head_item in zip(params, head)))
      clauses.append(begin(head_exps, body))
    if self.has_cut():
      body = wrap_cut(or_(*clauses)).cps(compiler, k)
    else:  
      body = or_(*clauses).cps(compiler, k)
    return cont(il.Lamda((k,)+params, body))
    
  def __repr__(self):
    return 'macrorules(%s)'%repr(self.rules)

def unify_macro_head(compiler, cont, args, head):
  # for direct call
  return begin(*tuple(unify_macro_head_item1(compiler, cont, arg, head_item) 
                      for arg, head_item, in zip(args, head)))

def unify_macro_head_item1(compiler, cont, arg, head_item):
  # for direct call
  if isinstance(head_item, Var) and not isinstance(head_item, LogicVar):
    k = compiler.new_var(il.ConstLocalVar('cont'))
    return Assign(head_item, direct_interlang(il.Lamda((k, ), 
                  arg.cps(compiler, k))))
  else: 
    return unify(arg, head_item)

@special
def unify_macro_head_item2(compiler, cont, arg, head_item):
  if isinstance(head_item, Var):
    if not isinstance(head_item, LogicVar):
      return il.begin(
        il.Assign(head_item.interlang(), arg),
        cont(il.TRUE))
    else: 
      v = compiler.new_var(il.ConstLocalVar('v'))
      head_item = head_item.interlang()
      arg1 = compiler.new_var(il.ConstLocalVar('arg'))
      head1 = compiler.new_var(il.ConstLocalVar('head'))
      return il.begin(
        il.Assign(arg1, il.Deref(arg)), #for LogicVar, could be optimized when generate code.
        il.Assign(head1, il.Deref(head_item)),
        il.If(il.IsLogicVar(arg1),
           il.begin(il.SetBinding(arg1, head1),
                 il.append_failcont(compiler, il.DelBinding(arg1)),
                 cont(il.TRUE)),
           il.begin(
             il.If(il.IsLogicVar(head1),
                il.begin(il.SetBinding(head1, arg1),
                      il.append_failcont(compiler, il.DelBinding(head1)),
                      cont(il.TRUE)),
                il.If(il.Eq(arg1, head1), cont(il.TRUE), il.failcont(il.FALSE))))))
  else:    
    arg1 = compiler.new_var(il.ConstLocalVar('arg'))
    return il.begin(
      il.Assign(arg1, il.Deref(arg)),
      il.If(il.IsLogicVar(arg1),
         il.begin(il.SetBinding(arg1, head_item),
               il.append_failcont(compiler, il.DelBinding(arg1)),
               cont(il.TRUE)),
              il.If(il.Eq(arg1, head_item), cont(TRUE), il.failcont(TRUE))))