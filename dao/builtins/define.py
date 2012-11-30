''' many lisp style special forms'''

from dao.base import Element, classeq
from dao.command import special, Command, CommandCall, SpecialCall, Apply, Var, LogicVar, Assign
from dao.compilebase import CompileTypeError, VariableNotBound
from dao.interlang import TRUE, FALSE, NONE, element
import dao.interlang as il
from special import begin

v0, fc0 = il.LocalVar('v'), il.LocalVar('fc')

def lamda(params, *body):
  body = tuple(il.element(x) for x in body)
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
    k = compiler.new_var(il.LocalVar('cont'))
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
    assign_bindings = tuple(Assign(new_env[var], value.alpha_convert(env, compiler)) 
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

class RecursiveFunctionVar(Var):
  def cps_convert(self, compiler, cont):
    return cont(il.RecursiveFunctionVar(self.name))
  
  def interlang(self):
    return il.RecursiveFunctionVar(self.name)
  
  def __repr__(self):
    return 'RecursiveFunctionVar(%s)'%self.name

class Letrec(Element):
  def __init__(self, bindings, body):
    self.bindings = bindings
    self.body = body
  
  def alpha_convert(self, env, compiler):
    new_env = env.extend()
    for var, value in self.bindings:
      if isinstance(value, Lamda):
        new_env.bindings[var] = compiler.new_var(RecursiveFunctionVar(var.name))
      else:
        new_env.bindings[var] = compiler.new_var(var)
    return begin(*(tuple(Assign(new_env[var], value.alpha_convert(new_env, compiler)) 
                         for var, value in self.bindings)
                   +(self.body.alpha_convert(new_env, compiler),)))
  
  def __repr__(self):
    return 'Letrec(%r, %r)'%(self.bindings, self.body)
  
from dao.builtins.control import or_
from dao.builtins.term import unify

def direct_interlang(*exps):
  return DirectInterlang(il.begin(*exps))

class DirectInterlang(Element):
  def __init__(self, body):
    self.body = body
  
  def alpha_convert(self, env, compiler):
    return self
  
  def cps_convert(self, compiler, cont):
    return cont(self.body)
  
def get_tuple_vars(exps):
  result = set()
  for x in exps:
    result |= x.vars()
  return result

def rules(*rules):
  result = {}
  for rule in rules:
    head = tuple(element(x) for x in rule[0])
    body = begin(*(element(x) for x in rule[1:]))
    result.setdefault(len(head), []).append((head, body))
  return Rules(result)

class Rules(Element):
  def __init__(self, rules):
    self.rules = rules

  def __call__(self, *args):
    return Apply(self, tuple(il.element(arg) for arg in args))
  
  def alpha_convert(self, env, compiler):
    rules1 = {}
    for arity, rules in self.rules.items():
      result = []
      for head, body in rules:
        head, new_env = alpha_rule_head(head, env, compiler)
        body = body.alpha_convert(new_env, compiler)
        result.append((head, body))
      rules1[arity] = result
    return Rules(rules1)
      
  def cps_convert_call(self, compiler, cont, args):
    clauses = []
    if len(args) not in self.rules:
      return il.failcont
    for head, body in self.rules[len(args)]:
      clauses.append(begin(unify_rule_head(args, head), body))
    return or_(*clauses).cps_convert(compiler, cont)
  
  def cps_convert(self, compiler, cont):
    k = compiler.new_var(il.LocalVar('cont'))
    params = compiler.new_var(il.LocalVar('params'))
    v = compiler.new_var(il.LocalVar('v'))
    arity_body_map = compiler.new_var(il.LocalVar('arity_body_map'))
    rules_function = compiler.new_var(il.LocalVar('rules_function'))
    cut_cont = compiler.new_var(il.LocalVar('cut_cont'))
    fc = compiler.new_var(il.LocalVar('old_failcont'))
    arity_body_pairs = []
    assigns = []
    rules_cont = il.clamda(v, 
                           il.SetCutCont(cut_cont), 
                           #il.SetFailCont(fc), 
                           k(v))
    for arity, rules in self.rules.items():
      clauses = []
      for head, body in rules:
        head_exps = begin(*tuple(
          unify_head_item2(il.GetItem(params, il.Integer(i)), head_item) 
                              for (i, head_item) in enumerate(head)))
        clauses.append(begin(head_exps, body))
      arity_fun = il.lamda((), 
            il.Assign(cut_cont, il.cut_cont),
            #il.Assign(fc, il.failcont),
            #il.SetFailCont(il.clamda(v, il.SetCutCont(cut_cont), fc(v))),
            il.SetCutCont(il.failcont), 
            or_(*clauses).cps_convert(compiler, rules_cont))
      arity_fun_name = compiler.new_var(il.LocalVar('arity_fun_%s'%arity))
      assigns.append(il.Assign(arity_fun_name, arity_fun))
      arity_body_pairs.append((arity, arity_fun_name))  
    
    rules_body = il.begin(
      il.begin(*assigns),
      il.Assign(arity_body_map, il.RulesDict({arity:body for arity, body in arity_body_pairs})),
      il.If(il.In(il.Len(params), arity_body_map),
            il.Apply(il.GetItem(arity_body_map, il.Len(params)), ()),
            il.failcont(NONE)))
    return cont(il.RulesFunction(rules_function, (k, params), rules_body))
    
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
        
def unify_rule_head(args, head):
  return il.begin(*tuple(unify_head_item1(arg, head_item) 
                      for (arg, head_item) in zip(args, head)))

def unify_head_item1(arg, head_item):
  # for direct call
  if isinstance(head_item, Var) and not isinstance(head_item, LogicVar):
    return Assign(head_item, arg)
  else: 
    return unify(arg, head_item)

@special
def unify_head_item2(compiler, cont, arg, head_item):
  # for call with rules variable.
  if not isinstance(head_item, Var):
    head_item = head_item.interlang()
    arg1 = compiler.new_var(il.LocalVar('arg'))
    return il.Begin((
      il.Assign(arg1, il.Deref(arg)),
      il.If(il.IsLogicVar(arg1),
         il.begin(il.SetBinding(arg1, head_item),
               il.append_failcont(compiler, il.DelBinding(arg1)),
               cont(il.TRUE)),
              il.If(il.Eq(arg1, head_item), cont(TRUE), il.failcont(TRUE)))))
  elif not isinstance(head_item, LogicVar):
    return il.Begin((
      il.Assign(head_item.interlang(), arg),
      cont(il.TRUE)))
  else:
    head_item = head_item.interlang()
    arg1 = compiler.new_var(il.LocalVar('arg'))
    head1 = compiler.new_var(il.LocalVar('head'))
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
  
from dao.command import expression_with_code

def macro(*rules):
  result = {}
  for rule in rules:
    head = tuple(element(x) for x in rule[0])
    body = begin(*(element(x) for x in rule[1:]))
    result.setdefault(len(head), []).append((head, body))
  return MacroRules(result)

from special import eval_

class MacroRules(Element):
  def __init__(self, rules):
    self.rules = rules

  def __call__(self, *args):
    return Apply(self, tuple(il.element(arg) for arg in args))
  
  def alpha_convert(self, env, compiler):
    rules1 = {}
    for arity, rules in self.rules.items():
      result = []
      for head, body in rules:
        head, new_env = alpha_rule_head(head, env, compiler)
        for var, new_var in new_env.bindings.items():
          new_env.bindings[var] = eval_(new_var)
        body = body.alpha_convert(new_env, compiler)
        result.append((head, body))
      rules1[arity] = result
    return MacroRules(rules1)
      
  def cps_convert_call(self, compiler, cont, args):
    clauses = []
    if len(args) not in self.rules:
      return il.failcont
    for head, body in self.rules[len(args)]:
      clauses.append(begin(unify_macro_head(args, head), body))
    return or_(*clauses).cps_convert(compiler, cont)
  
  def cps_convert(self, compiler, cont):
    k = compiler.new_var(il.LocalVar('cont'))
    params = compiler.new_var(il.LocalVar('params'))
    v = compiler.new_var(il.LocalVar('v'))
    arity_body_map = compiler.new_var(il.LocalVar('arity_body_map'))
    #rules_function = compiler.new_var(il.LocalVar('rules_function'))
    cut_cont = compiler.new_var(il.LocalVar('cut_cont'))
    arity_body_pairs = []
    assigns = []
    rules_cont = il.clamda(v, 
                           il.SetCutCont(cut_cont), 
                           k(v))
    for arity, rules in self.rules.items():
      clauses = []
      for head, body in rules:
        head_exps = begin(*tuple(
          unify_macro_head_item2(il.GetItem(params, il.Integer(i)), head_item) 
                              for (i, head_item) in enumerate(head)))
        clauses.append(begin(head_exps, body))
      arity_fun = il.lamda((), 
            il.Assign(cut_cont, il.cut_cont),
            #il.Assign(fc, il.failcont),
            #il.SetFailCont(il.clamda(v, il.SetCutCont(cut_cont), fc(v))),
            il.SetCutCont(il.failcont), 
            or_(*clauses).cps_convert(compiler, rules_cont))
      arity_fun_name = compiler.new_var(il.LocalVar('arity_fun_%s'%arity))
      assigns.append(il.Assign(arity_fun_name, arity_fun))
      arity_body_pairs.append((arity, arity_fun_name))  
    
    rules_body = il.begin(
      il.begin(*assigns),
      il.Assign(arity_body_map, il.RulesDict({arity:body for arity, body in arity_body_pairs})),
      il.If(il.In(il.Len(params), arity_body_map),
            il.Apply(il.GetItem(arity_body_map, il.Len(params)), ()),
            il.failcont(NONE)))
    return cont(il.MacroLamda((k, params), rules_body))
    
def unify_macro_head(args, head):
  return begin(*tuple(unify_macro_head_item1(arg, head_item) 
                      for arg, head_item, in zip(args, head)))

def unify_macro_head_item1(arg, head_item):
  # for direct call
  if isinstance(head_item, Var) and not isinstance(head_item, LogicVar):
    return Assign(head_item, expression_with_code(arg))
  else: 
    return unify(arg, head_item)

@special
def unify_macro_head_item2(compiler, cont, arg, head_item):
  if isinstance(head_item, Var):
    if not isinstance(head_item, LogicVar):
      return il.Begin((
        il.Assign(head_item.interlang(), arg),
        cont(il.TRUE)))
    else: 
      v = compiler.new_var(il.LocalVar('v'))
      head_item = head_item.interlang()
      arg1 = compiler.new_var(il.LocalVar('arg'))
      head1 = compiler.new_var(il.LocalVar('head'))
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
    arg1 = compiler.new_var(il.LocalVar('arg'))
    return il.begin(
      il.Assign(arg1, il.Deref(arg)),
      il.If(il.IsLogicVar(arg1),
         il.begin(il.SetBinding(arg1, head_item),
               il.append_failcont(compiler, il.DelBinding(arg1)),
               cont(il.TRUE)),
              il.If(il.Eq(arg1, head_item), cont(TRUE), il.failcont(TRUE))))