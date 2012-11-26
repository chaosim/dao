''' many lisp style special forms'''

from dao.base import Element, classeq
from dao.command import special, Command, CommandCall, SpecialCall, Apply, Var, LogicVar
from dao.compilebase import CompileTypeError, VariableNotBound
from dao.interlang import TRUE, FALSE, NONE, element
import dao.interlang as il
from special import begin

v0, fc0 = il.LocalVar('v'), il.LocalVar('fc')

def assign(var, exp):
  return Assign(il.element(var), il.element(exp))

class Assign(CommandCall):
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
    assign_bindings = tuple(assign(new_env[var], value.alpha_convert(env, compiler)) 
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
  
@special
def unify_head_item(compiler, cont, head_item, param):
  try: 
    head_item.cps_convert_unify
  except:
    head_item = head_item.interlang()
    x1 = compiler.new_var(il.LocalVar('x'))
    return il.begin(
      il.Assign(x1, il.Deref(param)),
      il.If(il.IsLogicVar(x1),
         il.begin(il.SetBinding(x1, head_item),
               il.append_failcont(compiler, il.DelBinding(x1)),
               cont(il.TRUE)),
              il.If(il.Eq(x1, head_item), cont(TRUE), il.failcont(TRUE))))
  
  head_item = head_item.interlang()
  x1 = compiler.new_var(il.LocalVar('x'))
  y1 = compiler.new_var(il.LocalVar('y'))
  return il.begin(
    il.Assign(x1, il.Deref(param)), #for LogicVar, could be optimized when generate code.
    il.Assign(y1, il.Deref(head_item)),
    il.If(il.IsLogicVar(x1),
       il.begin(il.SetBinding(x1, y1),
             il.append_failcont(compiler, il.DelBinding(x1)),
             cont(il.TRUE)),
       il.begin(
         il.If(il.IsLogicVar(y1),
            il.begin(il.SetBinding(y1, x1),
                  il.append_failcont(compiler, il.DelBinding(y1)),
                  cont(il.TRUE)),
            il.If(il.Eq(x1, y1), cont(il.TRUE), il.failcont(il.FALSE))))))
  
def unify_head_params(head, params):
  return begin(*tuple(unify_head_item(x, il.GetItem(params, il.Integer(i))) 
                      for (i, x) in enumerate(head)))

def unify_list(list1, list2):
  return begin(*tuple(unify(x, y) for x, y, in zip(list1, list2)))

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
    clauses = []
    if len(args) not in self.rules:
      return il.failcont
    arg_vars = tuple(Var('arg%s'%i) for i in range(len(args)))
    for head, body in self.rules[len(args)]:
      head_vars = get_tuple_vars(head)
      bindings = {var: LogicVar(var.name) for var in head_vars}
      head = tuple(x.subst(bindings) for x in head)
      clauses.append(begin(unify_list(arg_vars, head), body.subst(bindings)))
    return let(zip(arg_vars, args), or_(*clauses))
  
  def alpha_convert(self, env, compiler):
    rules1 = {}
    for arity, rules in self.rules.items():
      result = []
      for head, body in rules:
        head, new_env = alpha_rule_head(head, env, compiler)
        body = body.alpha_convert(new_env, compiler)
        result.append((new_env.bindings.values(), head, body))
      rules1[arity] = result
    return Rules(rules1)
      
  def cps_convert(self, compiler, cont):
    k = compiler.new_var(il.LocalVar('cont'))
    params = compiler.new_var(il.LocalVar('params'))
    v = compiler.new_var(il.LocalVar('v'))
    arity_body_map = compiler.new_var(il.LocalVar('arity_body_map'))
    rules_function = compiler.new_var(il.LocalVar('rules_function'))
    cut_cont = compiler.new_var(il.LocalVar('cut_cont'))
    arity_body_pairs = []
    assigns = []
    rules_cont = il.clamda(v, il.SetCutCont(cut_cont), k(v))
    for arity, rules in self.rules.items():
      clauses = []
      for head_vars, head, body in rules:
        assign_head_vars = il.begin(*(tuple(il.Assign(var.interlang(), il.new_logicvar(il.String(var.name)))
                      for var in head_vars)))
        clauses.append(begin(DirectInterlang(assign_head_vars), unify_head_params(head, params), body))
      arity_fun = il.lamda((), 
            il.Assign(cut_cont, il.cut_cont),
            il.SetCutCont(il.failcont), 
            or_(*clauses).cps_convert(compiler, rules_cont))
      arity_fun_name = compiler.new_var(il.LocalVar('arity_fun_%s'%arity))
      assigns.append(il.Assign(arity_fun_name, arity_fun))
      arity_body_pairs.append((arity, arity_fun_name))  
    
    rules_body = il.begin(
      il.begin(*assigns),
      il.Assign(arity_body_map, il.RulesDict({arity:body for arity, body in arity_body_pairs})),
      il.If(il.in_(il.Len(params), arity_body_map),
            il.Apply(il.GetItem(arity_body_map, il.Len(params)), ()),
            il.failcont(NONE)))
    return cont(il.RulesFunction(rules_function, (k, params), rules_body))
    
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
      clauses = []
      if len(args) not in self.rules:
        return il.failcont
      for head, body in self.rules[len(args)]:
        #env: if head item is a variable x, then env[head] = arg
        #bindings: if args item is a variable x, then bindings[arg] = head
        bindings, env = unify_rule_head(args, head)
        clauses.append(let(bindings, body.subst(env)))
      return or_(*clauses)  
    
  def alpha_convert(self, env, compiler):
    rules1 = {}
    for arity, rules in self.rules.items():
      result = []
      for head, body in rules:
        head, new_env = alpha_rule_head(head, env, compiler)
        body = body.alpha_convert(new_env, compiler)
        result.append((new_env.bindings.values(), head, body))
      rules1[arity] = result
    return MacroRules(rules1)
      
  def cps_convert(self, compiler, cont):
    k = compiler.new_var(il.LocalVar('cont'))
    params = compiler.new_var(il.LocalVar('params'))
    v = compiler.new_var(il.LocalVar('v'))
    arity_body_map = compiler.new_var(il.LocalVar('arity_body_map'))
    #rules_function = compiler.new_var(il.LocalVar('rules_function'))
    cut_cont = compiler.new_var(il.LocalVar('cut_cont'))
    arity_body_pairs = []
    assigns = []
    rules_cont = il.clamda(v, il.SetCutCont(cut_cont), k(v))
    for arity, rules in self.rules.items():
      clauses = []
      for head_vars, head, body in rules:
        assign_head_vars = il.begin(*(tuple(il.Assign(var.interlang(), il.new_logicvar(il.String(var.name)))
                      for var in head_vars)))
        clauses.append(begin(DirectInterlang(assign_head_vars), unify_head_params(head, params), eval_(body)))
      arity_fun = il.lamda((), 
            il.Assign(cut_cont, il.cut_cont),
            il.SetCutCont(il.failcont), 
            or_(*clauses).cps_convert(compiler, rules_cont))
      arity_fun_name = compiler.new_var(il.LocalVar('arity_fun_%s'%arity))
      assigns.append(il.Assign(arity_fun_name, arity_fun))
      arity_body_pairs.append((arity, arity_fun_name))  
    
    rules_body = il.begin(
      il.begin(*assigns),
      il.Assign(arity_body_map, il.RulesDict({arity:body for arity, body in arity_body_pairs})),
      il.If(il.in_(il.Len(params), arity_body_map),
            il.Apply(il.GetItem(arity_body_map, il.Len(params)), ()),
            il.failcont(NONE)))
    return cont(il.MacroLamda((k, params), rules_body))
    
