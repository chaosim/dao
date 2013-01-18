''' many lisp style special forms'''

from dao.base import Element, classeq
from dao.compilebase import CompileTypeError, VariableNotBound
from dao.command import special, Command, CommandCall, SpecialCall, Apply
from dao.command import Var, LogicVar, Const, Assign
from dao.command import element
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
  
  def alpha_convert(self, env, compiler):
    new_env = env.extend()
    for p in self.params: 
      new_env.bindings[p] = compiler.new_var(p)
    self.params = tuple(new_env[p] for p in self.params)
    self.body = self.body.alpha_convert(new_env, compiler)
    self.variables = new_env.bindings.values()
    return self
    
  def cps_convert(self, compiler, cont):
    k = compiler.new_var(il.ConstLocalVar('cont'))
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

def macro(params, *body):
  body = tuple(element(x) for x in body)
  return Macro(params, begin(*body))

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
  
  def alpha_convert(self, env, compiler):
    new_env = env.extend()
    for p in self.params: 
      new_env.bindings[p] = compiler.new_var(p)
    self.params = tuple(new_env[p] for p in self.params)
    for var, new_var in new_env.bindings.items():
      new_env.bindings[var] = eval_(new_var)
    self.body = self.body.alpha_convert(new_env, compiler)
    return self
    
  def has_cut(self, arity):
    return has_cut(self.body)

  def cps_convert(self, compiler, cont):
    k = compiler.new_var(il.ConstLocalVar('cont'))
    params = tuple(x.interlang() for x in self.params)
    return cont(il.MacroLamda((k,)+params, self.body.cps_convert(compiler, k)))
  
  def cps_convert_call(self, compiler, cont, args):
    # see The 90 minute Scheme to C compiler by Marc Feeley
    fun = self.body.cps_convert(compiler, cont)
    params = tuple(x.interlang() for x in self.params)
    for var, arg in reversed(zip(params, args)):
      fun = expression_with_code(arg).cps_convert(compiler, il.Clamda(var, fun))
    return fun
  
  def interlang(self):
    return il.MacroLamda(tuple(x.interlang() for x in self.params), self.body.interlang())

class MacroVar(Var): 
  def cps_convert(self, compiler, cont):
    return cont(self.interlang())
  
  def cps_convert_call(self, compiler, cont, args):
    function = compiler.new_var(il.ConstLocalVar('function'))
    v = compiler.new_var(il.ConstLocalVar('v'))
    macro_args = il.macro_args([il.ExpressionWithCode(arg, 
                                      il.Lamda((), arg.cps_convert(compiler, il.clamda(v, v)))) 
                                for arg in args])
    return self.cps_convert(compiler, 
                            il.clamda(function, il.Apply(function, (cont, macro_args))))
  
  def interlang(self):
    return il.Var(self.name)
  
  def __repr__(self):
    return 'MacroVar(%s)'%self.name
  
class ConstMacroVar(MacroVar, Const): 
  def interlang(self):
    return il.ConstLocalVar(self.name)

class LamdaVar(Var):
  def cps_convert(self, compiler, cont):
    return cont(self.interlang())
  
  def cps_convert_call(self, compiler, cont, args):
    function = compiler.new_var(il.Var('function'))
    vars = tuple(compiler.new_var(il.ConstLocalVar('a'+repr(i))) 
                 for i in range(len(args)))
    body = il.Apply(function, (cont,)+vars)
    for var, item in reversed(zip(vars, args)):
      body = item.cps_convert(compiler, il.clamda(var, body)) 
    v = compiler.new_var(il.ConstLocalVar('v'))
    return self.cps_convert(compiler, il.clamda(function,body))
  
  def interlang(self):
    return il.Var(self.name)
  
  def __repr__(self):
    return 'LamdaVar(%s)'%self.name
  
class ConstLamdaVar(LamdaVar, Const): 
  def interlang(self):
    return il.ConstLocalVar(self.name)  

class RulesVar(Var):
  def cps_convert(self, compiler, cont):
    return cont(self.interlang())
  
  def cps_convert_call(self, compiler, cont, args):
    function = compiler.new_var(il.Var('function'))
    vars = tuple(compiler.new_var(il.ConstLocalVar('a'+repr(i))) 
                 for i in range(len(args)))
    body = il.Apply(function, (cont, il.Tuple(*vars)))
    for var, item in reversed(zip(vars, args)):
      body = item.cps_convert(compiler, il.clamda(var, body)) 
    v = compiler.new_var(il.ConstLocalVar('v'))
    return self.cps_convert(compiler, il.clamda(function,body))
  
  def interlang(self):
    return il.Var(self.name)
  
  def __repr__(self):
    return 'RulesVar(%s)'%self.name

class ConstRulesVar(RulesVar, Const): 
  def interlang(self):
    return il.ConstLocalVar(self.name)  

def let(bindings, *body):
  bindings = tuple((var, element(value)) for var, value in bindings)
  return Let(bindings, begin(*body))

class Let(il.Element):
  def __init__(self, bindings, body):
    self.bindings = bindings
    self.body = body
  
  def alpha_convert(self, env, compiler):
    new_env = env.extend()
    for var, value in self.bindings:
      if isinstance(value, Rules):
        if not isinstance(var, Const):
          new_var = compiler.new_var(RulesVar(var.name))
        else:
          new_var = compiler.new_var(ConstRulesVar(var.name))
      elif isinstance(value, Lamda):
        if not isinstance(var, Const):
          new_var = compiler.new_var(LamdaVar(var.name))
        else:
          new_var = compiler.new_var(ConstLamdaVar(var.name))
      elif isinstance(value, MacroRules):
        if not isinstance(value, Const):
          new_var = compiler.new_var(MacroVar(var.name))
        else:
          new_var = compiler.new_var(ConstMacroVar(var.name))
      else:
        new_var = compiler.new_var(var)
      if isinstance(var, Const):
        new_var.assigned = True
      env[var ]  = new_var
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
    
class RecursiveFunctionVar(ConstLamdaVar):
  def cps_convert(self, compiler, cont):
    return cont(self.interlang())
  
  def cps_convert_call(self, compiler, cont, args):
    function = compiler.new_var(il.RecursiveVar('function'))
    vars = tuple(compiler.new_var(il.ConstLocalVar('a'+repr(i))) 
                 for i in range(len(args)))
    body = il.Apply(function, (cont,)+vars)
    for var, item in reversed(zip(vars, args)):
      body = item.cps_convert(compiler, il.clamda(var, body)) 
    v = compiler.new_var(il.ConstLocalVar('v'))
    return self.cps_convert(compiler, il.clamda(function,body))
  
  def interlang(self):
    return il.RecursiveVar(self.name)
  
  def __repr__(self):
    return 'RecursiveVar(%s)'%self.name

class RecursiveRulesVar(ConstRulesVar):
  def cps_convert(self, compiler, cont):
    return cont(self.interlang())
  
  def cps_convert_call(self, compiler, cont, args):
    function = compiler.new_var(il.RecursiveVar('function'))
    vars = tuple(compiler.new_var(il.ConstLocalVar('a'+repr(i))) 
                 for i in range(len(args)))
    body = il.Apply(function, (cont, il.Tuple(*vars)))
    for var, item in reversed(zip(vars, args)):
      body = item.cps_convert(compiler, il.clamda(var, body)) 
    v = compiler.new_var(il.ConstLocalVar('v'))
    return self.cps_convert(compiler, il.clamda(function,body))
  
  def interlang(self):
    return il.RecursiveVar(self.name)
  
  def __repr__(self):
    return 'RecursiveRulesVar(%s)'%self.name

class RecursiveMacroVar(ConstMacroVar): 
  def cps_convert(self, compiler, cont):
    return cont(self.interlang())
  
  def cps_convert_call(self, compiler, cont, args):
    function = compiler.new_var(il.RecursiveVar('function'))
    v = compiler.new_var(il.ConstLocalVar('v'))
    macro_args = il.macro_args([il.ExpressionWithCode(arg, 
                                      il.Lamda((), arg.cps_convert(compiler, il.clamda(v, v)))) 
                                for arg in args])
    return self.cps_convert(compiler, 
                            il.clamda(function,
                                il.Apply(function, (cont, macro_args))))
  
  def interlang(self):
    return il.RecursiveVar(self.name)
  
  def __repr__(self):
    return 'RecursiveVar(%s)'%self.name
  
def letrec(bindings, *body):
  return Letrec(tuple((element(var), element(value)) for var, value in bindings), 
                begin(*tuple(element(exp) for exp in body)))

class Letrec(Element):
  def __init__(self, bindings, body):
    self.bindings = bindings
    self.body = body
  
  def alpha_convert(self, env, compiler):
    new_env = env.extend()
    for var, value in self.bindings:
      if isinstance(value, Rules):
        new_var = compiler.new_var(RecursiveRulesVar(var.name))
      elif isinstance(value, Lamda):
        new_var = compiler.new_var(RecursiveFunctionVar(var.name))
      elif isinstance(value, MacroRules):
        new_var = compiler.new_var(RecursiveMacroVar(var.name))
      else:
        new_var = compiler.new_var(var)
      if isinstance(new_var, Const):
        new_var.assigned = True
      new_env.bindings[var] = new_var
    return begin(*(tuple(Assign(new_env[var], value.alpha_convert(new_env, compiler)) 
                         for var, value in self.bindings)
                   +(self.body.alpha_convert(new_env, compiler),)))
  
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
  result = {}
  for rule in rules:
    head = tuple(element(x) for x in rule[0])
    body = begin(*(element(x) for x in rule[1:]))
    result.setdefault(len(head), []).append((head, body))
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
    exp.cps_convert(compiler, il.clamda(v, 
      il.Assign(il.cut_cont, cut_cont),                         
      cont(v))))

from control import has_cut

class Rules(Lamda):
  def __init__(self, rules):
    self.rules = rules

  def __call__(self, *args):
    return Apply(self, tuple(element(arg) for arg in args))
  
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
  
  def has_cut(self, arity):
    for head, body in self.rules[arity]:
      if has_cut(body):
        return True
    return False
    
  def cps_convert_call(self, compiler, cont, args):
    clauses = []
    arity = len(args)
    if arity not in self.rules:
      return il.failcont
    for head, body in self.rules[arity]:
      clauses.append(begin(unify_rule_head(args, head), body))
    if self.has_cut(arity):
      cut_cont = compiler.new_var(Const('cut_cont'))
      return wrap_cut(or_(*clauses)).cps_convert(compiler, cont)
    else:
      return or_(*clauses).cps_convert(compiler, cont)
  
  def cps_convert(self, compiler, cont):
    k = compiler.new_var(il.ConstLocalVar('cont'))
    params = compiler.new_var(il.ConstLocalVar('params'))
    v = compiler.new_var(il.ConstLocalVar('v'))
    arity_body_map = compiler.new_var(il.ConstLocalVar('arity_body_map'))
    cut_cont = compiler.new_var(il.ConstLocalVar('cut_cont'))
    fc = compiler.new_var(il.ConstLocalVar('old_failcont'))
    arity_body_pairs = []
    assigns = []
    for arity, rules in self.rules.items():
      clauses = []
      for head, body in rules:
        head_exps = begin(*tuple(
          unify_head_item2(il.GetItem(params, il.Integer(i)), head_item) 
                              for (i, head_item) in enumerate(head)))
        clauses.append(begin(head_exps, body))
      if self.has_cut(arity):
        arity_fun = il.lamda((), 
              wrap_cut(or_(*clauses)).cps_convert(compiler, k))
      else:
        arity_fun = il.lamda((), 
              or_(*clauses).cps_convert(compiler, k))
      arity_fun_name = compiler.new_var(il.LocalVar('arity_fun_%s'%arity))
      assigns.append(il.Assign(arity_fun_name, arity_fun))
      arity_body_pairs.append((arity, arity_fun_name))  
    
    rules_body = il.begin(
      il.begin(*assigns),
      il.Assign(arity_body_map, il.RulesDict({arity:body for arity, body in arity_body_pairs})),
      il.If(il.In(il.Len(params), arity_body_map),
            il.Apply(il.GetItem(arity_body_map, il.Len(params)), ()),
            il.failcont(NONE)))
    return cont(il.RulesLamda((k, params), rules_body))
    
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
    arg1 = compiler.new_var(il.ConstLocalVar('arg'))
    return il.begin(
      il.Assign(arg1, il.Deref(arg)),
      il.If(il.IsLogicVar(arg1),
         il.begin(il.SetBinding(arg1, head_item),
               il.append_failcont(compiler, il.DelBinding(arg1)),
               cont(il.TRUE)),
              il.If(il.Eq(arg1, head_item), cont(TRUE), il.failcont(TRUE))))
  elif not isinstance(head_item, LogicVar):
    return il.begin(
      il.Assign(head_item.interlang(), arg),
      cont(il.TRUE))
  else:
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
  
from dao.command import expression_with_code

def macrorules(*rules):
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
    return Apply(self, tuple(element(arg) for arg in args))
  
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

  def has_cut(self, arity):
    for head, body in self.rules[arity]:
      if has_cut(body):
        return True
    return False

  def cps_convert_call(self, compiler, cont, args):
    clauses = []
    arity = len(args)
    if arity not in self.rules:
      return il.failcont(il.TRUE)
    for head, body in self.rules[arity]:
      clauses.append(begin(unify_macro_head(args, head), body))
    if self.has_cut(arity):
      return wrap_cut(or_(*clauses)).cps_convert(compiler, cont)
    else:
      return or_(*clauses).cps_convert(compiler, cont)
  
  def cps_convert(self, compiler, cont):
    k = compiler.new_var(il.ConstLocalVar('cont'))
    params = compiler.new_var(il.ConstLocalVar('params'))
    v = compiler.new_var(il.ConstLocalVar('v'))
    arity_body_map = compiler.new_var(il.ConstLocalVar('arity_body_map'))
    cut_cont = compiler.new_var(il.ConstLocalVar('cut_cont'))
    arity_body_pairs = []
    assigns = []
    for arity, rules in self.rules.items():
      clauses = []
      for head, body in rules:
        head_exps = begin(*tuple(
          unify_macro_head_item2(il.GetItem(params, il.Integer(i)), head_item) 
                              for (i, head_item) in enumerate(head)))
        clauses.append(begin(head_exps, body))
      if self.has_cut(arity):
        arity_fun = il.lamda((), 
              wrap_cut(or_(*clauses)).cps_convert(compiler, k))
      else:  
        arity_fun = il.lamda((), 
              or_(*clauses).cps_convert(compiler, k))
      arity_fun_name = compiler.new_var(il.LocalVar('arity_fun_%s'%arity))
      assigns.append(il.Assign(arity_fun_name, arity_fun))
      arity_body_pairs.append((arity, arity_fun_name))  
    
    rules_body = il.begin(
      il.begin(*assigns),
      il.Assign(arity_body_map, il.RulesDict({arity:body for arity, body in arity_body_pairs})),
      il.If(il.In(il.Len(params), arity_body_map),
            il.Apply(il.GetItem(arity_body_map, il.Len(params)), ()),
            il.failcont(NONE)))
    return cont(il.MacroRules((k, params), rules_body))
    
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