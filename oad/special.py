# -*- coding: utf-8 -*-

from oad.term import Apply, Function, Macro, closure
from oad.rule import Rule, RuleList
from oad.eval import solve_exps
from oad.env import BlockEnvironment

# special forms: quote, begin, if, eval, let, lambda, function, macro, module

class SpecialForm:#(UEntity):
  # 具体的特殊式各自定义自己的__init__ 与solve
  def __call__(self, *exps):
    return Apply(self, *exps)
  def apply(self, evaluator, *exps):
    for op in self.solve(evaluator):
      for x in op.apply(evaluator, *exps):
        yield x

class quote(SpecialForm):
  def __init__(self, exp):
    self.exp = exp
  def solve(self, evaluator):
    yield self.exp
  def __repr__(self):
    return "'%s"%self.exp

class set(SpecialForm):
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
  def solve(self, evaluator):
    for value in evaluator.solve(self.exp):
      env = evaluator.env
      while env is not None:
        if env.hasBindings() and self.var not in env.bindings:
          env = env.outer
        else: break
      else: raise Exception('%s is not defined'%self.var)
      env[self.var] = value
      yield value
  def __repr__(self):
    return "set(%s %s)"%(self.var, self.exp)

class begin(SpecialForm):
  def __init__(self, *exps):
    self.exps = exps
  def solve(self, evaluator):
    return solve_exps(evaluator, self.exps)
  def __repr__(self):
    return 'begin(%s)'%(';'.join([repr(x) for x in self.exps]))
  
class if_(SpecialForm):
  def __init__(self, test, exp1, exp2):
    self.test, self.exp1, self.exp2 = test, exp1, exp2
  def solve(self, evaluator):
    for x in evaluator.solve(self.test):
      if x: return evaluator.solve(self.exp1)
      else: return evaluator.solve(self.exp2)
  def __repr__(self):
    return 'if %s: %s else:%s'%(self.test, self.exp1, self.exp2)

# UserFunction and UserMacro both are Rules, 
# which distinct by the strict and lazy evaluation of the arguments.
# the implentations is based on "Lisp In Small Pieces" by Christian Queinnec and Ecole Polytechnique

def let(bindings, *body):
  return FunctionForm((bindings.keys(),)+ body)(*bindings.values())
  
def lambda_(vars, *body): 
  #[lambda [var ...] ...]
  return FunctionForm((vars,)+body)

class FunctionForm(SpecialForm):
  def __init__(self, *rules):
    self.arity2rules = {}
    for rule in rules:
      self.arity2rules.setdefault(len(rule[0]), RuleList()).append(Rule(rule[0], rule[1:]))
  def solve(self, evaluator):
    yield UserFunction(self.arity2rules, evaluator.env, recursive=False)
  def __repr__(self):
    result = 'func('
    for rules in self.arity2rules.values():
      result += '%s'%rules
    result += ')'
    return result

function = FunctionForm

def letrec(bindings, *body):
   #[letrec {var: value} ...] ...]
  return RecursiveFunctionForm((bindings.keys(),)+ body)(*bindings.values())

class RecursiveFunctionForm(function): 
  def solve(self, evaluator):
    newEnv = evaluator.env.extend()
    evaluator.env = newEnv
    yield UserFunction(self.arity2rules, newEnv, recursive=True)
  def __repr__(self):
    result = 'recfunc('
    for rules in self.arity2rules.values():
      result += '%s'%rules
    result += ')'
    return result
  
class MacroForm(SpecialForm):
  def __init__(self, *rules):
    self.arity2rules = {}
    for rule in rules:
      self.arity2rules.setdefault(len(rule[0]), RuleList()).append(Rule(rule[0], rule[1:]))
  def solve(self, evaluator):
    yield UserMacro(self.arity2rules, evaluator.env, recursive=False)
  def __repr__(self):
    result = 'macroform('
    for rules in self.arity2rules.values():
      result += '%s'%rules
    result += ')'
    return result

macro = MacroForm

class Rules:
  def __init__(self, rules, env, recursive):
    self.rules = rules
    self.env = env
    self.recursive = recursive
    
class UserFunction(Rules,  Function): 
  def apply(self, evaluator, *exps):
    if len(exps) not in self.rules: 
      throw_existence_error("procedure", self.get_prolog_signature())
    values = [evaluator.eval(e) for e in exps]
    return self.rules[len(exps)].apply(evaluator, self.env, self.recursive, *values)
  def __repr__(self):return 'fun(%s)'%repr(self.rules)
  
class UserMacro(Rules,  Macro): 
  def apply(self, evaluator, *exps):
    if len(exps) not in self.rules: 
      throw_existence_error("procedure", self.get_prolog_signature())
    exps = [closure(exp, evaluator.env) for exp in exps]
    return self.rules[len(exps)].apply(evaluator, self.env, self.recursive, *exps)
  def __repr__(self): return 'macro(%s)'%repr(self.rules)
  
class eval_(SpecialForm):
  def __init__(self, exp):
    self.exp = exp
  def solve(self, evaluator):
    for value in evaluator.solve(self.exp):
      for x in evaluator.solve(value): 
        yield x
  def __repr__(self):
    return 'eval(%s)'%self.exp

####@funcont
##def module_done_cont(self, value, evaluator):  
##  evaluator.scont = self.cont
##  evaluator.value = evaluator.env
##  evaluator.env = evaluator.env.outer
##  
####from oad.env import ModuleEnvironment  
##def on_module(evaluator, scont, tail): 
##  #[module name ...]
##  evaluator.env = ModuleEnvironment({}, evaluator.env)
##  evaluator.scont = module_done_cont(evaluator)
##  Cons('begin', tail).scont(evaluator)
##

class block(SpecialForm):
  def __init__(self, label, *body):
    self.label, self.body = label, body
  def solve(self, evaluator):
    try:
      evaluator.env = BlockEnvironment(self.label.name, evaluator.env, 'where is scont?')
      env = evaluator.env
      for x in solve_exps(evaluator, self.body):
        yield x
    except ReturnFromBlock, e:
      if e.label==env.label:
        for x in evaluator.solve(e.form):
          yield x
          
  def __repr__(self):
    return 'block(%s)'%self.body

class ReturnFromBlock:
  def __init__(self, label, form):
    self.label, self.form= label.name, form
    
class return_from(SpecialForm):
  def __init__(self, label, form):
    self.label, self.form = label, form
  def solve(self, evaluator):
    if 0: yield
    raise ReturnFromBlock(self.label, self.form)
  def __repr__(self):
    return 'return_from(%s)'%self.label

def on_unwind_protect(evaluator, scont, tail):
  #[unwind-protect form cleanup]
  form, cleanup = car(tail), cdr(tail)
  evaluator.scont = UnwindProtectContinuation(cleanup, evaluator)
  return form.scont(evaluator)

class UnwindProtectContinuation:#(Continuation):
  def __init__(self, cleanup, evaluator):
    Continuation.__init__(self, evaluator)
    self.cleanup, self.env = cleanup, evaluator.env
  def activate(self, evaluator):
    evaluator.env = self.env
    evaluator.set(protect_return_cont(evaluator.value, self.cont))
    return begin(self.cleanup, evaluator)
  def unwind(self, targetCont, evaluator):
    evaluator.env = self.env
    evaluator.set(unwind_cont(evaluator.value, targetCont, self.env, self.cont))
    return begin(self.cleanup, evaluator)

##@funcont
def protect_return_cont(self, value, evaluator):
  value = self.args[0]
  self.cont.on(value) 
##@funcont
def unwind_cont(self, _, evaluator): 
  evaluator.value, targetCont = self.arguments[:2]
  return self.cont.unwind(targetCont, evaluator)

class catch(SpecialForm):
  def __init__(self, tag, *body):
    self.tag, self.body = tag, body
  def solve(self, evaluator):
    try:
      evaluator.env = BlockEnvironment(self.label.name, evaluator.env, 'where is scont?')
      env = evaluator.env
      for x in solve_exps(evaluator, self.body):
        yield x
    except ReturnFromBlock, e:
      if e.label==env.label:
        for x in evaluator.solve(e.form):
          yield x
  tag, body = car(tail), cdr(tail)
  evaluator.scont = catch_cont(body, evaluator.env, scont)
  return tag.scont(evaluator)   

##@funcont
def catch_cont(self, tag, evaluator):
  body, env = self.arguments[:2]
  evaluator.env, evaluator.scont = env, LabelContinuation(tag, evaluator, self.cont)
  return begin(body, evaluator)

class LabelContinuation:#(Continuation):
  def __init__(self, tag, evaluator, cont):
    Continuation.__init__(self, cont)
    self.tag, self.block = tag, evaluator.env
  def activate(self, evaluator):
    return self.cont.on(evaluator.value, evaluator)
  def lookup(self, tag, cont, evaluator): 
    if tag==self.tag:
      form, evaluator.env = cont.arguments[:2]
      form.scont(evaluator.set(throwing_cont(self)))
    else: self.cont.lookup(tag, cont, evaluator)
  
##@funcont
def throwing_cont(self, value, evaluator): 
  return self.cont.unwind(self.cont, evaluator)

def on_throw(evaluator, scont, tail): 
  #[throw tag form]
  tag, form = car(tail), cadr(tail)
  evaluator.scont = throw_cont(form, evaluator.env, scont)
  return tag.scont(evaluator)  

##@funcont
def throw_cont(self, tag, evaluator):
  return self.lookup(tag, self, evaluator)

