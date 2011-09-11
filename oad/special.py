# -*- coding: utf-8 -*-

from oad.term import Apply, Function, Macro, closure
from oad.rule import Rule, RuleList
from oad.solve import value_cont
from oad.env import BlockEnvironment

# special forms: quote, begin, if, eval, let, lambda, function, macro, module

class SpecialForm:#(UEntity):
  # 具体的特殊式各自定义自己的__init__ 与solve
  def __call__(self, *exps):
    return Apply(self, *exps)
  def apply(self, solver, *exps):
    for op in self.solve(solver):
      for x in op.apply(solver, *exps):
        yield x

class quote(SpecialForm):
  def __init__(self, exp):
    self.exp = exp
  def cont(self, cont, solver): return value_cont(self.exp, cont)
  def __repr__(self):
    return "'%s"%self.exp

class set(SpecialForm):
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
  def cont(self, cont, solver):
    def set_cont(value, solver):
      env = solver.env
      while env is not None:
        if env.hasBindings() and self.var not in env.bindings:
          env = env.outer
        else: break
      else: env = solver.env
      env[self.var] = value
      yield cont, True
    def my_cont(value, solver):
      for value in solver.solve(self.exp, set_cont):
        yield set_cont, value
    return my_cont
  def __repr__(self):
    return "set(%s %s)"%(self.var, self.exp)

class begin(SpecialForm):
  def __init__(self, *exps):
    self.exps = exps
  def cont(self, cont, solver): 
    return solver.exps_cont(self.exps, cont)
  def __repr__(self):
    return 'begin(%s)'%(';'.join([repr(x) for x in self.exps]))
  
class if_(SpecialForm):
  def __init__(self, test, exp1, exp2):
    self.test, self.exp1, self.exp2 = test, exp1, exp2
  def cont(self, cont, solver):
    def mycont(value, solver):
      if value: yield solver.cont(self.exp1, cont), value
      else: yield solver.cont(self.exp2, cont), value
    return solver.cont(self.test, mycont)
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
  def cont(self, cont, solver):
    func = UserFunction(self.arity2rules, solver.env, recursive=False)
    return value_cont(func, cont)
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
  def cont(self, cont, solver):
    newEnv = solver.env.extend()
    solver.env = newEnv
    func = UserFunction(self.arity2rules, newEnv, recursive=True)
    return value_cont(func, cont)
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
  def cont(self, cont, solver):
    macro = UserMacro(self.arity2rules, solver.env, recursive=False)
    return value_cont(macro, cont)
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
  def apply(self, solver, values, cont):
    if len(values) not in self.rules: 
      throw_existence_error("procedure", self.get_prolog_signature())
    return self.rules[len(values)].apply(solver, self.env, cont, self.recursive, values)
  def __repr__(self):return 'fun(%s)'%repr(self.rules)
  
class UserMacro(Rules,  Macro): 
  def apply(self, solver, exps, cont):
    if len(exps) not in self.rules: 
      throw_existence_error("procedure", self.get_prolog_signature())
    exps = [closure(exp, solver.env) for exp in exps]
    return self.rules[len(exps)].apply(solver, self.env, cont, self.recursive, exps)
  def __repr__(self): return 'macro(%s)'%repr(self.rules)
  
class eval_(SpecialForm):
  def __init__(self, exp):
    self.exp = exp
  def cont(self, cont, solver):
    def eval_cont(value, solver): yield solver.cont(value, cont), value
    return solver.cont(self.exp, eval_cont)
  def __repr__(self): return 'eval(%s)'%self.exp

####@funcont
##def module_done_cont(self, value, solver):  
##  solver.scont = self.cont
##  solver.value = solver.env
##  solver.env = solver.env.outer
##  
####from oad.env import ModuleEnvironment  
##def on_module(solver, scont, tail): 
##  #[module name ...]
##  solver.env = ModuleEnvironment({}, solver.env)
##  solver.scont = module_done_cont(solver)
##  Cons('begin', tail).scont(solver)
##

class block(SpecialForm):
  def __init__(self, label, *body):
    self.label, self.body = label, body
  def solve(self, solver):
    try:
      solver.env = BlockEnvironment(self.label.name, solver.env, 'where is scont?')
      env = solver.env
      for x in solve_exps(solver, self.body):
        yield x
    except ReturnFromBlock, e:
      if e.label==env.label:
        for x in solver.solve(e.form):
          yield x
          
  def __repr__(self):
    return 'block(%s)'%self.body

class ReturnFromBlock:
  def __init__(self, label, form):
    self.label, self.form= label.name, form
    
class return_from(SpecialForm):
  def __init__(self, label, form):
    self.label, self.form = label, form
  def solve(self, solver):
    if 0: yield
    raise ReturnFromBlock(self.label, self.form)
  def __repr__(self):
    return 'return_from(%s)'%self.label

##def on_unwind_protect(solver, scont, tail):
##  #[unwind-protect form cleanup]
##  form, cleanup = car(tail), cdr(tail)
##  solver.scont = UnwindProtectContinuation(cleanup, solver)
##  return form.scont(solver)
##
##class UnwindProtectContinuation:#(Continuation):
##  def __init__(self, cleanup, solver):
##    Continuation.__init__(self, solver)
##    self.cleanup, self.env = cleanup, solver.env
##  def activate(self, solver):
##    solver.env = self.env
##    solver.set(protect_return_cont(solver.value, self.cont))
##    return begin(self.cleanup, solver)
##  def unwind(self, targetCont, solver):
##    solver.env = self.env
##    solver.set(unwind_cont(solver.value, targetCont, self.env, self.cont))
##    return begin(self.cleanup, solver)
##
####@funcont
##def protect_return_cont(self, value, solver):
##  value = self.args[0]
##  self.cont.on(value) 
####@funcont
##def unwind_cont(self, _, solver): 
##  solver.value, targetCont = self.arguments[:2]
##  return self.cont.unwind(targetCont, solver)
##
##class catch(SpecialForm):
##  def __init__(self, tag, *body):
##    self.tag, self.body = tag, body
##  def solve(self, solver):
##    try:
##      solver.env = BlockEnvironment(self.label.name, solver.env, 'where is scont?')
##      env = solver.env
##      for x in solve_exps(solver, self.body):
##        yield x
##    except ReturnFromBlock, e:
##      if e.label==env.label:
##        for x in solver.solve(e.form):
##          yield x
##    tag, body = car(tail), cdr(tail)
##    solver.scont = catch_cont(body, solver.env, scont)
##    return tag.scont(solver)   
##
####@funcont
##def catch_cont(self, tag, solver):
##  body, env = self.arguments[:2]
##  solver.env, solver.scont = env, LabelContinuation(tag, solver, self.cont)
##  return begin(body, solver)
##
##class LabelContinuation:#(Continuation):
##  def __init__(self, tag, solver, cont):
##    Continuation.__init__(self, cont)
##    self.tag, self.block = tag, solver.env
##  def activate(self, solver):
##    return self.cont.on(solver.value, solver)
##  def lookup(self, tag, cont, solver): 
##    if tag==self.tag:
##      form, solver.env = cont.arguments[:2]
##      form.scont(solver.set(throwing_cont(self)))
##    else: self.cont.lookup(tag, cont, solver)
##  
####@funcont
##def throwing_cont(self, value, solver): 
##  return self.cont.unwind(self.cont, solver)
##
##def on_throw(solver, scont, tail): 
##  #[throw tag form]
##  tag, form = car(tail), cadr(tail)
##  solver.scont = throw_cont(form, solver.env, scont)
##  return tag.scont(solver)  
##
####@funcont
##def throw_cont(self, tag, solver):
##  return self.lookup(tag, self, solver)
##
