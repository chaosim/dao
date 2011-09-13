# -*- coding: utf-8 -*-

from oad.term import Apply, Function, Macro, closure
from oad.rule import Rule, RuleList
from oad.solve import value_cont, mycont, translate
from oad.env import BlockEnvironment

# special forms: quote, begin, if, eval, let, lambda, function, macro, module

class SpecialForm:#(UEntity):
  # 具体的特殊式各自定义自己的__init__, __repr__与cont
  def __init__(self, *exps): 
    self.exps = exps
  @classmethod
  def make_special_form(cls, *exps): return cls(*exps)
  def translate(self): 
    return (self.__class__,)+tuple(translate(e) for e in self.exps)
  def __call__(self, *exps): return Apply(self, *exps)

class quote(SpecialForm):
  def __init__(self, exp): self.exp = exp
  def cont(self, cont, solver): return value_cont(self.exp, cont)
  def __repr__(self): return "'%s"%self.exp

class set(SpecialForm):
  def __init__(self, var, exp):
    SpecialForm.__init__(self, var, exp)
    self.var, self.exp = var, exp
  def cont(self, cont, solver):
    @mycont(cont)
    def set_cont(value, solver):
      env = solver.env
      while env is not None:
        if env.hasBindings() and self.var not in env.bindings:
          env = env.outer
        else: break
      else: env = solver.env
      env[self.var] = value
      yield cont, True
    return solver.cont(self.exp, set_cont)
  def __repr__(self): return "set(%s %s)"%(self.var, self.exp)

class begin(SpecialForm):
  def __init__(self, *exps):
    SpecialForm.__init__(self, *exps)
  def cont(self, cont, solver): 
    return solver.exps_cont(self.exps, cont)
  def __repr__(self):
    return 'begin(%s)'%(';'.join([repr(x) for x in self.exps]))
  
class if_(SpecialForm):
  def __init__(self, test, exp1, exp2):
    SpecialForm.__init__(self, test, exp1, exp2)
    self.test, self.exp1, self.exp2 = test, exp1, exp2
  def cont(self, cont, solver):
    @mycont(cont)
    def if_cont(value, solver):
      if value: yield solver.cont(self.exp1, cont), value
      else: yield solver.cont(self.exp2, cont), value
    return solver.cont(self.test, if_cont)
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
    SpecialForm.__init__(self, *rules)
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

class RecursiveFunctionForm(FunctionForm): 
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
  
class MacroForm(FunctionForm):
  def __init__(self, *rules):
    SpecialForm.__init__(self, *rules)
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
    return self.rules[len(exps)].apply(solver, self.env, cont, self.recursive, exps)
  def __repr__(self): return 'macro(%s)'%repr(self.rules)
  
class eval_(SpecialForm):
  def __init__(self, exp):
    SpecialForm.__init__(self, exp)
    self.exp = exp
  def cont(self, cont, solver):
    @mycont(cont)
    def eval_cont(value, solver): return solver.cont(value, cont)(value, solver)
    return solver.cont(self.exp, eval_cont)
  def __repr__(self): return 'eval(%s)'%self.exp

from oad.env import ModuleEnvironment  
class module(SpecialForm):
  def __init__(self, *body):
    SpecialForm.__init__(self, *body)
    #[module ...]
    self.body = body
  def cont(self, cont, solver):
    old_env = solver.env
    env = solver.env = ModuleEnvironment({}, old_env)
    @mycont(cont)
    def module_done_cont(value, solver): 
      solver.env = old_env
      yield cont, env
    return solver.exps_cont(self.body, module_done_cont)

class block(SpecialForm):
  def __init__(self, label, *body):
    SpecialForm.__init__(self, label, *body)
    self.label, self.body = label.name, body
  def cont(self, cont, solver):
    solver.env = BlockEnvironment(self.label, solver.env, cont)
    return solver.exps_cont(self.body, cont)
  def __repr__(self): return 'block(%s)'%self.body

class return_from(SpecialForm):
  def __init__(self, label, form):
    SpecialForm.__init__(self, label, form)
    self.label, self.form = label.name, form
  def cont(self, cont, solver):
    env = solver.env
    @mycont(cont)
    def return_from_cont(value, solver):
      block_cont =  env.lookup(self.label, cont, solver)
      yield block_cont, value
    return solver.cont(self.form, return_from_cont)
  def __repr__(self): return 'return_from(%s)'%self.label

def lookup(cont, tag, stop_cont, solver):
  try: return cont.lookup(cont, tag, stop_cont, solver)
  except AttributeError: 
    return lookup(cont.cont, tag, stop_cont, solver)
  
def unwind(cont, tag, stop_cont, solver):
  try: return cont.unwind(cont, tag, stop_cont, solver)
  except AttributeError: 
    if cont is stop_cont: return cont
    else: return unwind(cont.cont, tag, stop_cont, solver)

def have_lookup(fun):
  def lookup(cont, tag, stop_cont, solver): 
    if tag==cont.tag:
      @mycont(cont)
      def throwing_cont(value, solver): 
        yield unwind(cont, tag, cont, solver), value
      solver.env = cont.env
      return solver.cont(stop_cont.form, throwing_cont)
    else: return lookup(cont, tag, stop_cont, solver)
  fun.lookup = lookup
  return fun

class catch(SpecialForm):
  def __init__(self, tag, *body):
    SpecialForm.__init__(self, tag, *body)
    self.tag, self.body = tag, body
  def cont(self, cont, solver):
    env = solver.env
    @mycont(cont)
    def catch_cont(tag, solver):
      solver.env = env
      @have_lookup
      @mycont(cont)
      def label_cont(value, solver): yield cont, value
      label_cont.tag, label_cont.env = tag, env
      yield solver.exps_cont(self.body, label_cont), True
    catch_cont.env = solver.env  
    return solver.cont(self.tag, catch_cont)
  def __repr__(self): return 'block(%s)'%self.body
  
class throw(SpecialForm):
  def __init__(self, tag, form):
    SpecialForm.__init__(self, tag, form)
    self.tag, self.form = tag, form
  def cont(self, cont, solver):
    @mycont(cont)
    def throw_cont(tag, solver): 
      yield lookup(throw_cont, tag, throw_cont, solver), True
    throw_cont.form, throw_cont.env = self.form, solver.env
    return solver.cont(self.tag, throw_cont)

class unwind_protect(SpecialForm):
  #[unwind-protect form cleanup]
  def __init__(self, form, *cleanup):
    SpecialForm.__init__(self, form, *cleanup)
    self.form, self.cleanup = form, cleanup
  def cont(self, cont, solver):
    env = solver.env
    cont0 = cont
    @mycont(cont0)
    def unwind_protect_cont(value, solver):
      solver.env = env
      @mycont(cont0)
      def protect_return_cont(_, solver): yield cont, value
      yield solver.exps_cont(self.cleanup, protect_return_cont), True
      def unwind1(cont, tag, stop_cont, solver):
        solver.env = env
        @mycont(cont0)
        def unwind_cont(_, solver):
          yield unwind(cont0, tag, stop_cont, solver), value
        return solver.exps_cont(self.cleanup, unwind_cont)
      unwind_protect_cont.unwind = unwind1
    return solver.cont(self.form, unwind_protect_cont)
