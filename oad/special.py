# -*- coding: utf-8 -*-

from oad.term import Apply, Function, Macro, closure, Var
from oad.rule import Rule, RuleList
from oad.solve import value_cont, mycont
from oad.env import BlockEnvironment
from oad.builtins.arith import eq, not_
from oad.builtins.type import iter_next, make_iter

# special forms: quote, begin, if, eval, let, lambda, function, macro, module
class ParserForm: 
  def ___parse___(self, parser): 
    return self

class SpecialForm(ParserForm):
  # 具体的特殊式各自定义自己的__init__, __repr__与cont
  def __call__(self, *exps): return Apply(self, *exps)
  def __add__(self, other): 
    return begin(self, other)
  def __or__(self, other): 
    from oad.builtins.control import or_
    return or_(self, other)

class quote(SpecialForm):
  def __init__(self, exp): self.exp = exp
  def cont(self, cont, solver): return value_cont(self.exp, cont)
  def __eq__(self, other): return self.exp==other.exp
  def __repr__(self): return "'%s"%self.exp

def assign_var(var, value, env):
##  print 'set',
  env0 = env
  while env is not None:
    if env.hasBindings() and var not in env.bindings:
      env = env.outer
    else: 
      in_env = True
      break
  else: 
    in_env = False
    env = env0
  if in_env: old = env[var]
  else: old = None
  env[var] = value
  return env, in_env, old
  
class set(SpecialForm):
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
  def ___parse___(self, parser): 
    self.exp = parser.parse(self.exp)
    return self
  def cont(self, cont, solver):
    @mycont(cont)
    def set_cont(value, solver):
      env, in_env, old = assign_var(self.var, value, solver.env)
      yield cont, value
      if in_env: env[self.var] = old
      else: del env[self.var]
    return solver.cont(self.exp, set_cont)
  def __eq__(self, other): return self.var==other.var and self.exp==other.exp
  def __repr__(self): return "set(%s %s)"%(self.var, self.exp)
assign = set

class set_list(SpecialForm):
  def __init__(self, vars, exp):
    self.vars, self.exp = vars, exp
  def ___parse___(self, parser): 
    self.exp = parser.parse(self.exp)
    return self
  def cont(self, cont, solver):
    @mycont(cont)
    def set_cont(values, solver):
      if len(values)!=len(self.vars): raise ValueError(values)
      modifies = [assign_var(var, v, solver.env) for var, v in zip(self.vars, values)]
      yield cont, True
      for env, in_env, old in modifies:
        if in_env: env[self.var] = old
        else: del env[self.var]

    return solver.cont(self.exp, set_cont)
  def __eq__(self, other): return self.vars==other.vars and self.exp==other.exp
  def __repr__(self): return "set(%s %s)"%(self.vars, self.exp)
assign = set

class begin(SpecialForm):
  def __init__(self, *exps):
    self.exps = exps
  def cont(self, cont, solver): 
    return solver.exps_cont(self.exps, cont)
  def ___parse___(self, parser): 
    self.exps = tuple(parser.parse(exp) for exp in self.exps)
    return self
  def __eq__(self, other): 
    return self.exps==other.exps
  def __repr__(self):
    return 'begin(%s)'%(';'.join([repr(x) for x in self.exps]))
  
def make_if_cont(then, els, cont):
  @mycont(cont)
  def if_cont(value, solver):
    if value: yield solver.cont(then, cont), value
    elif els is not None: yield solver.cont(els, cont), value
    else: yield cont, value
  return if_cont

class if_(SpecialForm):
  def __init__(self, test, exp1, exp2=None):
    self.test, self.exp1, self.exp2 = test, exp1, exp2
  def ___parse___(self, parser): 
    self.exp1 = parser.parse(self.exp1)
    self.exp2 = parser.parse(self.exp2)
    return self
  def cont(self, cont, solver):
    if_cont = make_if_cont(self.exp1, self.exp2, cont)
    return solver.cont(self.test, if_cont)
  def __repr__(self):
    return 'if %s: %s else:%s'%(self.test, self.exp1, self.exp2)

def make_iff_cont(then, clauses, els, cont):
  @mycont(cont)
  def iff_cont(value, solver):
    if value: yield solver.cont(then, cont), value
    else:
      if len(clauses)==1: ifcont = make_if_cont(clauses[0][1], els, cont)
      else: ifcont = make_iff_cont(clauses[0][1], clauses[1:], els, cont)
      yield solver.cont(clauses[0][0], ifcont), value
  return iff_cont
  
class iff(SpecialForm):
  def __init__(self, clauses, els=None):
    self.clauses, self.els = clauses, els
  def ___parse___(self, parser): 
    self.clauses = [parser.parse(clause) for clause in self.clauses]
    self.els = parser.parse(self.els)
    return self
  def cont(self, cont, solver):
    if len(self.clauses)==1: 
      ifcont = make_if_cont(self.clauses[0][1], self.els, cont)
    else:  
      ifcont = make_iff_cont(self.clauses[0][1], self.clauses[1:], self.els, cont)
    return solver.cont(self.clauses[0][0], ifcont)
  def __eq__(self, other):
    return self.clauses==other.clauses and self.els==other.els
  def __repr__(self):
    result = 'if %s then %s; '%self.clauses[0]
    result += ''.join('elif %s then %s; '%clause for clause in self.clauses[1:])
    result += 'els %s'%self.els
    return result

class pytry(SpecialForm):
  def __init__(self, body, exception, clause, final=None):
    self.body = body
    self.exception, self.clause, self.final = exception, clause, final
  def ___parse___(self, parser): 
    for clause in self.clauses:
      clause[1] = parse(clause[1], parser)
    self.els = parse(self.els, parser)
    return self
  def cont(self, cont, solver):
    @mycont(cont)
    def pytry_cont(value, solver):
      try:
        for value in solver.solve(self.body):
          yield cont, value
      except self.exception, e: 
        for value in solver.solve(self.clause):
          yield cont, value
      finally:
        if self.final is None: return
        for value in solver.solve(self.final):
          yield cont, value
    return pytry_cont    
  def __eq__(self, other):
    return self.clauses==other.clauses and self.els==other.els
  def __repr__(self):
    result = 'if %s then %s; '%self.clauses[0]
    result += ''.join('elif %s then %s; '%clause for clause in self.clauses[1:])
    result += 'els %s'%self.els
    return result


class CaseForm(SpecialForm):
  def __init__(self, test, cases, els=None):
    self.test, self.cases, self.els = test, cases, els
  def ___parse___(self, parser): 
    for k in self.cases:
      self.cases[k] = parser.parse(self.cases[k])
    self.els = parser.parse(self.els)
    return self
  def cont(self, cont, solver):
    @mycont(cont)
    def case_cont(value, solver):
      try: exps = self.cases[value]
      except:  exps = self.els
      yield solver.exps_cont(exps, cont), value
    return solver.cont(self.test, case_cont)
  def __eq__(self, other):
    return self.test==other.test and self.cases==other.cases and self.els==other.els
  def __repr__(self):
    result = 'case %s=> '%self.test
    result += ''.join('of %s: %s; '%(repr(k), repr(v)) for k,v in self.cases.items())
    result += 'els %s'%self.els
    return result

class loop(SpecialForm):
  def __init__(self, *body):
    self.body = body
  def ___parse___(self, parser): 
    self.body = parser.parse(self.body)
    return self
  def cont(self, cont, solver):
    @mycont(cont)
    def loop_cont(value, solver):
      yield solver.exps_cont(self.body, loop_cont), value
    return solver.exps_cont(self.body, loop_cont)
  def __eq__(self, other):
    return self.body==other.body
  def __repr__(self):
    return 'loop{%s}'%repr(self.body)

class LoopForm(ParserForm):
  def __init__(self, body, label=None):
    self.body, self.label = body, label
  
  def ___parse___(self, parser):
    exit_label, next_label = parser.make_label(self.label)
    parser.push_label('loop', exit_label, next_label)
    body = parser.parse(self.body)
    parser.pop_label('loop')
    return block(exit_label, loop(block(next_label, *body)))
  def __eq__(self, other):
    return self.body==other.body
  def __repr__(self):
    return 'LoopForm(%s)'%(self.body)

class LoopTimesForm(ParserForm):
  def __init__(self, times, body, label=None):
    self.times, self.body, self.label = times, body, label
  def ___parse___(self, parser):
    exit_label, next_label = parser.make_label(self.label)
    parser.push_label('loop', exit_label, next_label)
    body = parser.parse(self.body)
    parser.pop_label('loop')
    i = Var('i')
    return block(exit_label, set(i, self.times),loop(
      block(next_label, if_(eq(i,0), return_from(exit_label)), set(i, i-1), 
          *body)))
  def __eq__(self, other):
    return self.times==other.times and self.body==other.body
  def __repr__(self):
    return 'LoopTimesForm(%s,%s)'%(self.times, self.body)

class WhenLoopForm(ParserForm):
  def __init__(self, body, condition):
    self.body, self.condition = body, condition
  def ___parse___(self, parser): 
    exit_label, next_label = parser.make_label(self.label)
    parser.push_label('when', exit_label, next_label)
    body = parser.parse(self.body)
    parser.pop_label('when')
    next_body = [if_(not_(self.condition), return_from(exit_label))]+body
    return block(exit_label, loop(block(next_label, next_body)))

class LoopWhenForm(ParserForm):
  def __init__(self, body, condition, label=None):
    self.body, self.condition, self.label = body, condition, label
  def ___parse___(self, parser): 
    exit_label, next_label = parser.make_label(self.label)
    parser.push_label('when', exit_label, next_label)
    body = parser.parse(self.body)
    parser.pop_label('when')
    next_body = body+(if_(not_(self.condition), return_from(exit_label)), )
    return block(exit_label, loop(block(next_label, *next_body)))
  def __eq__(self, other):
    return self.body==other.body and self.condition==other.condition
  def __repr__(self):
    return 'LoopWhenForm(%s,%s)'%(self.body, self.condition)

class LoopUntilForm(ParserForm):
  def __init__(self, body, condition, label=None):
    self.body, self.condition, self.label = body, condition, label
  def ___parse___(self, parser): 
    exit_label, next_label = parser.make_label(self.label)
    parser.push_label('until', exit_label, next_label) 
    body = parser.parse(self.body)
    parser.pop_label('until')
    next_body = body+(if_(self.condition, return_from(exit_label)), )
    return block(exit_label, loop(block(next_label, *next_body)))
  def __eq__(self, other):
    return self.body==other.body and self.condition==other.condition
  def __repr__(self):
    return 'LoopUntilForm(%s,%s)'%(self.body, self.condition)

class EachForm(ParserForm):
  def __init__(self, vars, iterator, body, label=None):
    self.vars, self.iterator, self.body, self.label = vars, iterator, body, label
  def ___parse___(self, parser): 
    exit_label, next_label = parser.make_label(self.label)
    parser.push_label('each', exit_label, next_label)
    body = parser.parse(self.body)
    parser.pop_label('each')
    iterator = Var('iterator')
    if isinstance(self.vars, Var):
      setvar = set(self.vars, iter_next(iterator))
    else:
      setvar = set_list(self.vars, iter_next(iterator))
    return block(exit_label, let({iterator:make_iter(self.iterator)},
          loop(block(next_label, 
            pytry(setvar, StopIteration, return_from(exit_label)),
          *body))))
  def __eq__(self, other):
    return self.vars==other.vars and self.iterator==other.iterator and self.body==other.body
  def __repr__(self):
    return 'EachForm(%s, %s,%s)'%(repr(self.vars), self.iterator, self.body)

class OnForm(ParserForm):
  def __init__(self, form, body, var=None):
    self.form, self.body, self.var = form, body, var
  def cont(self, solver):
    @mycont(cont)
    def on_cont(value, solver):
      with value:
        if self.var is not None:
          for _ in self.var.unify(form_value, solver.env):
            for v in solver.solve(body, cont):
              yield cont, v
        else:
          for v in solver.solve(body, cont):
            yield cont, v
    return solver.cont(self.form, cont)

class exit(ParserForm):
  def __init__(self, value=None, type=None, label=None): 
    self.value, self.type, self.label = value, type, label
  def ___parse___(self, parser):
    if self.label is None:
      return return_from(parser.exit_labels[self.type][-1], self.value)
    else:
      return return_from('exit_'+self.label+parser.surfix, self.value)
  def __repr__(self): return "exit"

class next(ParserForm):
  def __init__(self, type=None, label=None): 
    self.type, self.label = type, label
  def ___parse___(self, parser):
    if self.label is None:
      return return_from(parser.next_labels[self.type][-1])
    else:
      return return_from('next_'+self.label+parser.surfix)
  def __repr__(self): return "next"

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
  def ___parse___(self, parser):
    for arity, rule_list in self.arity2rules.items():
      self.arity2rules[arity] = parser.parse(rule_list)
    return self
  def cont(self, cont, solver):
    func = UserFunction(self.arity2rules, solver.env, recursive=False)
    return value_cont(func, cont)
  def __eq__(self, other):
    return self.arity2rules==other.arity2rules
  def __repr__(self):
    result = 'func('
    for rules in self.arity2rules.values():
      result += '%s'%rules
    result += ')'
    return result

function = FunctionForm

def letrec(bindings, *body):
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
    self.exp = exp
  def cont(self, cont, solver):
    @mycont(cont)
    def eval_cont(value, solver): return solver.cont(value, cont)(value, solver)
    return solver.cont(self.exp, eval_cont)
  def __repr__(self): return 'eval(%s)'%self.exp

from oad.env import ModuleEnvironment  
class module(SpecialForm):
  def __init__(self, *body):
    #[module ...]
    self.body = body
  def ___parse___(self, parser):
    self.body = parser.parse(self.body)
    return self
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
    self.label, self.body = label, body
  def ___parse___(self, parser): 
    self.body = parser.parse(self.body)
    return self
  def cont(self, cont, solver):
    solver.env = BlockEnvironment(self.label, solver.env, cont)
    return solver.exps_cont(self.body, cont)
  def __eq__(self, other):
    return self.label==other.label and self.body==other.body
  def __repr__(self): return 'block(%s)'%repr(self.body)

class return_from(SpecialForm):
  def __init__(self, label, form=None):
    self.label, self.form = label, form
  def ___parse___(self, parser):
    self.form = parser.parse(self.form)
    return self
  def cont(self, cont, solver):
    env = solver.env
    @mycont(cont)
    def return_from_cont(value, solver):
      block_cont =  env.lookup(self.label, cont, solver)
      yield block_cont, value
    return solver.cont(self.form, return_from_cont)
  def __eq__(self, other):
    return self.label==other.label and self.form==other.form
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
    self.tag, self.body = tag, body
  def ___parse___(self, parser):
    self.tag = parser.parse(self.tag)
    self.body = parser.parse(self.body)
    return self
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
    self.tag, self.form = tag, form
  def ___parse___(self, parser):
    self.tag = parser.parse(self.tag)
    self.form = parser.parse(self.form)
    return self
  def cont(self, cont, solver):
    @mycont(cont)
    def throw_cont(tag, solver): 
      yield lookup(throw_cont, tag, throw_cont, solver), True
    throw_cont.form, throw_cont.env = self.form, solver.env
    return solver.cont(self.tag, throw_cont)

class unwind_protect(SpecialForm):
  #[unwind-protect form cleanup]
  def __init__(self, form, *cleanup):
    self.form, self.cleanup = form, cleanup
  def ___parse___(self, parser):
    self.form = parser.parse(self.form)
    self.cleanup = parser.parse(self.cleanup)
    return self
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
