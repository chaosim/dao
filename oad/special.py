# -*- coding: utf-8 -*-

from oad.term import Apply, Function, Macro, closure, Var, ClosureVar
from oad.term import apply_generators
from oad.rule import Rule, RuleList
from oad.solve import value_cont, mycont, tag_unwind, DaoSyntaxError
from oad.env import BlockEnvironment
from oad.builtins.arith import eq, not_
from oad.builtins.term import iter_next, make_iter
from oad import builtin

# special forms: quote, begin, if, eval, let, lambda, function, macro, module
class ParserForm: 
  def ___parse___(self, parser): return self

class SpecialForm(ParserForm):
  # 具体的特殊式各自定义自己的__init__, __repr__与cont
  def __call__(self, *exps): return Apply(self, *exps)
  def __add__(self, other): return begin(self, other)
  def __or__(self, other): 
    from oad.builtins.control import or_
    return or_(self, other)

class quote(SpecialForm):
  def __init__(self, exp): self.exp = exp
  def cont(self, cont, solver): return value_cont(self.exp, cont)
  def __eq__(self, other): return self.exp==other.exp
  def __repr__(self): return "'%s"%self.exp

def assign_var(var, value, env):
  env0 = env
  while env is not None:
    try: 
      old = env.bindings[var]
      env.bindings[var] = value
      yield True
      env.bindings[var] = old
      return
    except KeyError: env = env.outer
  env0.bindings[var] = value
  yield True
  del env0.bindings[var]
  
class set(SpecialForm):
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
  def ___parse___(self, parser):
    self.var = parser.parse(self.var)
    self.exp = parser.parse(self.exp)
    return self
  def tag_loop_label(self, tagger): 
    self.exp = tagger.tag_loop_label(self.exp)
    return self
  def cont(self, cont, solver):
    @mycont(cont)
    def set_cont(value, solver):
      for _ in assign_var(self.var, value, solver.env):
        yield cont, value
    return solver.cont(self.exp, set_cont)
  def __eq__(self, other): return self.var==other.var and self.exp==other.exp
  def __repr__(self): return "set(%s, %s)"%(self.var, self.exp)
assign = set

class set_list(SpecialForm):
  def __init__(self, vars, exp):
    self.vars, self.exp = vars, exp
  def ___parse___(self, parser): 
    self.vars = parser.parse(self.vars)
    self.exp = parser.parse(self.exp)
    return self
  def tag_loop_label(self, tagger): 
    self.exp = tagger.tag_loop_label(self.exp)
    return self
  def cont(self, cont, solver):
    @mycont(cont)
    def set_cont(values, solver):
      if len(values)!=len(self.vars): raise ValueError(values)
      for _ in apply_generators([assign_var(var, v, solver.env) 
                                 for var, v in zip(self.vars, values)]):
        yield cont, True

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
  def tag_loop_label(self, tagger): 
    self.exps = tuple(tagger.tag_loop_label(exp) for exp in self.exps)
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
  def tag_loop_label(self, tagger): 
    self.exp1 = tagger.tag_loop_label(self.exp1)
    self.exp2 = tagger.tag_loop_label(self.exp2)
    return self
  def cont(self, cont, solver):
    if_cont = make_if_cont(self.exp1, self.exp2, cont)
    return solver.cont(self.test, if_cont)
  def __repr__(self):
    els = 'else: %s'%repr(self.exp2) if self.exp2 else ''
    return 'if %s: %s%s'%(self.test, self.exp1, els)

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
  def tag_loop_label(self, tagger): 
    self.clauses = [tagger.tag_loop_label(clause) for clause in self.clauses]
    self.els = tagger.tag_loop_label(self.els)
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
    result = 'if %s: %s'%self.clauses[0]
    result += ''.join(' elif %s: %s'%clause for clause in self.clauses[1:])
    result += ' els: %s'%self.els if self.els else ''
    return result

class pytry(SpecialForm):
  def __init__(self, body, exception, ex_clause, final=None):
    self.body = body
    self.exception, self.ex_clause, self.final = exception, ex_clause, final
  def ___parse___(self, parser): 
    for clause in self.clauses:
      clause[1] = parser.parse(clause[1])
    self.els = parser.parse(self.els)
    return self
  def tag_loop_label(self, tagger): 
    for clause in self.clauses:
      clause[1] = tagger.tag_loop_label(clause[1])
    self.els = tagger.tag_loop_label(self.els)
    return self
  def cont(self, cont, solver):
    @mycont(cont)
    def pytry_cont(value, solver):
      try:
        for c, value in solver.exp_run_cont(self.body, cont):
          yield c, value
      except self.exception, e: 
        for c, v in solver.exp_run_cont(self.ex_clause, cont):#
          yield c, v
      finally:
        if self.final is None: return
        for c, value in solver.exp_run_cont(self.final, cont):
          yield c, value
    return pytry_cont    
  def __eq__(self, other):
    return self.clauses==other.clauses and self.els==other.els
  def __repr__(self):
    return 'pytry: %s except %s: %s%s'%(
      repr(self.body), repr(self.exception), repr(self.ex_clause), 
      ' finally: %s'%repr(self.final) if self.final else '')


class CaseForm(SpecialForm):
  def __init__(self, test, cases, els=None):
    self.test, self.cases, self.els = test, cases, els
  def ___parse___(self, parser): 
    for k in self.cases:
      self.cases[k] = parser.parse(self.cases[k])
    self.els = parser.parse(self.els)
    return self
  def tag_loop_label(self, tagger): 
    for k in self.cases:
      self.cases[k] = tagger.tag_loop_label(self.cases[k])
    self.els = tagger.tag_loop_label(self.els)
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

class RepeatForm(ParserForm): pass

class LoopForm(RepeatForm):
  def __init__(self, body, label=None):
    self.body, self.label = body, label
  
  def ___parse___(self, parser): 
    return self
  def tag_loop_label(self, tagger):
    label = tagger.make_label(self.label)
    tagger.push_label('loop', label)
    body = tagger.tag_loop_label(self.body)
    tagger.pop_label('loop')
    return block(label, *(body+(continue_block(label),)))
  def __eq__(self, other):
    return self.body==other.body
  def __repr__(self):
    label = self.label+': ' if self.label else ''
    body = ', '.join([repr(e) for e in self.body])
    return 'Loop[%s%s]'%(label,body)

class LoopTimesForm(RepeatForm):
  def __init__(self, times, body, label=None):
    self.times, self.body, self.label = times, body, label
  def ___parse___(self, parser): return self
  def tag_loop_label(self, tagger):
    label = tagger.make_label(self.label)
    tagger.push_label('times', label)
    body = tagger.tag_loop_label(self.body)
    tagger.pop_label('times')
    i = Var('loop_i')
    start_condition = (if_(eq(i,0), exit_block(label)), set(i, i-1))
    body = start_condition+tuple(body)+(continue_block(label),)
    return begin(set(i, self.times), block(label, *body))
  def __eq__(self, other):
    return self.times==other.times and self.body==other.body
  def __repr__(self):
    label = self.label+': ' if self.label else ''
    body = ', '.join([repr(e) for e in self.body])
    return 'Loop(%s)[%s%s]'%(self.times, label, body)

class WhenLoopForm(RepeatForm):
  def __init__(self, body, condition):
    self.body, self.condition = body, condition
  def ___parse___(self, parser): return self
  def tag_loop_label(self, tagger):
    label = tagger.make_label(self.label)
    tagger.push_label('when', label)
    body = tagger.tag_loop_label(self.body)
    tagger.pop_label('when')
    start_condition = [if_(not_p(self.condition), exit_block(label))]
    return block(label, *(start_condition+body+[continue_block(label)]))
  def __eq__(self, other):
    return self.body==other.body and self.condition==other.condition
  def __repr__(self):
    return 'WhenLoopForm(%s,%s)'%(self.body, self.condition)
  
class LoopWhenForm(RepeatForm):
  def __init__(self, body, condition, label=None):
    self.body, self.condition, self.label = body, condition, label
  def ___parse___(self, parser): return self
  def tag_loop_label(self, tagger):
    label = tagger.make_label(self.label)
    tagger.push_label('when', label)
    body = tagger.tag_loop_label(self.body)
    tagger.pop_label('when')
    body = body+(if_(self.condition, continue_block(label)), )
    return block(label, *body)
  def __eq__(self, other):
    return self.body==other.body and self.condition==other.condition
  def __repr__(self):
    return 'LoopWhenForm(%s,%s)'%(self.body, self.condition)

class LoopUntilForm(RepeatForm):
  def __init__(self, body, condition, label=None):
    self.body, self.condition, self.label = body, condition, label
  def ___parse___(self, parser): return self
  def tag_loop_label(self, tagger):
    label = tagger.make_label(self.label)
    tagger.push_label('until', label)
    body = tagger.tag_loop_label(self.body)
    tagger.pop_label('until')
    body = body+(if_(not_(self.condition), continue_block(label)), )
    return block(label, *body)
  def __eq__(self, other):
    return self.body==other.body and self.condition==other.condition
  def __repr__(self):
    return 'LoopUntilForm(%s,%s)'%(self.body, self.condition)

from oad.solve import DaoStopIteration
class EachForm(RepeatForm):
  def __init__(self, vars, iterator, body, label=None):
    self.vars, self.iterator, self.body, self.label = vars, iterator, body, label
  def ___parse___(self, parser): return self
  def tag_loop_label(self, tagger):
    label = tagger.make_label(self.label)
    tagger.push_label('each', label)
    body = list(tagger.tag_loop_label(self.body))
    tagger.pop_label('each')
    iterator = Var('loop_iterator')
    if isinstance(self.vars, Var):
      setvar = set(self.vars, iter_next(iterator))
    else:
      setvar = set_list(self.vars, iter_next(iterator))
    return set(iterator, make_iter(self.iterator))+\
           block(label, 
                  pytry(setvar, DaoStopIteration, exit_block(label)),
                  *(body+[continue_block(label)]))
  def __eq__(self, other):
    return self.vars==other.vars and self.iterator==other.iterator and self.body==other.body
  def __repr__(self):
    return 'EachForm(%s, %s,%s)'%(repr(self.vars), self.iterator, self.body)

class exit(ParserForm):
  def __init__(self, value=None, type=None, level=0, label=None): 
    self.value, self.type, self.level, self.label = value, type, level, label
  def tag_loop_label(self, tagger):
    if self.label is None:
      try: 
        labels = tagger.labels[self.type]
        return exit_block(labels[len(labels)-1-self.level], self.value)
      except: raise DaoSyntaxError(self)
    else:
      return exit_block(self.label, self.value)
  def __eq__(self, other):
    return self.value==other.value and self.type==other.type \
           and self.level==other.level and self.label==other.label
  def __repr__(self):
    result = 'exit'
    if self.label: result += '.'+self.label
    elif self.type: result += ' '+self.type
    if self.value: result += ' '+repr(self.value)
    return result

class next(ParserForm):
  def __init__(self, type=None, level=0, label=None): 
    self.type, self.level, self.label = type, level, label
  def tag_loop_label(self, tagger):
    if self.label is None:
      try: 
        labels = tagger.labels[self.type]
        return continue_block(labels[len(labels)-1-self.level])
      except: raise DaoSyntaxError
    else:
      return continue_block(self.label)
  def __eq__(self, other):
    return self.type==other.type \
           and self.level==other.level and self.label==other.label
  def __repr__(self):
    result = 'next'
    if self.label: result += '.'+self.label
    elif self.type: result += ' '+self.type
    return result

class OnForm(ParserForm):
  def __init__(self, form, body, var=None):
    self.form, self.body, self.var = form, body, var
  def ___parse___(self, parser): 
    self.form = parser.___parse___(self.form)
    self.body = parser.___parse___(self.body)
    return self
  def tag_loop_label(self, tagger): 
    self.form = tagger.tag_loop_label(self.form)
    self.body = tagger.tag_loop_label(self.body)
    return self
  def cont(self, solver):
    @mycont(cont)
    def on_cont(value, solver):
      with value:
        if self.var is not None:
          for _ in self.var.unify(form_value, solver.env):
            for c, v in solver.exp_run_cont(body, cont):
              yield c, v
        else:
          for c, v in solver.exp_run_cont(body, cont):
            yield c, v
    return solver.cont(self.form, cont)

# which distinct by the strict and lazy evaluation of the arguments.
# the implentations is based on "Lisp In Small Pieces" by Christian Queinnec and Ecole Polytechnique

def let(bindings, *body):
  if isinstance(bindings, dict): raise Error
  vars = tuple(b[0] for b in bindings)
  values = tuple(b[1] for b in bindings)
  return FunctionForm((vars,)+ body)(*values)
  
def lambda_(vars, *body): 
  #[lambda [var ...] ...]
  return FunctionForm((vars,)+body)

def make_rules(rules):
  result = {}
  for rule in rules:
    result.setdefault(len(rule[0]), RuleList()).append(Rule(rule[0], rule[1:]))
  return result

class FunctionForm(SpecialForm):
  def __init__(self, *rules):
    self.arity2rules = make_rules(rules)
  def ___parse___(self, parser):
    for arity, rule_list in self.arity2rules.items():
      self.arity2rules[arity] = parser.parse(rule_list)
    return self
  def tag_loop_label(self, tagger):
    for arity, rule_list in self.arity2rules.items():
      self.arity2rules[arity] = tagger.tag_loop_label(rule_list)
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
  if isinstance(bindings, dict): raise Error
  vars = tuple(b[0] for b in bindings)
  values = tuple(b[1] for b in bindings)
  return RecursiveFunctionForm((vars,)+ body)(*values)

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
    self.arity2rules = self.arity2rules = make_rules(rules)
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
  def ___parse___(self, parser):
    self.exp = parser.parse(self.exp)
    return self
  def tag_loop_label(self, tagger):
    self.exp = tagger.tag_loop_label(self.exp)
    return self
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
  def tag_loop_label(self, tagger):
    self.body = tagger.tag_loop_label(self.body)
    return self
  def cont(self, cont, solver):
    old_env = solver.env
    env = solver.env = ModuleEnvironment({}, old_env)
    @mycont(cont)
    def module_done_cont(value, solver): 
      solver.env = old_env
      yield cont, env
    return solver.exps_cont(self.body, module_done_cont)

@builtin.macro()
def from_(solver, cont, module, var):
  if isinstance(var, ClosureVar): var = var.var
  @mycont(cont)
  def from_module_cont(module, solver): 
    yield cont, module[var]
  yield solver.cont(module, from_module_cont), module
  
class block(SpecialForm):
  def __init__(self, label, *body):
    self.label, self.body = label, body
  def ___parse___(self, parser): 
    self.body = parser.parse(self.body)
    return self
  def tag_loop_label(self, tagger):
    self.body = tagger.tag_loop_label(self.body)
    return self
  def cont(self, cont, solver):
    block_env = BlockEnvironment(self.label, solver.env, cont, None)
    solver.env = block_env
    next_cont = solver.exps_cont(self.body, cont)
    block_env.next_cont = next_cont
    return next_cont
  def __eq__(self, other):
    return self.label==other.label and self.body==other.body
  def __repr__(self):
    label = self.label+': ' if self.label else ''
    body = ', '.join([repr(e) for e in self.body])
    return '[%s%s]'%(label, body)

class exit_block(SpecialForm):
  def __init__(self, label, form=None):
    self.label, self.form = label, form
  def ___parse___(self, parser):
    self.form = parser.parse(self.form)
    return self
  def tag_loop_label(self, tagger):
    self.form = tagger.tag_loop_label(self.form)
    return self
  def cont(self, cont, solver):
    env = solver.env
    @mycont(cont)
    def exit_block_cont(value, solver):
      exit_cont =  env.lookup_exit_cont(self.label, cont, solver)
      yield exit_cont, value
    return solver.cont(self.form, exit_block_cont)
  def __eq__(self, other):
    return self.label==other.label and self.form==other.form
  def __repr__(self): return 'exit_block(%s)'%self.label

class continue_block(SpecialForm):
  def __init__(self, label):
    self.label = label
  def ___parse___(self, parser): return self
  def cont(self, cont, solver):
    env = solver.env
    @mycont(cont)
    def continue_block_cont(value, solver):
      next_cont =  env.lookup_next_cont(self.label, cont, solver)
      yield next_cont, value
    return continue_block_cont
  def __eq__(self, other): return self.label==other.label
  def __repr__(self): return 'continue_block(%s)'%self.label

def lookup(cont, tag, stop_cont, solver):
  try: return cont.lookup(cont, tag, stop_cont, solver)
  except AttributeError: 
    return lookup(cont.cont, tag, stop_cont, solver)
  
from oad.env import unwind
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
  def tag_loop_label(self, tagger):
    self.tag = tagger.tag_loop_label(self.tag)
    self.body = tagger.tag_loop_label(self.body)
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
  def tag_loop_label(self, tagger):
    self.tag = tagger.tag_loop_label(self.tag)
    self.form = tagger.tag_loop_label(self.form)
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
  def tag_loop_label(self, tagger):
    self.cleanup = tagger.tag_loop_label(self.cleanup)
    self.form = tagger.tag_loop_label(self.form)
    return self
  def cont(self, cont, solver):
    env = solver.env
    cont0 = cont
    
    def unwind_protect_cont_unwind(cont, tag, stop_cont, solver, next_cont):
      solver.env = env
      @mycont(cont0)
      def unwind_cont(value, solver):
        yield unwind(cont0, tag, stop_cont, solver, next_cont), value
      return solver.exps_cont(self.cleanup, unwind_cont)
    
    @tag_unwind(unwind_protect_cont_unwind)
    @mycont(cont0)
    def unwind_protect_cont(value, solver):      
      @mycont(cont0)
      def protect_return_cont(_, solver): yield cont0, value
      solver.env = env
      yield solver.exps_cont(self.cleanup, cont0), True
      
    return solver.cont(self.form, unwind_protect_cont)
