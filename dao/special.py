# -*- coding: utf-8 -*-

pyset = set

from dao.term import CommandCall, Function, Macro, closure, Var, ClosureVar#, Command
from dao.term import apply_generators, rule_head_signatures
from dao.rule import Rule, RuleList
from dao.solve import value_cont, mycont, tag_unwind, DaoSyntaxError, to_sexpression
from dao.solve import BaseCommand
from dao.env import BlockEnvironment
from dao.builtins.arith import eq, not_
from dao.builtins.container import iter_next, make_iter
from dao import builtin

from dao.solve import run_mode, set_run_mode, interactive, noninteractive
from dao.solve import interactive_parser, interactive_tagger

# compile
from dao.compiler import ValueCont, code

# special forms: quote, begin, if, eval, let, lambda, function, macro, module
class ParserForm(object): 
  def ___parse___(self, parser): return self

class SpecialForm(BaseCommand, ParserForm):
  is_global = True

  def __call__(self, *exps): return CommandCall(self, *exps)
  def __add__(self, other): return begin(self, other)
  def __or__(self, other): 
    from dao.builtins.control import or_p
    return or_p(self, other)

class quote(SpecialForm):
  name = 'quote'
  symbol = "'"
  def __init__(self, exp): self.exp = exp
  def to_sexpression(self):
    return (quote, to_sexpression(self.exp))
  def cont(self, cont, solver): 
    return value_cont(self.exp, cont)
  def compile_to_cont(self, cont, compiler):
    return ValueCont(self.exp, cont)
  def __eq__(self, other): return self.exp==other.exp
  def __repr__(self): 
    if run_mode()==interactive:
      set_run_mode(noninteractive)
      code  = interactive_parser().parse(self.exp)
      code  = interactive_tagger().tag_loop_label(code)
      result = "%s"%code
      set_run_mode(interactive)
      return result
    else: 
      return "'%s"%self.exp

# do not restore the value of var in assign
# if need to restore, use define instead.

class SetCont:
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
  def code(self):
    return '%s = %s'%(code(self.var), code(self.exp))

# assign var in the most inner env
class set(SpecialForm):
  name = 'set'
  symbol = "set"
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
  def ___parse___(self, parser):
    self.var = parser.parse(self.var)
    self.exp = parser.parse(self.exp)
    return self
  def tag_loop_label(self, tagger): 
    self.exp = tagger.tag_loop_label(self.exp)
    return self
  def to_sexpression(self):
    return (set, self.var, to_sexpression(self.exp))
  def cont(self, cont, solver):
    @mycont(cont)
    def set_cont(value, solver):
      solver.env.bindings[self.var] = value
      solver.scont = cont
      return value
    return solver.cont(self.exp, set_cont)
  def compile_to_cont(self, cont, compiler):
    return SetCont(self.var, compiler.compile_to_cont(self.exp, cont))
  def __eq__(self, other): return self.var==other.var and self.exp==other.exp
  def __repr__(self): return "set(%s, %s)"%(self.var, self.exp)
assign = set

# assign var in the most inner env
class set_list(SpecialForm):
  def __init__(self, vars, exp):
    self.vars, self.exp = tuple(vars), exp
  def ___parse___(self, parser): 
    self.vars = parser.parse(self.vars)
    self.exp = parser.parse(self.exp)
    return self
  def tag_loop_label(self, tagger): 
    self.exp = tagger.tag_loop_label(self.exp)
    return self
  def to_sexpression(self):
    return (set_list, self.vars, to_sexpression(self.exp))
  def cont(self, cont, solver):
    @mycont(cont)
    def set_cont(values, solver):
      if len(values)!=len(self.vars): raise ValueError(values)
      for var, value in zip(self.vars, values):
        solver.env[var] = value
      solver.scont = cont
      return None
    return solver.cont(self.exp, set_cont)
  def __eq__(self, other): return self.vars==other.vars and self.exp==other.exp
  def __repr__(self): return "set(%s %s)"%(self.vars, self.exp)
assign = set

# do not restore the side effect of assing!!!
class get_outer(SpecialForm):
  name = 'get_outer'
  symbol = "get_outer"
  def __init__(self, var):
    self.var = var
  def ___parse___(self, parser):
    self.var = parser.parse(self.var)
    self.exp = parser.parse(self.exp)
    return self
  def tag_loop_label(self, tagger): 
    self.exp = tagger.tag_loop_label(self.exp)
    return self
  def to_sexpression(self):
    return (get_outer, self.var)
  def cont(self, cont, solver):
    return value_cont(solver.env.outer[self.var], cont)
  def __eq__(self, other): return self.var==other.var and self.exp==other.exp
  def __repr__(self): return "get_outer(%s)"%(self.var)

class set_outer(SpecialForm):
  name = 'set_outer'
  symbol = "set_outer"
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
  def ___parse___(self, parser):
    self.var = parser.parse(self.var)
    self.exp = parser.parse(self.exp)
    return self
  def tag_loop_label(self, tagger): 
    self.exp = tagger.tag_loop_label(self.exp)
    return self
  def to_sexpression(self):
    return (set_outer, self.var, to_sexpression(self.exp))
  def cont(self, cont, solver):
    @mycont(cont)
    def set_cont(value, solver):
      if env is not solver.global_env: 
        env = solver.env.outer
      while env is not None:
        try: 
          env.bindings[var] = value
          solver.scont = cont
          return value
        except KeyError: env = env.outer
      env = env or solver.env.outer
      env.bindings[self.var] = value 
    return solver.cont(self.exp, set_cont)
  def __eq__(self, other): return self.var==other.var and self.exp==other.exp
  def __repr__(self): return "set_outer(%s, %s)"%(self.var, self.exp)

class get_global(SpecialForm):
  name = 'get_global'
  symbol = "get_global"
  def __init__(self, var):
    self.var = var
  def ___parse___(self, parser):
    self.var = parser.parse(self.var)
    self.exp = parser.parse(self.exp)
    return self
  def tag_loop_label(self, tagger): 
    self.exp = tagger.tag_loop_label(self.exp)
    return self
  def to_sexpression(self):
    return (get_global, self.var)
  def cont(self, cont, solver):
    return value_cont(self.global_env.bindings[self.var])
  def __eq__(self, other): return self.var==other.var
  def __repr__(self): return "get_global(%s, %s)"%(self.var, self.exp)
  
class set_global(SpecialForm):
  name = 'set_global'
  symbol = "set_global"
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
  def ___parse___(self, parser):
    self.var = parser.parse(self.var)
    self.exp = parser.parse(self.exp)
    return self
  def tag_loop_label(self, tagger): 
    self.exp = tagger.tag_loop_label(self.exp)
    return self
  def to_sexpression(self):
    return (set_global, self.var, to_sexpression(self.exp))
  def cont(self, cont, solver):
    @mycont(cont)
    def set_cont(value, solver):
      self.global_env.bindings[var] = value
      solver.scont = cont
      return value
    return solver.cont(self.exp, set_cont)
  def __eq__(self, other): return self.var==other.var and self.exp==other.exp
  def __repr__(self): return "set_global(%s, %s)"%(self.var, self.exp)

class BeginCont:
  def __init__(self, exps):
    self.exps = exps
  def code(self):
    return '; '.join([code(x) for x in self.exps])
    
class begin(SpecialForm):
  name = 'begin'
  def __init__(self, *exps):
    self.exps = exps
  def ___parse___(self, parser): 
    self.exps = tuple(parser.parse(exp) for exp in self.exps)
    return self
  def tag_loop_label(self, tagger): 
    self.exps = tuple(tagger.tag_loop_label(exp) for exp in self.exps)
    return self
  def to_sexpression(self):
    return (begin, )+to_sexpression(self.exps)
  def cont(self, cont, solver): 
    return solver.exps_cont(self.exps, cont)
  def compile_to_cont(self, cont, compiler): 
    return BeginCont(compiler.compile_to_cont(x, cont) for x in self.exps)
  def __eq__(self, other): 
    return self.exps==other.exps
  def __repr__(self):
    return 'begin(%s)'%(';'.join([repr(x) for x in self.exps]))
  
def make_if_cont(then, els, cont):
  @mycont(cont)
  def if_cont(value, solver):
    if value: 
      solver.scont = solver.cont(then, cont)
      return value
    elif els is not None: 
      solver.scont = solver.cont(els, cont)
      return value
    else: 
      solver.scont = cont
      return None
  return if_cont

class if_(SpecialForm):
  symbol = 'if'
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
  def to_sexpression(self):
    return (if_, )+to_sexpression((self.test, self.exp1, self.exp2))
  def cont(self, cont, solver):
    if_cont = make_if_cont(self.exp1, self.exp2, cont)
    return solver.cont(self.test, if_cont)
  def __repr__(self):
    els = 'else: %s'%repr(self.exp2) if self.exp2 else ''
    return 'if %s: %s%s'%(self.test, self.exp1, els)

def make_iff_cont(then, clauses, els, cont):
  @mycont(cont)
  def iff_cont(value, solver):
    if value: 
      solver.scont = solver.cont(then, cont)
      return value
    else:
      if len(clauses)==1: ifcont = make_if_cont(clauses[0][1], els, cont)
      else: ifcont = make_iff_cont(clauses[0][1], clauses[1:], els, cont)
      solver.scont = solver.cont(clauses[0][0], ifcont)
      return value
  return iff_cont

class iff(SpecialForm):
  name = 'iff'
  symbol = 'iff'
  def __init__(self, clauses, els=None):
    self.clauses, self.els = tuple(clauses), els
  def ___parse___(self, parser): 
    self.clauses = tuple(parser.parse(clause) for clause in self.clauses)
    self.els = parser.parse(self.els)
    return self
  def tag_loop_label(self, tagger): 
    self.clauses = tuple(tagger.tag_loop_label(clause) for clause in self.clauses)
    self.els = tagger.tag_loop_label(self.els)
    return self
  def to_sexpression(self):
    return (iff, to_sexpression(self.clauses), to_sexpression(self.els))
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
    #self.ex_clause = parser.parse(self.ex_clause)
    #self.body = parser.parse(self.body)
    #self.final = parser.parse(self.final)
    return self
  def tag_loop_label(self, parser): 
    #self.ex_clause = tagger.tag_loop_label(self.ex_clause)
    #self.body = tagger.tag_loop_label(self.body)
    #self.final = tagger.tag_loop_label(self.final)
    return self
  def to_sexpression(self):
    return (pytry,)+to_sexpression((self.body, self.exception, self.ex_clause, self.final))
  def cont(self, cont, solver):
    old_fcont1 = solver.fcont
    try_cont_gen = solver.exp_run_cont(self.body, cont)
    def pytry_cont(value, solver):
      try:
        solver.scont, v = try_cont_gen.next()
        return v
      except StopIteration:
        solver.scont = old_fcont1
        return value
      except self.exception, e: 
        except_cont_gen = solver.exp_run_cont(self.ex_clause, cont)
        old_fcont2 = solver.fcont
        def except_cont(value, solver):
          try: 
            solver.scont, v = except_cont_gen.next()
            return v
          except StopIteration:
            solver.scont = old_fcont2
            return value
        solver.scont = solver.fcont = except_cont
        return value
      finally:
        if self.final is None: 
          self.scont = cont
          return value
        solver.scont = solver.cont(self.final, cont)
        return value
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
  def to_sexpression(self):
    cases = dict((k, to_sexpression(tuple(v))) for k, v in self.cases.items())
    return (CaseForm,)+to_sexpression((self.test, cases, self.els))
  def cont(self, cont, solver):
    @mycont(cont)
    def case_cont(value, solver):
      try: exps = self.cases[value]
      except:  exps = self.els
      solver.scont = solver.exps_cont(exps, cont)
      return value
    return solver.cont(self.test, case_cont)
  def __eq__(self, other):
    return self.test==other.test and self.cases==other.cases and self.els==other.els
  def __repr__(self):
    result = 'case %s=> '%self.test
    result += ''.join('of %s: %s; '%(repr(k), repr(v)) for k,v in self.cases.items())
    result += 'els %s'%self.els
    return result

case = CaseForm

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
    return begin(set(i, self.times), block(label, *body), None)
  def __eq__(self, other):
    return self.times==other.times and self.body==other.body
  def __repr__(self):
    label = self.label+': ' if self.label else ''
    body = ', '.join([repr(e) for e in self.body])
    return 'Loop(%s)[%s%s]'%(self.times, label, body)

class WhenLoopForm(RepeatForm):
  def __init__(self, condition, body, label=None):
    self.body, self.condition, self.label = body, condition, label
  def ___parse___(self, parser): return self
  def tag_loop_label(self, tagger):
    label = tagger.make_label(self.label)
    tagger.push_label('when', label)
    body = tagger.tag_loop_label(self.body)
    tagger.pop_label('when')
    start_condition = [if_(not_(self.condition), exit_block(label))]
    return begin(block(label, *(start_condition+list(body)+[continue_block(label)])), None)
  def __eq__(self, other):
    return self.body==other.body and self.condition==other.condition
  def __repr__(self):
    return 'WhenLoopForm(%s,%s)'%(self.condition, self.body)
  
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
    return begin(block(label, *body), None)
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
    return begin(block(label, *body), None)
  def __eq__(self, other):
    return self.body==other.body and self.condition==other.condition
  def __repr__(self):
    return 'LoopUntilForm(%s,%s)'%(self.body, self.condition)

from dao.solve import DaoStopIteration
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
    return set(iterator, make_iter(quote(self.iterator)))+\
           block(label, 
                  pytry(setvar, DaoStopIteration, exit_block(label)),
                  *(body+[continue_block(label)]))
  def __eq__(self, other):
    return self.vars==other.vars and self.iterator==other.iterator and self.body==other.body
  def __repr__(self):
    return 'EachForm(%s, %s,%s)'%(repr(self.vars), self.iterator, self.body)

class exit(ParserForm):
  def __init__(self, value=None, type=None, level=1, label=None): 
    self.value, self.type, self.level, self.label = value, type, level, label
  def tag_loop_label(self, tagger):
    if self.label is None:
      try: 
        labels = tagger.labels[self.type]
        return exit_block(labels[-self.level], self.value)
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
        return continue_block(labels[-self.level])
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
  def to_sexpression(self):
    return (OnForm,)+to_sexpression(self.form, self.body, self.var)
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

class let(SpecialForm):
  symbol = 'let'
  def __init__(self, bindings, *body):
    self.bindings, self.body = tuple(bindings), body
  def ___parse___(self, parser):
    self.bindings = tuple((b[0], parser.parse(b[1])) for b in self.bindings)
    self.body = parser.parse(self.body)
    return self
  def tag_loop_label(self, tagger):
    self.bindings = tuple((b[0], tagger.tag_loop_label(b[1])) for b in self.bindings)
    self.body = tagger.tag_loop_label(self.body)
    return self
  def to_sexpression(self):
    self.bindings = to_sexpression(self.bindings)
    self.body = to_sexpression(self.body)
    return (self.__class__, self.bindings)+self.body
  def cont(self, cont, solver):
    vars = tuple(b[0] for b in self.bindings)
    values = tuple(b[1] for b in self.bindings)
    return  solver.cont(((FunctionForm,((vars,)+self.body)),)+values, cont)
  def __eq__(self, other):
    return self.bindings==other.bindings and self.body==other.body
  def __repr__(self):
    return 'let %s: %s'%(repr(self.bindings), self.body)
  
class letr(let):
  symbol = 'letr'
  def cont(self, cont, solver):
    vars = tuple(b[0] for b in self.bindings)
    values = tuple(b[1] for b in self.bindings)
    return  solver.cont(((RecursiveFunctionForm,((vars,)+self.body)),)+values, cont)

  def __repr__(self):
    return 'letr %s: %s'%(repr(self.bindings), self.body)
  
#[lambda [var ...] ...]
class lambda_(SpecialForm):
  symbol = 'lambda'
  def __init__(self, vars, *body):
    self.vars, self.body = vars, body
  def ___parse___(self, parser):
    self.body = parser.parse(self.body)
    return self
  def tag_loop_label(self, tagger):
    self.body = tagger.tag_loop_label(self.body)
    return self
  def to_sexpression(self):
    return (self.__class__, self.vars)+to_sexpression(self.body)
  def cont(self, cont, solver):
    return solver.cont((FunctionForm, ((self.vars,)+self.body)), cont)
  def __repr__(self):
    return 'lambda %s: %s'%(repr(self.vars), self.body)
  
def make_rules(rules):
  arity2rules, arity2signatures = {}, {}
  for rule in rules:
    head = rule[0]
    rule = Rule(head, rule[1:])
    arity = len(head)
    arity2rules.setdefault(arity, []).append(rule)
    if arity==0: continue
    for signature in rule_head_signatures(head):
      arity_signature = arity2signatures.setdefault(arity, {})
      arity_signature.setdefault(signature, pyset()).add(len(arity2rules[arity])-1)
  return arity2rules, arity2signatures

class FunctionForm(SpecialForm):
  symbol = 'function'
  def __init__(self, *rules):
    self.rules = tuple((tuple(rule[0]),)+tuple(rule[1:]) for rule in rules)
    
  def ___parse___(self, parser):
    self.rules = tuple(parser.parse(rule) for rule in self.rules)
    return self
  def tag_loop_label(self, tagger):
    self.rules = tuple(tagger.tag_loop_label(rule) for rule in self.rules)
    return self
  def to_sexpression(self):
    return (self.__class__,)+to_sexpression(self.rules)
  def cont(self, cont, solver):
    arity2rules, signature2rules = make_rules(self.rules)
    func = UserFunction(arity2rules, signature2rules, solver.env, recursive=False)
    return value_cont(func, cont)
  def __eq__(self, other):
    return isinstance(other, FunctionForm) and self.rules==other.rules
  def __repr__(self):
    result = 'func('
    for rule in self.rules:
      result += '%s'%repr(rule)
    result += ')'
    return result

function = FunctionForm

class RecursiveFunctionForm(FunctionForm): 
  def cont(self, cont, solver):
    newEnv = solver.env.extend({})
    solver.env = newEnv
    arity2rules, signature2rules = make_rules(self.rules)
    func = UserFunction(arity2rules, signature2rules, newEnv, recursive=True)
    return value_cont(func, cont)
  def __repr__(self):
    result = 'recfunc('
    for rule in self.rules:
      result += '%s'%repr(rule)
    result += ')'
    return result
  
class MacroForm(FunctionForm):
  symbol = 'macro'
  def cont(self, cont, solver):
    arity2rules, signature2rules = make_rules(self.rules)
    macro = UserMacro(arity2rules, signature2rules, solver.env, recursive=False)
    return value_cont(macro, cont)
  def __repr__(self):
    result = 'macroform('
    for rule in self.rules:
      result += '%s'%repr(rule)
    result += ')'
    return result

macro = MacroForm

class CallData:
  def __init__(self, command, signatures, env, recursive):
    self.command, self.signatures, self.env, self.recursive = (
      command, signatures, env, recursive)
  
class Rules:
  def __init__(self, arity2rules, signature2rules, env, recursive): 
    self.arity2rules, self.signature2rules = arity2rules, signature2rules
    self.env = env
    self.recursive = recursive
    
  def apply(self, solver, cont, values, signatures):
    arity = len(values)
    if arity==0:
      rule_list = RuleList(self.arity2rules[0])
      if len(rule_list)==0: 
        solver.scont = solver.fcont
        return
    else:
      arity2rules = self.arity2rules[arity]
      sign2index = self.signature2rules[arity]
      index_set = pyset(range(len(arity2rules)))
      for signature in signatures:
        if signature==(signature[0], Var): continue
        else:
          var_sign = signature[0], Var
          index_set &= sign2index.get(var_sign, pyset())|\
                       sign2index.get(signature, pyset())
      if len(index_set)==0: 
        solver.scont = solver.fcont
        return
      rule_list = list(index_set)
      rule_list.sort()
      rule_list = RuleList([arity2rules[i] for i in rule_list])
    call_data = CallData(self, signatures, self.env, self.recursive)
    return rule_list.apply(solver, cont, values, call_data)
          
class UserFunction(Rules,  Function):
  memorable = True
  def __repr__(self):return 'fun(%s)'%repr(self.arity2rules)
  
class UserMacro(Rules,  Macro):
  memorable = True
  def apply(self, solver, cont, values, signatures):
    @mycont(cont)
    def eval_macro_result_cont(value, solver):
      solver.scont = solver.cont(value, cont)
      return value
    return Rules.apply(self, solver, eval_macro_result_cont, values, signatures)
  def __repr__(self): return 'macro(%s)'%repr(self.arity2rules)
  
@builtin.predicate('eval')
def eval_(solver, cont, exp):
  @mycont(cont)
  def eval_cont(value, solver): 
    solver.scont = solver.cont(value, cont)
    return value
  solver.scont = solver.cont(exp, eval_cont)
  return True
  
from dao.env import ModuleEnvironment 

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
  def to_sexpression(self):
    return (module, )+to_sexpression(self.body)
  def cont(self, cont, solver):
    old_env = solver.env
    env = solver.env = ModuleEnvironment({}, old_env, '')
    @mycont(cont)
    def module_done_cont(value, solver): 
      solver.env = old_env
      env.outer = None
      solver.scont = cont
      return env
    return solver.exps_cont(self.body, module_done_cont)

class in_module(SpecialForm):
  def __init__(self, module_env, *body):
    #[module ...]
    self.module_env, self.body = module_env, body
  def ___parse___(self, parser):
    self.body = parser.parse(self.body)
    return self
  def tag_loop_label(self, tagger):
    self.body = tagger.tag_loop_label(self.body)
    return self
  def to_sexpression(self):
    self.body = to_sexpression(self.body)
    return (in_module, self.module_env)+self.body
  def cont(self, cont, solver):
    env = self.module_env
    while env.outer is not solver.global_env:
      env = env.outer
    env.outer = old_env = solver.env
    solver.env = self.module_env
    @mycont(cont)
    def in_module_done_cont(value, solver): 
      solver.env = old_env
      env.outer = solver.global_env
      solver.scont = cont
      return value
    return solver.exps_cont(self.body, in_module_done_cont)

@builtin.macro('from_', 'from')
def from_(solver, cont, module, var):  
  if isinstance(var, ClosureVar): var = var.var
  @mycont(cont)
  def from_module_cont(module, solver):
    solver.scont = cont
    return module.lookup(var)
  solver.scont = solver.cont(module, from_module_cont)
  return module
  
class block(SpecialForm):
  def __init__(self, label, *body):
    self.label, self.body = label, body
  def ___parse___(self, parser): 
    self.body = parser.parse(self.body)
    return self
  def tag_loop_label(self, tagger):
    self.body = tagger.tag_loop_label(self.body)
    return self
  def to_sexpression(self):
    return (block, self.label)+to_sexpression(self.body)
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
  name = 'exit_block'
  symbol = 'return-from'
  def __init__(self, label, form=None):
    self.label, self.form = label, form
  def ___parse___(self, parser):
    self.form = parser.parse(self.form)
    return self
  def tag_loop_label(self, tagger):
    self.form = tagger.tag_loop_label(self.form)
    return self
  def to_sexpression(self):
    return (exit_block,)+to_sexpression((self.label, self.form))
  def cont(self, cont, solver):
    env = solver.env
    @mycont(cont)
    def exit_block_cont(value, solver):
      exit_cont =  env.lookup_exit_cont(self.label, cont, value, solver)
      solver.scont = exit_cont
      return value
    return solver.cont(self.form, exit_block_cont)
  def __eq__(self, other):
    return self.label==other.label and self.form==other.form
  def __repr__(self): return 'exit_block(%s)'%self.label

class continue_block(SpecialForm):
  def __init__(self, label):
    self.label = label
  def ___parse___(self, parser): return self
  def to_sexpression(self):
    return (continue_block, self.label)
  def cont(self, cont, solver):
    env = solver.env
    @mycont(cont)
    def continue_block_cont(value, solver):
      next_cont =  env.lookup_next_cont(self.label, cont, solver)
      solver.scont = next_cont
      return value
    return continue_block_cont
  def __eq__(self, other): return self.label==other.label
  def __repr__(self): return 'continue_block(%s)'%self.label

def lookup(cont, tag, stop_cont, solver):
  try: return cont.lookup(cont, tag, stop_cont, solver)
  except AttributeError: 
    return lookup(cont.cont, tag, stop_cont, solver)
  
from dao.solve import tag_lookup
from dao.env import unwind
def label_cont_lookup(cont, tag, stop_cont, solver): 
  if tag==cont.tag:
    @mycont(cont)
    def throwing_cont(value, solver): 
      solver.scont = unwind(stop_cont, value, tag, cont, solver)
      return value
    solver.env = cont.env
    return solver.cont(stop_cont.form, throwing_cont)
  else: return lookup(cont, tag, stop_cont, solver)

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
  def to_sexpression(self):
    return (catch, to_sexpression(self.tag)) +to_sexpression(self.body)
  def cont(self, cont, solver):
    env = solver.env # not necessary?
    @mycont(cont)
    def catch_cont(tag, solver):
      solver.env = env # not necessary?
      @tag_lookup(label_cont_lookup)
      @mycont(cont)
      def label_cont(value, solver): 
        solver.scont = cont
        return value
      label_cont.tag, label_cont.env = tag, env
      solver.scont = solver.exps_cont(self.body, label_cont)
      return True
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
  def to_sexpression(self):
    return (throw,)+to_sexpression((self.tag, self.form))
  def cont(self, cont, solver):
    @mycont(cont)
    def throw_cont(tag, solver): 
      solver.scont = lookup(throw_cont, tag, throw_cont, solver)
      return True
    throw_cont.form, throw_cont.env = self.form, solver.env
    return solver.cont(self.tag, throw_cont)
  
  def __repr__(self): return 'throw(%s,%s)'%(repr(self.tag),repr(self.form))
  
class unwind_protect(SpecialForm):
  name = 'unwind_protect'
  symbol = 'unwind-protect'
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
  
  def to_sexpression(self):
    return (unwind_protect, to_sexpression(self.form))+to_sexpression(self.cleanup)
  
  def cont(self, cont, solver):
    env = solver.env
    cont0 = cont
    
    def unwind_protect_cont_unwind(cont, form_value, tag, stop_cont, solver, next_cont):
      solver.env = env
      @mycont(cont0)
      def unwind_cont(value, solver):
        solver.scont = unwind(cont0, form_value, tag, stop_cont, solver, next_cont)
        return form_value
      return solver.exps_cont(self.cleanup, unwind_cont)
    
    @tag_unwind(unwind_protect_cont_unwind)
    @mycont(cont0)
    def unwind_protect_cont(value, solver):      
      @mycont(cont0)
      def protect_return_cont(_, solver): 
        solver.scont = cont0
        return value
      solver.env = env
      solver.scont = solver.exps_cont(self.cleanup, protect_return_cont)
      return True
      
    return solver.cont(self.form, unwind_protect_cont)
