# -*- coding: utf-8 -*-

from oad.solve import CutException, mycont
from oad.term import getvalue, unify_list_rule_head, unify, apply_generators

class Rule(object):
  def __init__(self, head, body):
    self.head, self.body = head, body
    self.signature = len(self.head)

  def ___parse___(self, parser):
    return Rule(self.head, parser.parse(self.body))
  def apply(self, solver, env, cont, recursive, values):
    values = [getvalue(v, solver.env) for v in values]
    caller_env = solver.env
    if not recursive: solver.env = env.extend()
    else: 
      env.bindings = {}
      solver.env = env
    subst = {}
    for _ in unify_list_rule_head(values, self.head, solver.env, subst):
      @mycont(cont)
      def rule_done_cont(value, solver):
        self.body
        env_values = tuple(getvalue(v, solver.env) for v in subst.values())
        generators = tuple(set_bindings(caller_env.bindings, k, v) 
                           for k, v in zip(subst.keys(), env_values))
        for _ in apply_generators(generators):
          solver.env = caller_env
          yield cont, value
      yield solver.exps_cont(self.body, rule_done_cont), True
    solver.env = caller_env # must outside of for loop!!!
  def copy(self): return Rule(self.head, self.body)
  def __eq__(self, other): 
    return self.__class__==other.__class__ and self.head==other.head and self.body==other.body
  def __ne__(self, other): return not self==other
  def __repr__(self):
    head = '(%s)'%' '.join(['%s'%repr(a) for a in self.head])
    body = '; '.join(['%s'%' '.join(repr(stmt)) for stmt in self.body])
    return "%s:- %s." % (head, body)

def set_bindings(bindings, var, value):
  try:
    old = bindings[var]
    bindings[var] = value
    yield True
    bindings[var] = old
  except:
    bindings[var] = value
    yield True
    del bindings[var]
    
class RuleList(list):  
  def ___parse___(self, parser):
    return RuleList([parser.parse(rule) for rule in self])
  def apply(self, solver, env, cont, recursive, values):
    def rules_cont(values, solver):
      for rule in self:
        for c, v in rule.apply(solver, env, cont, recursive, values):
          yield c, v
    rules_cont.cut = True
    yield rules_cont, values
  def copy(self):
    return RuleList([r.copy() for r in self])
  def __repr__(self): 
    return '{%s}'%' '.join([repr(rule) for rule in self])
