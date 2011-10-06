# -*- coding: utf-8 -*-

from oad.solve import CutException, mycont
from oad.term import getvalue, unify_list_rule_head, unify

class Rule(object):
  def __init__(self, head, body):
    self.head, self.body = head, body
    self.signature = len(self.head)

  def parse(self, parser):
    return Rule(self.head, parser.parse(self.body))
  def apply(self, solver, env, cont, recursive, values):
    caller_env = solver.env
    if not recursive: solver.env = env.extend()
    else: 
      env.bindings = {}
      solver.env = env
    values = [getvalue(v, solver.env) for v in values]
    for binding_set in unify_list_rule_head(values, self.head, 
                                            solver.env, caller_env, set()):
      @mycont(cont)
      def rule_done_cont(value, solver):
##        self.body
        old_bindings = {}
        for v in binding_set:
          old_bindings[v] = v_value = v.getvalue(caller_env)
          caller_env.bindings[v] = getvalue(v_value, solver.env)
        solver.env = caller_env
        yield cont, value
        for v in binding_set: 
          caller_env.bindings[v] = old_bindings[v]
        solver.env = caller_env
      yield solver.exps_cont(self.body, rule_done_cont), True
      solver.env = caller_env
  def copy(self): return Rule(self.head, self.body)
  def __eq__(self, other): 
    return self.__class__==other.__class__ and self.head==other.head and self.body==other.body
  def __ne__(self, other): return not self==other
  def __repr__(self):
    head = '(%s)'%' '.join(['%s'%repr(a) for a in self.head])
    body = '; '.join(['%s'%' '.join(repr(stmt)) for stmt in self.body])
    return "%s:- %s." % (head, body)

class RuleList(list):  
  def parse(self, parser):
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
