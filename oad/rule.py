# -*- coding: utf-8 -*-

from oad.solve import CutException, mycont
from oad.term import getvalue, unify_list_rule_head, unify

class Rule(object):
  def __init__(self, head, body):
    self.head, self.body = head, body
    self.signature = len(self.head)

  def apply(self, solver, env, cont, recursive, values):
    caller_env = solver.env
##    caller_env_bindings = solver.env.bindings.copy()
    if not recursive: solver.env = env.extend()
    else: 
      env.bindings = {}
      solver.env = env
    values = [getvalue(v, solver.env) for v in values]
    for binding_set in unify_list_rule_head(values, self.head, solver.env, caller_env):
      @mycont(cont)
      def rule_done_cont(value, solver):
        self.body
        old_bindings = {}
        for v in binding_set:
          old_bindings[v] = v_value = v.getvalue(caller_env)
          caller_env.bindings[v] = getvalue(v_value, solver.env)
        solver.env = caller_env
        yield cont, value
        for v in binding_set: caller_env.bindings[v] = old_bindings[v]
      yield solver.exps_cont(self.body, rule_done_cont), True
      solver.env = caller_env
##      solver.env.bindings = caller_env_bindings
  def __eq__(self, other): 
    return self.__class__==other.__class__ and self.head==other.head and self.body==other.body
  def __ne__(self, other): return not self==other
  def __repr__(self):
    head = '(%s)'%' '.join(['%s'%repr(a) for a in self.head])
    body = '; '.join(['%s'%' '.join(repr(stmt)) for stmt in self.body])
    return "%s:- %s." % (head, body)

class RuleList(list):  
  def apply(self, solver, env, cont, recursive, values):
    def rules_cont(values, solver):
      stream = solver.stream
      for rule in self:
        for c, v in rule.apply(solver, env, cont, recursive, values):
          yield c, v
          solver.stream = stream
    rules_cont.cut = True
    yield rules_cont, values
  def __repr__(self): 
    return '{%s}'%' '.join([repr(rule) for rule in self])
