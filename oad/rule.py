# -*- coding: utf-8 -*-

from oad.solve import CutException
from oad.term import getvalue, unify_list_rule_head

class Rule(object):
  def __init__(self, head, body):
    self.head, self.body = head, body
    self.signature = len(self.head)

  def apply(self, solver, env, cont, recursive, values):
    callerEnv = solver.env
    if not recursive: solver.env = env.extend()
    else: 
      env.bindings = {}
      solver.env = env
    values = [getvalue(v, solver.env) for v in values]
    for _ in unify_list_rule_head(values, self.head, solver.env):
      yield solver.exps_cont(self.body, cont), True
      
  def __eq__(self, other): 
    return self.__class__==other.__class__ and self.head==other.head and self.body==other.body
  def __ne__(self, other): return not self==other
  def __repr__(self):
    head = '(%s)'%' '.join(['%s'%a for a in self.head])
    body = '; '.join(['%s'%' '.join(repr(stmt)) for stmt in self.body])
    return "%s:- %s." % (head, body)

class RuleList(list):  
  def apply(self, solver, env, cont, recursive, values):
    def rules_cont(values, solver):
      env_bindings = solver.env.bindings.copy()
      stream = solver.stream
      for rule in self:
        for c, v in rule.apply(solver, env, cont, recursive, values):
          yield c, v
          solver.stream = stream
          solver.env.bindings = env_bindings
        else: solver.env.bindings = env_bindings
    rules_cont.cut = True
    yield rules_cont, values
  def __repr__(self): 
    return '{%s}'%' '.join([repr(rule) for rule in self])
