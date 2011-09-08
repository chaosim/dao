# -*- coding: utf-8 -*-

from oad.eval import solve_exps, CutException
from oad.term import getvalue, unify_list_rule_head

class Rule(object):
  def __init__(self, head, body):
    self.head, self.body = head, body
    self.signature = len(self.head)

  def apply(self, evaluator, env, recursive, *exps):
    callerEnv = evaluator.env
    if not recursive: evaluator.env = env.extend()
    else: 
      env.bindings = {}
      evaluator.env = env
    exps = [getvalue(e, evaluator.env) for e in exps]
    for x in unify_list_rule_head(exps, self.head, evaluator.env):
      for x in solve_exps(evaluator, self.body):
        evaluator.env = callerEnv
        yield x
      
  def __eq__(self, other): 
    return self.__class__==other.__class__ and self.head==other.head and self.body==other.body
  def __ne__(self, other): return not self==other
  def __repr__(self):
    head = '(%s)'%' '.join(['%s'%a for a in self.head])
    body = '; '.join(['%s'%stmt for stmt in self.body])
    return "%s:- %s." % (head, body)

class RuleList(list):  
  def apply(self, evaluator, env, recursive, *exps):
    env_bindings = evaluator.env.bindings.copy()
    for rule in self:
      try:
        for x in rule.apply(evaluator, env, recursive, *exps):
          yield x
          evaluator.env.bindings = env_bindings
        else: 
          evaluator.env.bindings = env_bindings
      except CutException: return
      
  def __repr__(self): 
    return '{%s}'%' '.join([repr(rule) for rule in self])
