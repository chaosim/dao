# -*- coding: utf-8 -*-

from oad.eval import solve_exps
from oad.term import getvalue, unify_rule_head, UnifyFail

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
    exps = [getvalue(e, evaluator.trail) for e in exps]
    for h, p in zip(self.head, exps):
      unify_rule_head(p, h, evaluator.trail)
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
    undotrail = evaluator.trail
    evaluator.trail = evaluator.trail.branch()
    env_bindings = evaluator.env.bindings.copy()
    for i, rule in enumerate(self):
      try:
        for x in rule.apply(evaluator, env, recursive, *exps):
          yield x
      except UnifyFail: 
        if i==len(self)-1: raise UnifyFail
        else:
          evaluator.trail = evaluator.trail.revert_upto(undotrail, discard_choicepoint=True)
          evaluator.env.bindings = env_bindings
##      except CutException: return
      
  def __repr__(self): 
    return '{%s}'%' '.join([repr(rule) for rule in self])
