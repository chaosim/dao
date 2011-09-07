# -*- coding: utf-8 -*-

from oad.eval import solve_exps
from oad.term import getvalue, unify_list_rule_head, UnifyFail

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
    exps = [getvalue(e) for e in exps]
    for x in unify_list_rule_head(exps, self.head):
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
    for i, rule in enumerate(self):
      for x in rule.apply(evaluator, env, recursive, *exps):
        solved = True
        yield x
        if i==len(self)-1: return
        else:
          evaluator.env.bindings = env_bindings
##      except CutException: return
      
  def __repr__(self): 
    return '{%s}'%' '.join([repr(rule) for rule in self])
