# -*- coding: utf-8 -*-

from dao.solve import CutException, mycont
from dao.term import getvalue, unify_list_rule_head, unify, apply_generators

class Rule(object):
  def __init__(self, head, body):
    self.head, self.body = head, body

  def ___parse___(self, parser):
    return Rule(self.head, parser.parse(self.body))
  
  def tag_loop_label(self, tagger):
    return Rule(self.head, tagger.tag_loop_label(self.body))
  
  def apply(self, solver, cont, values, call_data):
    parse_state = solver.parse_state
    caller_env = solver.env
    env = call_data.env
    call_path = solver.call_path
    solver.call_path = solver.call_path+[self]
    if not call_data.recursive: solver.env = env.extend({})
    else: 
      env.bindings = {}
      solver.env = env
    subst = {}
    sign_state = (call_data.command, call_data.signatures), parse_state
    values = getvalue(values, solver.env, {})
    if not unify_list_rule_head(values, self.head, solver, subst):
      solver.scont = solver.fcont
      return
    @mycont(cont)
    def rule_done_cont(value, solver):
      self.body
      env_values = tuple(getvalue(v, solver.env, {}) for v in subst.values())
      for k, v in zip(subst.keys(), env_values):
        set_bindings(caller_env.bindings, k, v, solver) 
      solver.env = caller_env
      solver.call_path = call_path
      old_fcont = solver.fcont
      def fcont(value, solver):
        solver.parse_state = parse_state
        solver.env = caller_env
        solver.call_path = call_path
        solver.fcont = old_fcont
      solver.fcont = fcont
      solver.scont = cont
      return value
    old_fcont = solver.fcont
    def fcont(value, solver):
      solver.env = caller_env 
      solver.call_path = call_path
      solver.parse_state = parse_state  
      solver.fcont = old_fcont
    solver.fcont = fcont
    solver.scont = solver.exps_cont(self.body, rule_done_cont)
    return True
    
  def copy(self): return Rule(self.head, self.body)
  
  def __eq__(self, other): 
    return self.__class__==other.__class__ and self.head==other.head and self.body==other.body
  def __ne__(self, other): return not self==other
  
  def __repr__(self):
    head = '(%s)'%' '.join(['%s'%repr(a) for a in self.head])
    body = '; '.join(['%s'%' '.join(repr(stmt)) for stmt in self.body])
    return "%s:- %s." % (head, body)

def set_bindings(bindings, var, value, solver):
  try:
    old = bindings[var]
    bindings[var] = value
    old_fcont = solver.fcont
    def fcont(value, solver):
      bindings[var] = old
      solver.fcont = old_fcont
    solver.fcont = fcont
    return True
  except KeyError:
    bindings[var] = value
    old_fcont = solver.fcont
    def fcont(value, solver):
      del bindings[var]
      solver.fcont = old_fcont
    solver.fcont = fcont
    return True
    
    
class RuleList(list): 
  def __init__(self, rules):
    list.__init__(self, rules)
  
  def ___parse___(self, parser):
    return RuleList([parser.parse(rule) for rule in self])
  
  def tag_loop_label(self, tagger):
    return RuleList([tagger.tag_loop_label(rule) for rule in self])
  
  def apply(self, solver, cont, values, call_data):
    if len(self)>2:
      old_fcont = solver.fcont
      def fcont(value, solver):
        return RuleList(self[1:]).apply(solver, cont, values, call_data)
      solver.fcont = fcont
    elif len(self)==2:
      old_fcont = solver.fcont
      def fcont(value, solver):
        solver.fcont = old_fcont
        return self[1].apply(solver, cont, values, call_data)
      solver.fcont = fcont
    return self[0].apply(solver, cont, values, call_data)
    
  def __repr__(self): 
    return 'RuleList[%s]'%' '.join([repr(rule) for rule in self])
