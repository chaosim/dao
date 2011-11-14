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
    call_path = solver.call_path[:]
    solver.call_path.append(self)
    if not call_data.recursive: solver.env = env.extend()
    else: 
      env.bindings = {}
      solver.env = env
    subst = {}
    sign_state = (call_data.command, call_data.signatures), parse_state
    values = getvalue(values, solver.env, {})
    for _ in unify_list_rule_head(values, self.head, solver.env, subst):
      @mycont(cont)
      def rule_done_cont(value, solver):
        self.body
        env_values = tuple(getvalue(v, solver.env, {}) for v in subst.values())
        generators = tuple(set_bindings(caller_env.bindings, k, v) 
                           for k, v in zip(subst.keys(), env_values))
        for _ in apply_generators(generators):
          solver.env = caller_env
          solver.call_path = call_path
          yield cont, value
          solver.parse_state = parse_state
        solver.env = caller_env
        solver.call_path = call_path
      yield solver.exps_cont(self.body, rule_done_cont), True
    solver.env = caller_env 
    solver.call_path = call_path
    solver.parse_state = parse_state  
    
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
  except KeyError:
    bindings[var] = value
    yield True
    del bindings[var]
    
class RuleList(list): 
  def __init__(self, rules):
    list.__init__(self, rules)
  
  def ___parse___(self, parser):
    return RuleList([parser.parse(rule) for rule in self])
  
  def tag_loop_label(self, tagger):
    return RuleList([tagger.tag_loop_label(rule) for rule in self])
  
  def apply(self, solver, cont, values, call_data):
    def rules_cont(values, solver):
      for rule in self:
        for c, v in rule.apply(solver, cont, values, call_data):
          yield c, v
    rules_cont.cut = True
    yield rules_cont, values
    
  def __repr__(self): 
    return 'RuleList[%s]'%' '.join([repr(rule) for rule in self])
