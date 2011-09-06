from oad import error
##from oad.term import SUCCESS, car, cdr, cadr
from oad import builtin
from oad.rule import Rule

# rule manipulation

@builtin.function()
def abolish(rules, arity):
  del rules[arity.val]
  return SUCCESS

@builtin.function('assert')
def assert_(rules, rule):
  rules.rules[len(car(rule))].append(Rule(car(rule), cdr(rule)))
  return rules

@builtin.function('asserta')
def asserta(rules, rule):
  rules.rules[len(car(rule))].insert(0, Rule(car(rule), cdr(rule)))
  return rules

@builtin.function2('retract')
def retract(evaluator, rules, rule):
  rule = rule.deref(evaluator.trail)
  head, body = car(rule), cdr(rule)
  if len(head) not in rules.rules: return
  rules = rules.rules[len(head)]
  index = 0
  while index<len(rules):
    rule = rules[index]
    oldtrail = evaluator.trail
    evaluator.trail = evaluator.trail.branch()
    try:
      deleted_body = rule.apply(evaluator.trail, head)
      body.unify(deleted_body, evaluator.trail)
    except error.UnifyFail: 
      evaluator.trail = evaluator.trail.revert_upto(oldtrail)
      index += 1
    else: del rules[index]
  return
