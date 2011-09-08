from oad import error
##from oad.term import True, car, cdr, cadr
from oad import builtin
from oad.rule import Rule

# rule manipulation

@builtin.function()
def abolish(rules, arity):
  del rules[arity.val]
  return True

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
  rule = rule.deref(evaluator.env)
  head, body = car(rule), cdr(rule)
  if len(head) not in rules.rules: return
  rules = rules.rules[len(head)]
  index = 0
  while index<len(rules):
    rule = rules[index]
    oldtrail = evaluator.env
    evaluator.env = evaluator.env.branch()
    try:
      deleted_body = rule.apply(evaluator.env, head)
      body.unify(deleted_body, evaluator.env)
    except error.UnifyFail: 
      evaluator.env = evaluator.env.revert_upto(oldtrail)
      index += 1
    else: del rules[index]
  return
