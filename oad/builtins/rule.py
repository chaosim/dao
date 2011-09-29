from oad.term import deref, unify_list_rule_head, conslist, getvalue
from oad import error
from oad import builtin
from oad.rule import Rule

# rule manipulation

@builtin.function()
def abolish(rules, arity):
  del rules.rules[arity]
  return True

@builtin.macro('assert')
def assert_(solver, cont, rules, head, *body):
  rules = getvalue(rules, solver.env)
  rules.rules[len(head)].append(Rule(head, body))
  yield cont, rules

@builtin.macro('asserta')
def asserta(solver, cont, rules, head, *body):
  rules = getvalue(rules, solver.env)
  rules.rules[len(head)].insert(0, Rule(head, body))
  yield cont, rules

@builtin.macro('retract')
def retract(solver, cont, rules, head, *body):
  rules = getvalue(rules, solver.env)
  if len(head) not in rules.rules: return
  rules = rules.rules[len(head)]
  index = 0
  while index<len(rules):
    rule = rules[index]
    unified = False
    caller_env = solver.env.extend()
    callee_env = caller_env.extend()
    for _ in unify_list_rule_head(head, rule.head, callee_env, caller_env, set()):
      rule.body = tuple(getvalue(conslist(*body), solver.env))
      index += 1
      unified = True
    if not unified: del rules[index]
  yield cont, rules
