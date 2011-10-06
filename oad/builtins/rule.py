from oad.term import deref, unify_list_rule_head, conslist, getvalue, match
from oad import error
from oad import builtin
from oad.rule import Rule

# rule manipulation

@builtin.function()
def abolish(rules, arity):
  del rules.rules[arity]
  return rules.rules

@builtin.macro('assert')
def assert_(solver, cont, rules, head, body):
  rules = getvalue(rules, solver.env)
  rules.rules[len(head)].append(Rule(head, body))
  yield cont, rules

@builtin.macro('asserta')
def asserta(solver, cont, rules, head, body):
  rules = getvalue(rules, solver.env)
  rules.rules[len(head)].insert(0, Rule(head, body))
  yield cont, rules

# replace the rules which the head can match with.
@builtin.macro('replace')
def replace(solver, cont, rules, head, body):
  rules = getvalue(rules, solver.env)
  if len(head) not in rules.rules: return
  rules = rules.rules[len(head)]
  index = 0
  replaced = False
  while index<len(rules):
    rule = rules[index]
    if match(head, rule.head):
      if not replaced:
        rule.body = tuple(getvalue(conslist(*body), solver.env))
        index += 1
        replaced = True
      else: del rules[index]
    else: index += 1
  yield cont, rules
  
# retract(+Term)                                                    [ISO]
#   When  Term  is an  atom  or a  term  it is  unified with  the  first
#   unifying  fact or clause  in the database.   The  fact or clause  is
#   removed from the database.
@builtin.macro('retract')
def retract(solver, cont, rules, head):
  rules = getvalue(rules, solver.env)
  if len(head) not in rules.rules: return
  rules = rules.rules[len(head)]
  index = 0
  while index<len(rules):
    rule = rules[index]
    caller_env = solver.env.extend()
    callee_env = caller_env.extend()
    for _ in unify_list_rule_head(head, rule.head, callee_env, caller_env, set()):
      del rules[index]
      yield cont, True
      return
  yield cont, True
  
# All  facts or  clauses in the  database for  which the head  unifies
#   with Head are removed.
@builtin.macro('retractall')
def retractall(solver, cont, rules, head):
  rules = getvalue(rules, solver.env)
  if len(head) not in rules.rules: return
  rules = rules.rules[len(head)]
  index = 0
  while index<len(rules):
    unified = False
    caller_env = solver.env.extend()
    callee_env = caller_env.extend()
    for _ in unify_list_rule_head(head, rules[index].head, 
                                  callee_env, caller_env, set()):
      unified = True
      del rules[index]
    if not unified: index += 1
  yield cont, rules

# remove all rules which head matched with.
@builtin.macro('remove')
def remove(solver, cont, rules, head):
  rules = getvalue(rules, solver.env)
  if len(head) not in rules.rules: return
  rules = rules.rules[len(head)]
  index = 0
  while index<len(rules):
    if match(head, rules[index].head):
      del rules[index]
    else: index += 1
  yield cont, rules

