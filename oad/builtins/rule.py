from oad.term import deref, unify_list_rule_head, conslist, getvalue, match, Var
##from oad import error
from oad import builtin
from oad.rule import Rule, RuleList
from oad.special import UserFunction, UserMacro, make_rules

# rule manipulation

@builtin.macro()
def abolish(solver, cont, rules, arity):
  rules = getvalue(rules, solver.cont)
  if not isinstance(rules, UserFunction) and not isinstance(rules, UserMacro):
    yield cont, rules
  arity = deref(arity, solver.cont)
  if arity not in rules.rules:
    yield cont, rules.rules
    return
  old = rules.rules[arity]
  del rules.rules[arity]
  yield cont, rules.rules
  rules[arity] = old

@builtin.macro('assert')
def assert_(solver, cont, rules, head, body):
  rules = getvalue(rules, solver.env)
  rules.rules[len(head)].append(Rule(head, body))
  yield cont, rules
  del rules.rules[-1]

@builtin.macro('append_def')
def append_def(solver, cont, rules, head, bodies, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if not isinstance(rules, klass): raise ValueError(rules)
  for body in bodies:
    rules.rules[len(head)].append(Rule(head, body))
  yield cont, rules
  del rules.rules[len(head)][-len(bodies):]

@builtin.macro('asserta')
def asserta(solver, cont, rules, head, body, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if not isinstance(rules, klass): raise ValueError(rules)
  rules.rules[len(head)].insert(0, Rule(head, body))
  yield cont, rules
  del rules.rules[0]

@builtin.macro('asserta')
def insert_def(solver, cont, rules, head, bodies, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if not isinstance(rules, klass): raise ValueError(rules)
  if len(head) not in rules.rules:
    rules.rules[len(head)] = RuleList([Rule(head, body) for body in bodies])
    yield cont, rules
    del rules.rules[len(head)]
    return
  for body in reversed(bodies):
    rules.rules[len(head)].insert(0, Rule(head, body))
  yield cont, rules
  del rules.rules[len(head)][0:len(bodies)]

# replace the rules which the head can match with.
@builtin.macro('replace')
def replace(solver, cont, rules, head, *body):
  rules = getvalue(rules, solver.env)
  if isinstance(rules, Var):
    solver.env[rules] = FunctionForm((head, body))
    yield cont, rules
    del solver.env[rules]
    return
  if len(head) not in rules.rules: return
  arity_rules = rules.rules[len(head)]
  old = None
  index = 0
  replaced = False
  while index<len(arity_rules):
    rule = arity_rules[index]
    if match(head, rule.head):
      if not replaced:
        old = arity_rules.copy()
        rule.body = tuple(getvalue(conslist(*body), solver.env))
        index += 1
        replaced = True
      else: del arity_rules[index]
    else: index += 1
  yield cont, rules
  if old is not None:
    rules.rules[len(head)] = old
  
# replace or define the rules which the head can match with.
@builtin.macro('replace_def')
def replace_def(solver, cont, rules, head, bodies, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if isinstance(rules, Var):
    new_rules = [(head,)+tuple(body) for body in bodies]
    solver.env[rules] = klass(make_rules(new_rules), solver.env, False)
    yield cont, rules
    del solver.env.bindings[rules]
    return
  elif not isinstance(rules, klass): raise ValueError
  if len(head) not in rules.rules: 
    rules.rules[len(head)] = RuleList([Rule(head, body) for body in bodies])
    yield cont, rules
    del rules.rules[len(head)]
    return
  arity_rules = rules.rules[len(head)]
  old = arity_rules.copy()
  index = 0
  while index<len(arity_rules):
    rule = arity_rules[index]
    if match(head, rule.head): del arity_rules[index]
    else: index += 1
  for body in bodies: arity_rules.append(Rule(head,body))
  yield cont, rules
  if old is not None:
    rules.rules[len(head)] = old
    
# retract(+Term)                                                    [ISO]
#   When  Term  is an  string  or a  term  it is  unified with  the  first
#   unifying  fact or clause  in the database.   The  fact or clause  is
#   removed from the database.
@builtin.macro('retract')
def retract(solver, cont, rules, head):
  rules = getvalue(rules, solver.env)
  if len(head) not in rules.rules: return
  arity_rules = rules.rules[len(head)]
  index = 0
  old  = None
  while index<len(arity_rules):
    rule = arity_rules[index]
    caller_env = solver.env.extend()
    callee_env = caller_env.extend()
    for _ in unify_list_rule_head(head, rule.head, callee_env, caller_env, set()):
      if old is None:
        old = arity_rules.copy()
      del arity_rules[index]
      yield cont, True
      rules.rules[len(head)] = old
      return
  yield cont, True
  
# All  facts or  clauses in the  database for  which the head  unifies
#   with Head are removed.
@builtin.macro('retractall')
def retractall(solver, cont, rules, head, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if not isinstance(rules, klass): raise ValueError
  if len(head) not in rules.rules: return
  arity_rules = rules.rules[len(head)]
  index = 0
  old  = None
  while index<len(arity_rules):
    unified = False
    caller_env = solver.env.extend()
    callee_env = caller_env.extend()
    for _ in unify_list_rule_head(head, arity_rules[index].head, 
                                  callee_env, caller_env, set()):
      if not unifyied:
        unified = True
        old = arity_rules.copy()
      del arity_rules[index]
    if not unified: index += 1
  yield cont, rules
  if old is not None: rules.rules[len(head)] = old

# remove all rules which head matched with.
@builtin.macro('remove')
def remove(solver, cont, rules, head, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if not isinstance(rules, klass): raise ValueError
  if len(head) not in rules.rules: 
    yield cont, True
    return
  arity_rules = rules.rules[len(head)]
  index = 0
  old  = None
  while index<len(arity_rules):
    if match(head, arity_rules[index].head):
      if old is None: old = arity_rules.copy()
      del arity_rules[index]
    else: index += 1
  yield cont, rules
  if old is not None: rules.rules[len(head)] = old
