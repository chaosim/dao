from dao.term import deref, unify_list_rule_head, conslist, getvalue, match, Var
from dao.term import rule_head_signatures
from dao import builtin
from dao.rule import Rule, RuleList
from dao.special import UserFunction, UserMacro, make_rules, begin

# rule manipulation

@builtin.macro()
def abolish(solver, cont, rules, arity):
  rules = getvalue(rules, solver.cont)
  if not isinstance(rules, UserFunction) and not isinstance(rules, UserMacro):
    raise ValueError(rules)
    #yield cont, rules
  arity = deref(arity, solver.cont)
  if arity not in rules.arity2rules:
    yield cont, rules.rules
    return
  old_arity2rules = rules.arity2rules[arity]
  old_signature2rules = rules.signature2rules[arity]
  del rules.arity2rules[arity]
  del rules.signature2rules[arity]
  yield cont, rules.arity2rules
  rules.arity2rules[arity] = old_arity2rules
  rules.signature2rules[arity] = old_signature2rules

@builtin.macro('assert')
def assert_(solver, cont, rules, head, body, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if not isinstance(rules, klass): raise ValueError(rules)
  arity = len(head)
  arity_rules = rules.arity2rules.setdefault(arity, [])
  index = len(arity_rules)
  arity_rules.append(Rule(head, begin(*body)))
  for signature in rule_head_signatures(head):
    arity2signature = rules.signature2rules.setdefault(arity, {})
    arity2signature.setdefault(signature, set()).add(index)
  yield cont, arity_rules
  if index==0: 
    del rules.arity2rules[arity]
    del rules.signature2rules[arity]
  else:
    del arity_rules[-1]
    for signature in rule_head_signatures(head):
      arity2signature[signature].remove(index)
    if arity2signature[signature]==set(): del arity2signature[signature]

@builtin.macro('asserta')
def asserta(solver, cont, rules, head, body, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if not isinstance(rules, klass): raise ValueError(rules)
  arity = len(head)
  arity_rules = rules.arity2rules.setdefault(arity, [])
  arity_signature = rules.signature2rules.setdefault(arity, {})
  arity_rules.insert(0, Rule(head, begin(*body)))
  for sign in arity_signature:
    arity_signature[sign] = set([i+1 for i in arity_signature[sign]])
  for signature in rule_head_signatures(head):
    arity_signature.setdefault(signature, set()).add(0)
  yield cont, rules
  del arity_rules[0]
  if len(arity_rules)==1: 
    del rules.arity2rules[arity]
    del rules.signature2rules[arity]
  else:
    del arity_rules[0]
    for sign in arity_signature:
      arity_signature[sign] = set([i-1 for i in arity_signature[sign] if i!=0])
      if arity_signature[sign]==set(): del arity_signature[sign]

@builtin.macro('append_def')
def append_def(solver, cont, rules, head, bodies, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if not isinstance(rules, klass): raise ValueError(rules)
  arity = len(head)
  arity_rules = rules.arity2rules.setdefault(arity, [])
  arity2signature = rules.signature2rules.setdefault(arity, {})
  index = length = len(arity_rules)
  arity_rules.rules[arity] += [Rule(head, begin(*body)) for body in bodies]
  new_indexes = set(range(length, length+len(bodies)))
  for signature in rule_head_signatures(head):
    indexes = arity2signature.setdefault(signature, set()) 
    indexes |= new_indexes
  yield cont, arity_rules
  if length==0: 
    del rules.arity2rules[arity]
    del rules.signature2rules[arity]
  else:
    del arity_rules[length:]
    for signature in rule_head_signatures(head):
      arity2signature[signature] -= new_indexes
      if arity2signature[signature]==set(): del arity2signature[signature]

@builtin.macro('asserta')
def insert_def(solver, cont, rules, head, bodies, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if not isinstance(rules, klass): raise ValueError(rules)
  arity = len(head)
  arity2signature = rules.signature2rules.setdefault(arity, {})
  length = len(rules.arity2rules[arity])
  rules.arity2rules[arity] = [Rule(head, begin(*body))]+rules.arity2rules[arity]
  bodies_length = len(bodies)
  new_indexes = set(range(bodies_length))
  for signature in rule_head_signatures(head):
    indexes = arity2signature.setdefault(signature, set())
    indexes |= new_indexes
  yield cont, arity_rules
  if length==0: 
    del rules.arity2rules[arity]
    del rules.signature2rules[arity]
  else:
    del arity_rules[:bodies_length]
    for signature in rule_head_signatures(head):
      arity2signature[signature] -= new_indexes
      if arity2signature[signature]==set(): del arity2signature[signature]

def deepcopy(d):
  result = {}
  for k, v in d.items():
    result[k] = v.copy()
  return result

# replace the rules which the head can match with.
@builtin.macro('replace')
def replace(solver, cont, rules, head, body, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if not isinstance(rules, klass): raise ValueError(rules)
  arity = len(head)
  arity_rules = rules.arity2rules.setdefault(arity, [])
  old_arity_rules = None
  arity_signatures = rules.signature2rules.setdefault(arity, {})
  del_indexes = []
  index = 0
  while index<len(arity_rules):
    rule = arity_rules[index]
    if match(head, rule.head):
      if old_arity_rules is None:
        old_arity_rules = arity_rules[:]
        old_arity_signatures = deepcopy(arity_signatures)
        arity_rules[index] = Rule(head, begin(*body))
        for signature in rule_head_signatures(rule.head):
          arity_signatures[signature].remove(index)
        for signature in rule_head_signatures(head):
          arity_signatures.setdefault(signature, set()).add(index)
        index += 1
      else: 
        del arity_rules[index]
        del_indexes.append(index)
    else: index += 1 
  if old_arity_rules is not None:
    delta = 0
    modify_dict = {}
    for i in range(index):
      if i in del_indexes: delta += 1
      else: modify_dict[i] = i-delta
    for sign in arity_signatures:
      arity_signatures[sign] = set([modify_dict[i] for i in arity_signatures[sign] 
                                   if i not in del_indexes])
    yield cont, arity_rules
    # backtracking
    rules.arity2rules[arity] = old_arity_rules
    rules.arity2signatures[arity] = old_arity_signatures
  else: yield cont, arity_rules

# replace or define the rules which the head can match with.
@builtin.macro('replace_def')
def replace_def(solver, cont, rules, head, bodies, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if isinstance(rules, Var):
    new_rules = [(head,)+tuple(body) for body in bodies]
    arity2rules, signature2rules = make_rules(new_rules)
    solver.env[rules] = klass(arity2rules, signature2rules, solver.env, False)
    yield cont, rules
    del solver.env.bindings[rules]
    return
  
  if not isinstance(rules, klass): raise ValueError
  
  arity = len(head)
  new_indexes = set(range(len(bodies)))
  if arity not in rules.arity2rules: 
    rules.arity2rules[arity] = [Rule(head, begin(*body)) for body in bodies]
    rules.signature2rules[arity] = {}
    for signature in rule_head_signatures(head):
      rules.signature2rules[arity][signature] = new_indexes
    yield cont, rules.arity2rules[arity]
    del rules.arity2rules[arity]
    del rules.signature2rules[arity]
    return
  
  arity_rules = rules.arity2rules[arity]
  old_arity_rules = None
  index = 0
  arity_signatures = rules.signature2rules[arity]
  del_indexes = []
  while index<len(arity_rules):
    rule = arity_rules[index]
    if match(head, rule.head):
      if old_arity_rules is None:
        old_arity_rules = arity_rules[:]
        old_arity_signatures = deepcopy(arity_signatures)
        new_indexes_start = index
        new_indexes = set(range(index, index+len(bodies)))
        del arity_rules[index]
        for signature in rule_head_signatures(rule.head):
          arity_signatures[signature].remove(index)
        for body in bodies:
          arity_rules.insert(index, Rule(head, begin(*body)))
        new_indexes_map = {}
        for signature in rule_head_signatures(head):
          new_indexes_map[signature] = new_indexes
        index += len(bodies)
      else: 
        del arity_rules[index]
        del_indexes.append(index)
    else: index += 1 
  if old_arity_rules is not None:
    delta = 0
    modify_dict = {}
    i = 0
    delta = 0
    while i < index:
      if i in del_indexes: delta -= 1
      elif i==new_indexes_start: delta += len(bodies)-1 
      else: modify_dict[i] = i+delta
      i += 1
    for sign in arity_signatures:
      arity_signatures[sign] = set([modify_dict[i] for i in arity_signatures[sign] 
                                   if i not in del_indexes])
      arity_signatures[sign] |= new_indexes_map.get(sign, set())
    yield cont, arity_rules
    # backtracking
    rules.arity2rules[arity] = old_arity_rules
    rules.arity2signatures[arity] = old_arity_signatures
  else: yield cont, arity_rules
      
# retract(+Term)                                                    [ISO]
#   When  Term  is an  string  or a  term  it is  unified with  the  first
#   unifying  fact or clause  in the database.   The  fact or clause  is
#   removed from the database.
@builtin.macro('retract')
def retract(solver, cont, rules, head):
  rules = getvalue(rules, solver.env)
  if not isinstance(rules, klass): raise ValueError(rules)
  arity = len(head)
  if arity not in rules.arity_rules: 
    yield cont, rules.arity_rules[arity]
    return
  arity_rules = rules.arity_rules[arity]
  index = 0
  while index<len(arity_rules):
    rule = arity_rules[index]
    caller_env = solver.env.extend()
    callee_env = caller_env.extend()
    for _ in unify_list_rule_head(head, rule.head, callee_env, caller_env, set()):
      rule = arity_rules[index]
      del arity_rules[index]
      arity_signature2rules = rules.signature2rules[arity]
      for signature in rule_head_signatures(rule.head):
        arity_signature2rules[signature].remove(index)
      yield cont, arity_rules
      arity_rules.insert(index, rule)
      for signature in rule_head_signatures(rule.head):
        arity_signature2rules[signature].add(index)
      return
  yield cont, True
  
# All  rules for  which head  unifies with head are removed.
@builtin.macro('retractall')
def retractall(solver, cont, rules, head, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if not isinstance(rules, klass): raise ValueError
  arity = len(head)
  if arity not in rules.arity_rules: 
    yield cont, rules.arity_rules[arity]
    return
  arity_signature2rules = rules.signature2rules[arity]
  arity_rules = rules.arity_rules[arity]
  old_arity_rules = arity_rules[:]
  del_indexes = {}
  index = 0
  changed = False
  while index<len(arity_rules):
    caller_env = solver.env.extend()
    callee_env = caller_env.extend()
    unified = False
    for _ in unify_list_rule_head(head, rule.head, callee_env, caller_env, set()):
      unified = True
      changed = True
      rule = arity_rules[index]
      del arity_rules[index]
      for signature in rule_head_signatures(rule.head):
        arity_signature2rules[signature].remove(index)
        del_indexes.setdefault(signature, set()).add(index)
      del_indexes.append(index)
    if not unified: index += 1  
  yield cont, arity_rules
  if not changed:  return
  rules.signature2rules[arity] = old_arity_rules
  for signature, indexes in del_indexes.items():
    arity_signature2rules[signature] |= indexes

# remove all rules which head matched with.
@builtin.macro('remove')
def remove(solver, cont, rules, head, klass=UserFunction):
  rules = getvalue(rules, solver.env)
  if not isinstance(rules, klass): raise ValueError
  arity = len(head)
  if arity not in rules.arity_rules: 
    yield cont, rules.arity_rules[arity]
    return
  arity_signature2rules = rules.signature2rules[arity]
  arity_rules = rules.arity_rules[arity]
  old_arity_rules = arity_rules[:]
  del_indexes = {}
  index = 0
  changed = False
  while index<len(arity_rules):
    if match(head, arity_rules[index].head):
      changed = True
      rule = arity_rules[index]
      del arity_rules[index]
      for signature in rule_head_signatures(rule.head):
        arity_signature2rules[signature].remove(index)
        del_indexes.setdefault(signature, set()).add(index)
      del_indexes.append(index)
    if not unified: index += 1  
  yield cont, arity_rules
  if not changed:  return
  rules.signature2rules[arity] = old_arity_rules
  for signature, indexes in del_indexes.items():
    arity_signature2rules[signature] |= indexes