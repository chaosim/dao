from oad.term import Var, deref, Apply
from oad import builtin
from oad.eval import solve_exps, CutException

# control predicates

@builtin.macro()
def succeed(evaluator): yield True
succeed = succeed()

@builtin.macro()
def fail(evaluator):  
  if 0: yield
  return
fail = fail()

@builtin.macro()
def repeat(evaluator):
  while 1: yield True

@builtin.macro('!')
def cut(evaluator, *exps):
  yield True
  raise CutException
cut = cut()

@builtin.macro('and')
def and_(evaluator, *exps):
  return solve_exps(evaluator, exps)

@builtin.macro('or_')
def or_(evaluator, call1, call2):
  call1 = deref(call1, evaluator.env)
  call2 = deref(call2, evaluator.env)
  if isinstance(call1, Apply) and call1.operator==ifp: # A -> B; C
    if_clause = deref(call1.operand[0], evaluator.env)
    then_clause = deref(call1.operand[1], evaluator.env)
    call1 = if_clause&cut&then_clause
  try:
    for x in evaluator.solve(call1):
      yield x
  except CutException: return
  for x in evaluator.solve(call2):
    yield x

@builtin.macro('->')  
def ifp(evaluator, if_clause, then_clause):
  # This unusual semantics is part of the ISO and all de-facto Prolog standards.
  # see SWI-Prolog help.
  if_clause = deref(if_clause, evaluator.env)
  then_clause = deref(then_clause, evaluator.env)
  for x in evaluator.solve(if_clause):
    if not x: return
    for y in evaluator.solve(then_clause):
      yield y

@builtin.macro('not')  
def not_(evaluator, call):
  call = deref(call, evaluator.env)
  for x in evaluator.solve(call):
    return
  yield True