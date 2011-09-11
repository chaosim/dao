from oad.term import Var, deref, Apply
from oad import builtin
from oad.solve import CutException

# control predicates

@builtin.macro()
def succeed(solver): yield True
succeed = succeed()

@builtin.macro()
def fail(solver):  
  if 0: yield
  return
fail = fail()

@builtin.macro()
def repeat(solver):
  while 1: yield True

@builtin.macro('!')
def cut(solver, *exps):
  yield True
  raise CutException
cut = cut()

@builtin.macro('and')
def and_(solver, *exps):
  return solve_exps(solver, exps)

@builtin.macro('or_')
def or_(solver, call1, call2):
  call1 = deref(call1, solver.env)
  call2 = deref(call2, solver.env)
  if isinstance(call1, Apply) and call1.operator==ifp: # A -> B; C
    if_clause = deref(call1.operand[0], solver.env)
    then_clause = deref(call1.operand[1], solver.env)
    call1 = if_clause&cut&then_clause
  try:
    for x in solver.solve(call1):
      yield x
  except CutException: return
  for x in solver.solve(call2):
    yield x

@builtin.macro('->')  
def ifp(solver, if_clause, then_clause):
  # This unusual semantics is part of the ISO and all de-facto Prolog standards.
  # see SWI-Prolog help.
  if_clause = deref(if_clause, solver.env)
  then_clause = deref(then_clause, solver.env)
  for x in solver.solve(if_clause):
    if not x: return
    for y in solver.solve(then_clause):
      yield y

@builtin.macro('not')  
def not_(solver, call):
  call = deref(call, solver.env)
  for x in solver.solve(call):
    return
  yield True