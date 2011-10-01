from oad.term import Var, deref, Apply
from oad import builtin
from oad.solve import CutException

# control predicates

@builtin.macro()
def succeed(solver, cont): yield cont, True
succeed = succeed()

@builtin.macro()
def fail(solver, cont):  
  if 0: yield cont, True
  return
fail = fail()

@builtin.macro()
def repeat(solver, cont):
  def repeat_cont(value, solver):
    while 1: 
      yield cont, True
##  repeat_cont.cut = True
  yield repeat_cont, True

repeat = repeat()
  
@builtin.macro('!')
def cut(solver, cont):
  yield cont, True
  raise CutException
cut = cut()

@builtin.macro('and')
def and_(solver, cont, *calls):
  if len(calls)==1:
    call = deref(calls[0], solver.env)
    yield solver.cont(call, cont), True
  elif len(calls)==2:
    call1 = deref(calls[0], solver.env)
    call2 = deref(calls[1], solver.env)
    def and_cont(value, solver): yield solver.cont(call2, cont), value
    yield solver.cont(call1, and_cont), True
  else: 
    call1 = deref(calls[:-1], solver.env)
    call2 = deref(calls[-1], solver.env)
    def and_cont(value, solver): yield solver.cont(call2, cont), value
    yield solver.cont(and_(*call1), and_cont), True
@builtin.macro('or_')
def or_(solver, cont, call1, call2):
  call1 = deref(call1, solver.env)
  call2 = deref(call2, solver.env)
  if isinstance(call1, Apply) and call1.operator==ifp: # A -> B; C
    if_clause = deref(call1.operand[0], solver.env)
    then_clause = deref(call1.operand[1], solver.env)
    call1 = if_clause&cut&then_clause
  def or_cont(value, solver):
    stream = solver.stream
    yield solver.cont(call1, cont), True
    solver.stream = stream
    yield solver.cont(call2, cont), True
    solver.stream = stream
##  or_cont.cut = True
  yield or_cont, True

@builtin.macro('->')  
def ifp(solver, cont, if_clause, then_clause):
  # This unusual semantics is part of the ISO and all de-facto Prolog standards.
  # see SWI-Prolog help.
  if_clause = deref(if_clause, solver.env)
  then_clause = deref(then_clause, solver.env)
  def ifp_cont(value, solver):
    if not value: return
    yield solver.cont(then_clause, cont), True
  yield solver.cont(if_clause, ifp_cont), True

@builtin.macro('not')  
def not_(solver, cont, call):
  call = deref(call, solver.env)
  for x in solver.solve(call, cont): return
  yield cont, True