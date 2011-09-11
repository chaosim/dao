from oad.term import deref
from oad import builtin

# meta call predicates

@builtin.macro()
def call(solver, pred):
  for x in solver.solve(deref(pred, solver.env)):
    yield x

@builtin.macro()
def once(solver, pred):
  for x in solver.solve(deref(pred, solver.env)):
    yield x
    return
