from oad.term import deref
from oad import builtin

# meta call predicates

@builtin.macro()
def call(solver, cont, pred): yield solver.cont(deref(pred, solver.env), cont), True

@builtin.macro()
def once(solver, cont, pred):
  for x in solver.solve(deref(pred, solver.env), cont):
    yield cont, x
    return
