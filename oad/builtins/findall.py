from oad import builtin
from oad.term import deref, getvalue

# finding all solutions to a goal

@builtin.macro()
def findall(solver, cont, goal, template, bag):
  goal = deref(goal, solver.env)
  result = []
  for x in solver.solve(goal, cont):
    result.append(getvalue(template, solver.env))
  for x in bag.unify(result, solver.env):
    yield cont, True
    