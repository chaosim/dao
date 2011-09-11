from oad import builtin
from oad.term import deref, getvalue

# finding all solutions to a goal

@builtin.macro()
def findall(solver, goal, template, bag):
  goal = deref(goal, solver.env)
  result = []
  for x in solver.solve(goal):
    result.append(getvalue(template, solver.env))
  for x in bag.unify(result, solver.env):
    yield True  