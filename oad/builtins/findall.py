from oad import builtin
from oad.term import deref, getvalue

# finding all solutions to a goal

@builtin.macro()
def findall(evaluator, goal, template, bag):
  goal = deref(goal, evaluator.env)
  result = []
  for x in evaluator.solve(goal):
    result.append(getvalue(template, evaluator.env))
  for x in bag.unify(result, evaluator.env):
    yield True  