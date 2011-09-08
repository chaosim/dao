from oad.term import deref
from oad import builtin

# meta call predicates

@builtin.macro()
def call(evaluator, pred):
  for x in evaluator.solve(deref(pred, evaluator.env)):
    yield x

@builtin.macro()
def once(evaluator, pred):
  for x in evaluator.solve(deref(pred, evaluator.env)):
    yield x
    return
