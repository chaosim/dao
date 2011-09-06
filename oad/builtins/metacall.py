##from oad.term import SUCCESS
from oad import builtin
##from oad.cont import Continuation, insert_cut_delimiter

# meta call predicates

@builtin.macro()
def call(evaluator, pred):
  pred = pred.deref(evaluator.trail)
  insert_cut_delimiter(evaluator)
  pred.scont(evaluator)

class OnceContinuation:#(Continuation):
  def __init__(self, evaluator):
    Continuation.__init__(self, evaluator)
    self.fcont = evaluator.fcont
  def activate(self, evaluator): 
    evaluator.set(self.cont, self.fcont)
    evaluator.value = SUCCESS

@builtin.macro()
def once(evaluator, pred):
  pred.deref(evaluator.trail).scont(evaluator.set(OnceContinuation(evaluator)))
