from oad.error import UnifyFail
##from oad.term import atom, SUCCESS, Var, Integer, conslist
##from oad.cont import Continuation, BodyContinuation
##from oad.cont import ChoiceContinuation, FailureContinuation
from oad import builtin
from oad import helper

# finding all solutions to a goal

class FindallCollectContinuation:#(Continuation):
  def __init__(self, template):
    Continuation.__init__(self, None, None)
    self.template, self.result = template, []

  def activate(self, evaluator):
    self.result.append(self.template.getvalue(evaluator.trail))
    raise UnifyFail()

class FindallDoneContinuation:#(FailureContinuation):
  def __init__(self, bag, collector, evaluator):
    Continuation.__init__(self, evaluator)
    self.collector = collector
    self.orig_fcont = evaluator.fcont
    self.undotrail = evaluator.trail
    self.bag = bag
  def activate(self, evaluator): raise NotImplementedError
  def fail(self, evaluator):
    trail = evaluator.trail.revert_upto(self.undotrail)
    self.bag.unify(conslist(*self.collector.result), trail)
    evaluator.set(self.cont, self.orig_fcont, trail)

@builtin.macro()
def findall(evaluator, goal, template, bag):
  goal = goal.deref(evaluator.trail)
  template = template.deref(evaluator.trail)
  newtrail = evaluator.trail.branch()
  collector = FindallCollectContinuation(template)
  newscont = BodyContinuation(goal, evaluator, collector)
  fcont = FindallDoneContinuation(bag, collector, evaluator)
  evaluator.set(newscont, fcont, newtrail)
