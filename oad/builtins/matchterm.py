##from oad.term import atom, Atom, String, Var, Integer, SUCCESS, conslist
from oad.error import throw_type_error
from oad import builtin

##from oad.cont import BodyContinuation, DoneContinuation
##from oad.cont import Continuation, FailureContinuation

# parser predicate
# optional, any, some, times, seplist, ...

class OptionalContinuation:#(FailureContinuation):
  def __init__(self, evaluator):
    FailureContinuation.__init__(self, evaluator)
    self.undotrail = evaluator.trail
    self.oldStream = evaluator.stream
    self.orig_fcont = evaluator.fcont

  def activate(self, evaluator): raise NotImplementedError("unreachable")

  def cut(self, evaluator): 
    self.orig_fcont.cut(evaluator)

  def fail(self, evaluator):
    evaluator.trail = evaluator.trail.revert_upto(self.undotrail, discard_choicepoint=True)
    evaluator.stream = self.oldStream
    evaluator.set(self.cont, self.orig_fcont)
    evaluator.value = SUCCESS
  def __repr__(self): return "OptionalContinuation"

@builtin.macro()
def optional(evaluator, item):
  item = item.deref(evaluator.trail)
  evaluator.set(BodyContinuation(item, evaluator), 
                OptionalContinuation(evaluator),
                evaluator.trail.branch())
  evaluator.value = SUCCESS

class ParallelContinuation:#(Continuation):
  def __init__(self, leftCalls, evaluator):
    Continuation.__init__(self, evaluator)
    self.right = -1
    self.leftStream = evaluator.stream
    self.leftCalls = leftCalls
  def activate(self, evaluator):
    if self.right==-1: self.right = evaluator.stream.position
    elif self.right!=evaluator.stream.position: raise UnifyFail
    if not self.leftCalls: 
      evaluator.value = SUCCESS
      return evaluator.set(self.cont)
    evaluator.stream = self.leftStream
    call0 = self.leftCalls[0].deref(evaluator.trail)
    self.leftCalls = self.leftCalls[1:]
    evaluator.set(self)
    return call0.scont(evaluator)
  def __repr__(self): return "<ParallelContinuation %s>" % (self.leftCalls, )

@builtin.macro()
def parallel(evaluator, *calls):
  call1 = calls[0].deref(evaluator.trail)
  if len(calls)==1: return call1.scont(evaluator)
  evaluator.set(ParallelContinuation(calls[1:], evaluator))
  call1.scont(evaluator)

class RepeatMatchContinuation:#(Continuation):
  def __init__(self, item, template, evaluator, repeatDone):
    Continuation.__init__(self, evaluator)
    self.item, self.template = item, template
    repeatDone.streams = [evaluator.stream]
    repeatDone.undoTrails = [evaluator.trail]
    repeatDone.result = []
    self.repeatDone = repeatDone

  def activate(self, evaluator):
    repeatDone = self.repeatDone
    if self.template is not None: repeatDone.result.append(self.template.getvalue(evaluator.trail))
    repeatDone.streams.append(self.evaluator.stream)
    repeatDone.undoTrails.append(evaluator.trail)
    evaluator.set(BodyContinuation(self.item, evaluator, self), 
                  repeatDone, evaluator.trail.branch())

class RepeatDoneContinuation:#(FailureContinuation):
  def __init__(self, resultVar, timesVar, evaluator):
    FailureContinuation.__init__(self, evaluator)
    self.orig_fcont = evaluator.fcont
    self.timesVar, self.resultVar = timesVar, resultVar 
    self.index = None    
  def activate(self, evaluator): raise NotImplementedError("unreachable")
  def fail(self, evaluator):
    if self.index is None: self.index = len(self.streams)-1
    evaluator.stream = self.streams[self.index]
    evaluator.trail = evaluator.trail.revert_upto(self.undoTrails[self.index])
    if self.resultVar is not None:
      self.resultVar.unify(conslist(*self.result[:self.index]), evaluator.trail)
    if self.timesVar is not None: self.timesVar.unify(Integer(self.index), evaluator.trail)
    self.index -= 1
    if self.index>=0: evaluator.set(self.cont, self)
    else: evaluator.set(self.cont, self.orig_fcont) 
    evaluator.value = SUCCESS
  def cut(self, evaluator):
    self.undoTrails[0].discard(evaluator.trail)
    self.orig_fcont.cut(evaluator)
  def __repr__(self, trail=None): 
    return 'RepeatDone(%s:%s)'%(self.result, self.index)

@builtin.macro()
def any(evaluator, item, template=None, result=None):   
  item = item.deref(evaluator.trail)
  if template is not None: 
    template = template.deref(evaluator.trail)
    result = result.deref(evaluator.trail)
  repeatDone = RepeatDoneContinuation(result, None, evaluator)
  matchCont = RepeatMatchContinuation(item, template, evaluator, repeatDone)
  evaluator.set(BodyContinuation(item, evaluator, matchCont), 
                repeatDone, evaluator.trail.branch())

@builtin.macro()
def some(evaluator, item, template=None, result=None):   
  item = item.deref(evaluator.trail)
  if template is not None: 
    template = template.deref(evaluator.trail)
    result = result.deref(evaluator.trail)
  repeatDone = RepeatDoneContinuation(result, None, evaluator)
  matchCont = RepeatMatchContinuation(item, template, evaluator, repeatDone)
  evaluator.set(BodyContinuation(item, evaluator, matchCont), 
                evaluator.fcont, evaluator.trail.branch())

class TimesContinuation:#(Continuation):
  def __init__(self, item, expectTimes, template, result, evaluator):
    Continuation.__init__(self, evaluator)
    self.item, self.expectTimes = item, expectTimes
    self.template, self.result = template, result
    self.matchedTimes, self.matchedResult = 0, []

  def activate(self, evaluator):
    if self.matchedTimes>0 and self.template is not None:
      self.matchedResult.append(self.template.getvalue(evaluator.trail))
    if self.matchedTimes==self.expectTimes:
      if self.result is not None: self.result.unify(conslist(*self.matchedResult), evaluator.trail)
      return evaluator.set(self.cont)
    self.matchedTimes += 1
    evaluator.set(BodyContinuation(self.item, self.evaluator, self))    
  def __repr__(self): return 'TimesCont(%s:%s)'%(self.item, self.expectTimes)
    
@builtin.macro()
def times(evaluator, item, expectTimes, template=None, result=None):   
  item = item.deref(evaluator.trail)
  expectTimes = expectTimes.getvalue(evaluator.trail)
  if template is not None: 
    template = template.deref(evaluator.trail)
    result = result.deref(evaluator.trail)
  if expectTimes==Integer(0): 
    if result is not None: result.unify(conslist(), evaluator.trail)
    return
  if isinstance(expectTimes, Integer):
    assert expectTimes.val>=0
    scont = TimesContinuation(item, expectTimes.val, template, result, evaluator)
    evaluator.set(scont)
  else:# isinstance(expectTimes, Var):
    repeatDone = RepeatDoneContinuation(result, expectTimes, evaluator)
    matchCont = RepeatMatchContinuation(item, template, evaluator, repeatDone)
    evaluator.set(BodyContinuation(item, evaluator, matchCont), repeatDone)

from oad.builtins.control import and_
@builtin.macro()
def seplist(evaluator, item, separator, template=None, result=None):
  item = item.deref(evaluator.trail)
  separator = separator.deref(evaluator.trail)
  if template is not None: 
    template = template.deref(evaluator.trail)
    result = result.deref(evaluator.trail)
  repeatDone = RepeatDoneContinuation(result, None, evaluator)
  matchCont = RepeatMatchContinuation(conslist(and_, separator,item), 
                                      template, evaluator, repeatDone)
  evaluator.set(BodyContinuation(item, evaluator, matchCont), repeatDone)

@builtin.macro()
def xxxfollow(evaluator):
  arg0 = self.elements[0].deref(evaluator.trail)
  oldStreamPosition = evaluator.streamer.position
  evaluator.streamer.position = oldStreamPosition
  return scont, fcont, evaluator.trail

@builtin.macro()
def followby(evaluator):
  arg0 = self.elements[0].deref(evaluator.trail)
  call2 = self.elements[1].deref(evaluator.trail)
  oldStreamPosition = evaluator.streamer.position
  evaluator.call(arg0, DoneContinuation(evaluator), fcont, evaluator.trail)
  evaluator.streamer.position = oldStreamPosition
  return scont, fcont, evaluator.trail

@builtin.macro()
def notfollowby(evaluator):
  arg0 = self.elements[0].deref(evaluator.trail)
  call2 = self.elements[1].deref(evaluator.trail)
  oldStreamPosition = evaluator.streamer.position
  try: 
    evaluator.call(arg0, DoneContinuation(evaluator), fcont, evaluator.trail)
    raise UnifyFail()
  except: evaluator.streamer.position = oldStreamPosition
  return scont, fcont, evaluator.trail
