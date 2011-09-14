from oad.term import deref
from oad.error import throw_type_error
from oad import builtin
from oad.solve import mycont

# parser predicate
# optional, any, some, times, seplist, ...

@builtin.macro()
def optional(solver, cont, item):
  item = deref(item, solver.env)
  stream = solver.stream
  yield solver.cont(item, cont), True
  solver.stream = stream
  yield cont, True

@builtin.macro()
def parallel(solver, cont, *calls):
  call0 = deref(calls[0], solver.env)
  if len(calls)==1: 
    yield solver.cont(call0, cont), True
    return
  stream = solver.stream
  @mycont(cont)
  def pallel_cont(value, solver):
    if pallel_cont.right==-1: pallel_cont.right = solver.stream[1]
    elif pallel_cont.right!=solver.stream[1]: return
    if len(pallel_cont.calls)==0: 
      yield cont, True
      return
    solver.stream = stream
    call0 = pallel_cont.calls[0]
    pallel_cont.calls =pallel_cont.calls[1:]
    yield solver.cont(call0, pallel_cont), True
  pallel_cont.right = -1
  pallel_cont.calls = calls[1:]
  yield solver.cont(call0, pallel_cont), True

class RepeatMatchContinuation:#(Continuation):
  def __init__(self, item, template, solver, repeatDone):
    Continuation.__init__(self, solver)
    self.item, self.template = item, template
    repeatDone.streams = [solver.stream]
    repeatDone.undoTrails = [solver.env]
    repeatDone.result = []
    self.repeatDone = repeatDone

  def activate(self, solver):
    repeatDone = self.repeatDone
    if self.template is not None: repeatDone.result.append(self.template.getvalue(solver.env))
    repeatDone.streams.append(self.solver.stream)
    repeatDone.undoTrails.append(solver.env)
    solver.set(BodyContinuation(self.item, solver, self), 
                  repeatDone, solver.env.branch())

class RepeatDoneContinuation:#(FailureContinuation):
  def __init__(self, resultVar, timesVar, solver):
    FailureContinuation.__init__(self, solver)
    self.orig_fcont = solver.fcont
    self.timesVar, self.resultVar = timesVar, resultVar 
    self.index = None    
  def activate(self, solver): raise NotImplementedError("unreachable")
  def fail(self, solver):
    if self.index is None: self.index = len(self.streams)-1
    solver.stream = self.streams[self.index]
    solver.env = solver.env.revert_upto(self.undoTrails[self.index])
    if self.resultVar is not None:
      self.resultVar.unify(conslist(*self.result[:self.index]), solver.env)
    if self.timesVar is not None: self.timesVar.unify(Integer(self.index), solver.env)
    self.index -= 1
    if self.index>=0: solver.set(self.cont, self)
    else: solver.set(self.cont, self.orig_fcont) 
    solver.value = True
  def cut(self, solver):
    self.undoTrails[0].discard(solver.env)
    self.orig_fcont.cut(solver)
  def __repr__(self, trail=None): 
    return 'RepeatDone(%s:%s)'%(self.result, self.index)

@builtin.macro()
def any(solver, item, template=None, result=None):   
  item = item.deref(solver.env)
  if template is not None: 
    template = template.deref(solver.env)
    result = result.deref(solver.env)
  repeatDone = RepeatDoneContinuation(result, None, solver)
  matchCont = RepeatMatchContinuation(item, template, solver, repeatDone)
  solver.set(BodyContinuation(item, solver, matchCont), 
                repeatDone, solver.env.branch())

@builtin.macro()
def some(solver, item, template=None, result=None):   
  item = item.deref(solver.env)
  if template is not None: 
    template = template.deref(solver.env)
    result = result.deref(solver.env)
  repeatDone = RepeatDoneContinuation(result, None, solver)
  matchCont = RepeatMatchContinuation(item, template, solver, repeatDone)
  solver.set(BodyContinuation(item, solver, matchCont), 
                solver.fcont, solver.env.branch())

class TimesContinuation:#(Continuation):
  def __init__(self, item, expectTimes, template, result, solver):
    Continuation.__init__(self, solver)
    self.item, self.expectTimes = item, expectTimes
    self.template, self.result = template, result
    self.matchedTimes, self.matchedResult = 0, []

  def activate(self, solver):
    if self.matchedTimes>0 and self.template is not None:
      self.matchedResult.append(self.template.getvalue(solver.env))
    if self.matchedTimes==self.expectTimes:
      if self.result is not None: self.result.unify(conslist(*self.matchedResult), solver.env)
      return solver.set(self.cont)
    self.matchedTimes += 1
    solver.set(BodyContinuation(self.item, self.solver, self))    
  def __repr__(self): return 'TimesCont(%s:%s)'%(self.item, self.expectTimes)
    
@builtin.macro()
def times(solver, item, expectTimes, template=None, result=None):   
  item = item.deref(solver.env)
  expectTimes = expectTimes.getvalue(solver.env)
  if template is not None: 
    template = template.deref(solver.env)
    result = result.deref(solver.env)
  if expectTimes==Integer(0): 
    if result is not None: result.unify(conslist(), solver.env)
    return
  if isinstance(expectTimes, Integer):
    assert expectTimes.val>=0
    scont = TimesContinuation(item, expectTimes.val, template, result, solver)
    solver.set(scont)
  else:# isinstance(expectTimes, Var):
    repeatDone = RepeatDoneContinuation(result, expectTimes, solver)
    matchCont = RepeatMatchContinuation(item, template, solver, repeatDone)
    solver.set(BodyContinuation(item, solver, matchCont), repeatDone)

from oad.builtins.control import and_
@builtin.macro()
def seplist(solver, item, separator, template=None, result=None):
  item = item.deref(solver.env)
  separator = separator.deref(solver.env)
  if template is not None: 
    template = template.deref(solver.env)
    result = result.deref(solver.env)
  repeatDone = RepeatDoneContinuation(result, None, solver)
  matchCont = RepeatMatchContinuation(conslist(and_, separator,item), 
                                      template, solver, repeatDone)
  solver.set(BodyContinuation(item, solver, matchCont), repeatDone)

@builtin.macro()
def xxxfollow(solver):
  arg0 = self.elements[0].deref(solver.env)
  oldStreamPosition = solver.streamer.position
  solver.streamer.position = oldStreamPosition
  return scont, fcont, solver.env

@builtin.macro()
def followby(solver):
  arg0 = self.elements[0].deref(solver.env)
  call2 = self.elements[1].deref(solver.env)
  oldStreamPosition = solver.streamer.position
  solver.call(arg0, DoneContinuation(solver), fcont, solver.env)
  solver.streamer.position = oldStreamPosition
  return scont, fcont, solver.env

@builtin.macro()
def notfollowby(solver):
  arg0 = self.elements[0].deref(solver.env)
  call2 = self.elements[1].deref(solver.env)
  oldStreamPosition = solver.streamer.position
  try: 
    solver.call(arg0, DoneContinuation(solver), fcont, solver.env)
    raise UnifyFail()
  except: solver.streamer.position = oldStreamPosition
  return scont, fcont, solver.env
