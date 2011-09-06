from oad import error
from oad.term import Var#, conslist
##from oad.cont import Continuation, FailureContinuation
from oad import builtin
from oad.eval import solve_exps, CutException

# control predicates

##from oad.term import SUCCESS as SUCCESS

@builtin.macro()
def succeed(evaluator):
  evaluator.scont.on(SUCCESS, evaluator)
  
@builtin.macro()
def fail(evaluator):
  raise error.UnifyFail()
  
class RepeatContinuation:#(FailureContinuation):
  def __init__(self, evaluator):
    FailureContinuation.__init__(self, evaluator)
    self.fcont = evaluator.fcont
    self.undotrail = evaluator.trail
  def activate(self, evaluator): assert 0, "Unreachable"
  def fail(self, evaluator):
    evaluator.set(self.cont, self, evaluator.trail.revert_upto(self.undotrail))
  def cut(self, evaluator):
    evaluator.trail =  self.undotrail.discard(evaluator.trail)
    self.fcont.cut(evaluator)

@builtin.macro()
def repeat(evaluator):
  evaluator.set(evaluator.scont, RepeatContinuation(evaluator), evaluator.trail.branch())

@builtin.macro('!')
def cut(evaluator, *exps):
  raise CutException

cut = cut()

##from oad.cont import BodyContinuation

@builtin.macro('and')
def and_(evaluator, *exps):
  return solve_exps(evaluator, exps)

class OrContinuation:#(FailureContinuation):
  def __init__(self, altcall, evaluator, nextcont, orig_fcont):
    FailureContinuation.__init__(self, evaluator, nextcont)
    self.altcall = altcall
    self.oldStream = evaluator.stream
    self.orig_fcont, self.undotrail = orig_fcont, evaluator.trail
  def activate(self, evaluator):
    evaluator.scont = self.cont
    self.altcall.scont(evaluator)
  def cut(self, evaluator): return self.orig_fcont.cut(evaluator)
  def fail(self, evaluator):
    evaluator.stream = self.oldStream
    evaluator.set(self, self.orig_fcont,
      evaluator.trail.revert_upto(self.undotrail, discard_choicepoint=True))
  def __repr__(self): return "<OrContinuation altcall=%s" % (self.altcall, )

##from oad.cont import insert_cut_delimiter

@builtin.macro('or_')
def or_(evaluator, call1, call2):
  call1 = call1.deref(evaluator.trail)
  call2 = call2.deref(evaluator.trail)
  if call1.head.deref(evaluator.trail)!=ifp.deref(evaluator.trail): #A -> B; C
    evaluator.set(BodyContinuation(call1, evaluator), 
           OrContinuation(call2, evaluator, evaluator.scont, evaluator.fcont), evaluator.trail.branch())
  else:
    scont, fcont = insert_cut_delimiter(evaluator)
    fcont = OrContinuation(call2, evaluator, scont, fcont)
    if_clause = call1.tail.head.deref(evaluator.trail)
    then_clause = call1.tail.tail.head.deref(evaluator.trail)
    scont = BodyContinuation(then_clause, evaluator, scont)
    evaluator.set(BodyContinuation(L(and_, if_clause, [cut]), evaluator, scont), fcont, evaluator.trail.branch())

##from oad.term import conslist as L
@builtin.macro('->')  
def ifp(evaluator, if_clause, then_clause):
  # This unusual semantics is part of the ISO and all de-facto Prolog standards.
  # see SWI-Prolog help.
  if_clause = if_clause.deref(evaluator.trail)
  then_clause = then_clause.deref(evaluator.trail)
  scont = BodyContinuation(then_clause, evaluator)
  scont, fcont = insert_cut_delimiter(evaluator, scont)
  evaluator.set(BodyContinuation(L(and_, if_clause, cut), evaluator, scont), fcont)

class NotSuccessContinuation:#(Continuation):
  def __init__(self, evaluator, nextcont):
    Continuation.__init__(self, evaluator, nextcont)
    self.undotrail = evaluator.trail
  def activate(self, evaluator):
    evaluator.trail.revert_upto(self.undotrail)
    evaluator.fcont = self.cont
    raise error.UnifyFail
class NotFailureContinuation:#(FailureContinuation):
  def __init__(self, evaluator, nextcont, orig_fcont):
    FailureContinuation.__init__(self, evaluator, nextcont)
    self.undotrail = evaluator.trail
    self.orig_fcont = orig_fcont
  def activate(self, evaluator): assert 0, "Unreachable"
  def fail(self, evaluator):
    evaluator.trail.revert_upto(self.undotrail)
    evaluator.value = SUCCESS
    evaluator.set(self.cont, self.orig_fcont, self.undotrail)
@builtin.macro('not')  
def not_(evaluator, call):
  notscont = NotSuccessContinuation(evaluator, evaluator.fcont)
  notfcont = NotFailureContinuation(evaluator, evaluator.scont, evaluator.fcont)
  newscont = BodyContinuation(call.deref(evaluator.trail), evaluator, notscont)
  evaluator.set(newscont, notfcont, evaluator.trail.branch())

