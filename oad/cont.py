from dao.error import UnifyFail
from dao.term import Cons, UEntity

class Continuation(UEntity):
  def __init__(self, *arguments):
    if len(arguments)==2: 
      self.evaluator, self.cont = arguments
      if self.cont is None and self.evaluator is not None: 
        self.cont = self.evaluator.scont
    elif isinstance(arguments[0], Continuation): self.cont = arguments[0]
    else: 
      self.evaluator = arguments[0]
      self.cont = self.evaluator.scont
    if self.cont is not None: self._candiscard = self.cont.candiscard()
    else: self._candiscard = False    
    self.done = False
  def candiscard(self): return self._candiscard
  def discard(self):
    if self.cont is not None: self.cont.discard()
  def on(self, value, evaluator): 
    evaluator.value = value
    evaluator.scont = self
  def __call__(self, values, evaluator):
    assert len(values)==1
    self.on(values[0], evaluator)
  def lookup(self, tag, cont, evaluator): 
    self.cont.lookup(tag, cont, evaluator)
  def unwind(self, cont, evaluator):
    if cont is self: evaluator.scont = self
    else: self.cont.unwind(cont, evaluator)
  def activate(self, evaluator): evaluator.scont = self
  def __eq__(self, other): return self is other

class FailureContinuation(Continuation):
  def fail(self, trail): raise NotImplementedError("abstract base class")
  def cut(self, evaluator): evaluator.fcont = self

class DoneContinuation(FailureContinuation):
  def __init__(self, evaluator=None):
    FailureContinuation.__init__(self, evaluator, None)
    self.failed = False
    self.done = True
  def on(self, value, evaluator): 
    FailureContinuation.on(self, value, evaluator)
    self.done = True
  def activate(self, evaluator): assert 0, 'unreachable'
  def fail(self, evaluator): raise UnifyFail
  def __repr__(self, trail=None): return 'done'

done = DoneContinuation()

class ChoiceContinuation(FailureContinuation):
  def __init__(self, *arguments):
    FailureContinuation.__init__(self, *arguments)
    self.orig_fcont, self.undotrail = None, None    
  def prepare_more_solutions(self, evaluator):
    self.orig_fcont, self.undotrail = evaluator.fcont, evaluator.trail
    self.oldStream = evaluator.stream
    evaluator.fcont = self
    evaluator.trail = evaluator.trail.branch()
    self.env_bindings = evaluator.env.bindings.copy()
  def fail(self, evaluator):
    assert self.undotrail is not None
    evaluator.scont = self
    evaluator.fcont = self.orig_fcont
    evaluator.trail = evaluator.trail.revert_upto(self.undotrail, discard_choicepoint=True)
    evaluator.stream = self.oldStream
    evaluator.env.bindings = self.env_bindings
  def cut(self, evaluator):
    evaluator.trail = self.undotrail.discard(evaluator.trail)
    self.orig_fcont.cut(evaluator)
  def discard(self): pass

class RuleSetContinuation(ChoiceContinuation):
  def __init__(self, pattern, rules, env, recursive, evaluator, nextcont):
    ChoiceContinuation.__init__(self, evaluator, nextcont)
    self.pattern, self.rules, self.env = pattern, rules, env
    self.recursive = recursive

  def activate(self, evaluator):
    rules = self.rules
    evaluator.scont = RuleContinuation(self.pattern, rules[0], self.recursive, 
                        self.env, self.evaluator, self.cont)
    rules = rules.find_applicable_rule()
    if rules is not None:
      self.prepare_more_solutions(evaluator)
      self.rules = rules    
  def __repr__(self, trail=None): 
    return "UserCon: %s@ %s"%(self.pattern, self.rules)

class RuleContinuation(Continuation):
  def __init__(self, pattern, rule, recursive, env, evaluator, nextcont):
    Continuation.__init__(self, evaluator, nextcont)
    self.pattern,self.rule, self.env = pattern, rule, env
    self.recursive = recursive
    
  def activate(self, evaluator):
    evaluator.scont = self.cont
    callerEnv = evaluator.env
    if not self.recursive: evaluator.env = self.env.extend()
    else: 
      self.env.bindings = {}
      evaluator.env = self.env
    nextcall = self.rule.apply(evaluator.trail, self.pattern)
    evaluator.set(RuleDoneContinuation(evaluator, callerEnv))
    return Cons('begin', nextcall).scont(self.evaluator)
  def __repr__(self): 
    return "RuleCont: %s @ %r" % (self.pattern, self.rule)

class RuleDoneContinuation(Continuation):
  def __init__(self, evaluator, callerEnv):
    Continuation.__init__(self, evaluator)
    self.callerEnv = callerEnv
  def activate(self, evaluator):
    evaluator.env = self.callerEnv
    evaluator.set(self.cont)
  def __repr__(self): return "RuleDoneCont"
    
class CutScopeNotifier(Continuation):
  def __init__(self, evaluator, nextcont):
    Continuation.__init__(self, evaluator, nextcont)
    self.cutcell = CutCell()
  def candiscard(self): return not self.cutcell.discarded
  def activate(self, evaluator):
    self.cutcell.activated = True
    evaluator.set(self.cont)

  def discard(self):
    assert not self.cutcell.activated
    self.cutcell.discarded = True
    
  def __repr__(self): return 'CutScopeNotifier'

class CutCell(object):
  def __init__(self):
    self.activated = False
    self.discarded = False

class CutDelimiter(FailureContinuation):
  def __init__(self, evaluator, nextcont, cutcell):
    FailureContinuation.__init__(self, evaluator, nextcont)
    self.cutcell = cutcell
  def candiscard(self): return not self.cutcell.discarded
  def activate(self, evaluator): raise NotImplementedError("unreachable")
  def fail(self, evaluator): 
    self.cont.fail(evaluator)
  def cut(self, evaluator):
    if not self.cutcell.activated: evaluator.fcont = self
    else: self.cont.cut(evaluator)
  def __repr__(self, trail=None): 
    return "CutDelimiter: %r, %r"%(self.cutcell.activated, self.cutcell.discarded)

def insert_cut_delimiter(evaluator, scont=None, fcont=None):
  if scont is None: scont = evaluator.scont
  if fcont is None: fcont = evaluator.fcont
  if isinstance(fcont, CutDelimiter):
    if fcont.cutcell.activated or fcont.cutcell.discarded:
      fcont = fcont.cont
      if isinstance(scont, CutScopeNotifier) and scont.cutcell.discarded:
        scont = scont.cont
    elif (isinstance(scont, CutScopeNotifier) and scont.cutcell is fcont.cutcell):
      assert not fcont.cutcell.activated
      return scont, fcont
  scont = CutScopeNotifier(evaluator, scont)
  fcont = CutDelimiter(evaluator, fcont, scont.cutcell)
  return scont, fcont

class CatchingDelimiter(Continuation):
  def __init__(self, catcher, recover, evaluator):
    Continuation.__init__(self, evaluator)
    self.catcher, self.recover = catcher, recover
    self.fcont, self.trail = evaluator.fcont, evaluator.trail
  def activate(self, evaluator): evaluator.set(self.cont, self.fcont, self.trail)
  def __repr__(self): return "(CatchingDelimiter: %s, %s)"%(self.catcher, self.recover)

class BodyContinuation(Continuation):
  def __init__(self, body, evaluator, nextcont=None):
    Continuation.__init__(self, evaluator, nextcont)
    self.body = body
  def activate(self, evaluator):
    evaluator.scont = self.cont
    self.body.scont(evaluator)
  def __repr__(self): return "BodyCont: %s" %self.body

class FunctionContinuation(Continuation):
  def __init__(self, function, *arguments): 
    Continuation.__init__(self, arguments[-1])
    self.arguments = arguments[:-1]
    self.function = function
  def activate(self, evaluator):
    self.function(self, evaluator.value, evaluator)
  def __repr__(self): return "%s%s"%(self.function.__name__,self.arguments[:-2])
  __str__ = __repr__

class FunctionContinuationConstructor:
  def __init__(self, function):
    self.function = function
  def __call__(self, *arguments):
    return FunctionContinuation(self.function, *arguments)

funcont = FunctionContinuationConstructor