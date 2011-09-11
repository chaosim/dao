from dao.error import UnifyFail
from dao.term import Cons, UEntity

class Continuation(UEntity):
  def __init__(self, *arguments):
    if len(arguments)==2: 
      self.solver, self.cont = arguments
      if self.cont is None and self.solver is not None: 
        self.cont = self.solver.scont
    elif isinstance(arguments[0], Continuation): self.cont = arguments[0]
    else: 
      self.solver = arguments[0]
      self.cont = self.solver.scont
    if self.cont is not None: self._candiscard = self.cont.candiscard()
    else: self._candiscard = False    
    self.done = False
  def candiscard(self): return self._candiscard
  def discard(self):
    if self.cont is not None: self.cont.discard()
  def on(self, value, solver): 
    solver.value = value
    solver.scont = self
  def __call__(self, values, solver):
    assert len(values)==1
    self.on(values[0], solver)
  def lookup(self, tag, cont, solver): 
    self.cont.lookup(tag, cont, solver)
  def unwind(self, cont, solver):
    if cont is self: solver.scont = self
    else: self.cont.unwind(cont, solver)
  def activate(self, solver): solver.scont = self
  def __eq__(self, other): return self is other

class FailureContinuation(Continuation):
  def fail(self, trail): raise NotImplementedError("abstract base class")
  def cut(self, solver): solver.fcont = self

class DoneContinuation(FailureContinuation):
  def __init__(self, solver=None):
    FailureContinuation.__init__(self, solver, None)
    self.failed = False
    self.done = True
  def on(self, value, solver): 
    FailureContinuation.on(self, value, solver)
    self.done = True
  def activate(self, solver): assert 0, 'unreachable'
  def fail(self, solver): raise UnifyFail
  def __repr__(self, trail=None): return 'done'

done = DoneContinuation()

class ChoiceContinuation(FailureContinuation):
  def __init__(self, *arguments):
    FailureContinuation.__init__(self, *arguments)
    self.orig_fcont, self.undotrail = None, None    
  def prepare_more_solutions(self, solver):
    self.orig_fcont, self.undotrail = solver.fcont, solver.env
    self.oldStream = solver.stream
    solver.fcont = self
    solver.env = solver.env.branch()
    self.env_bindings = solver.env.bindings.copy()
  def fail(self, solver):
    assert self.undotrail is not None
    solver.scont = self
    solver.fcont = self.orig_fcont
    solver.env = solver.env.revert_upto(self.undotrail, discard_choicepoint=True)
    solver.stream = self.oldStream
    solver.env.bindings = self.env_bindings
  def cut(self, solver):
    solver.env = self.undotrail.discard(solver.env)
    self.orig_fcont.cut(solver)
  def discard(self): pass

class RuleSetContinuation(ChoiceContinuation):
  def __init__(self, pattern, rules, env, recursive, solver, nextcont):
    ChoiceContinuation.__init__(self, solver, nextcont)
    self.pattern, self.rules, self.env = pattern, rules, env
    self.recursive = recursive

  def activate(self, solver):
    rules = self.rules
    solver.scont = RuleContinuation(self.pattern, rules[0], self.recursive, 
                        self.env, self.solver, self.cont)
    rules = rules.find_applicable_rule()
    if rules is not None:
      self.prepare_more_solutions(solver)
      self.rules = rules    
  def __repr__(self, trail=None): 
    return "UserCon: %s@ %s"%(self.pattern, self.rules)

class RuleContinuation(Continuation):
  def __init__(self, pattern, rule, recursive, env, solver, nextcont):
    Continuation.__init__(self, solver, nextcont)
    self.pattern,self.rule, self.env = pattern, rule, env
    self.recursive = recursive
    
  def activate(self, solver):
    solver.scont = self.cont
    callerEnv = solver.env
    if not self.recursive: solver.env = self.env.extend()
    else: 
      self.env.bindings = {}
      solver.env = self.env
    nextcall = self.rule.apply(solver.env, self.pattern)
    solver.set(RuleDoneContinuation(solver, callerEnv))
    return Cons('begin', nextcall).scont(self.solver)
  def __repr__(self): 
    return "RuleCont: %s @ %r" % (self.pattern, self.rule)

class RuleDoneContinuation(Continuation):
  def __init__(self, solver, callerEnv):
    Continuation.__init__(self, solver)
    self.callerEnv = callerEnv
  def activate(self, solver):
    solver.env = self.callerEnv
    solver.set(self.cont)
  def __repr__(self): return "RuleDoneCont"
    
class CutScopeNotifier(Continuation):
  def __init__(self, solver, nextcont):
    Continuation.__init__(self, solver, nextcont)
    self.cutcell = CutCell()
  def candiscard(self): return not self.cutcell.discarded
  def activate(self, solver):
    self.cutcell.activated = True
    solver.set(self.cont)

  def discard(self):
    assert not self.cutcell.activated
    self.cutcell.discarded = True
    
  def __repr__(self): return 'CutScopeNotifier'

class CutCell(object):
  def __init__(self):
    self.activated = False
    self.discarded = False

class CutDelimiter(FailureContinuation):
  def __init__(self, solver, nextcont, cutcell):
    FailureContinuation.__init__(self, solver, nextcont)
    self.cutcell = cutcell
  def candiscard(self): return not self.cutcell.discarded
  def activate(self, solver): raise NotImplementedError("unreachable")
  def fail(self, solver): 
    self.cont.fail(solver)
  def cut(self, solver):
    if not self.cutcell.activated: solver.fcont = self
    else: self.cont.cut(solver)
  def __repr__(self, trail=None): 
    return "CutDelimiter: %r, %r"%(self.cutcell.activated, self.cutcell.discarded)

def insert_cut_delimiter(solver, scont=None, fcont=None):
  if scont is None: scont = solver.scont
  if fcont is None: fcont = solver.fcont
  if isinstance(fcont, CutDelimiter):
    if fcont.cutcell.activated or fcont.cutcell.discarded:
      fcont = fcont.cont
      if isinstance(scont, CutScopeNotifier) and scont.cutcell.discarded:
        scont = scont.cont
    elif (isinstance(scont, CutScopeNotifier) and scont.cutcell is fcont.cutcell):
      assert not fcont.cutcell.activated
      return scont, fcont
  scont = CutScopeNotifier(solver, scont)
  fcont = CutDelimiter(solver, fcont, scont.cutcell)
  return scont, fcont

class CatchingDelimiter(Continuation):
  def __init__(self, catcher, recover, solver):
    Continuation.__init__(self, solver)
    self.catcher, self.recover = catcher, recover
    self.fcont, self.trail = solver.fcont, solver.env
  def activate(self, solver): solver.set(self.cont, self.fcont, self.trail)
  def __repr__(self): return "(CatchingDelimiter: %s, %s)"%(self.catcher, self.recover)

class BodyContinuation(Continuation):
  def __init__(self, body, solver, nextcont=None):
    Continuation.__init__(self, solver, nextcont)
    self.body = body
  def activate(self, solver):
    solver.scont = self.cont
    self.body.scont(solver)
  def __repr__(self): return "BodyCont: %s" %self.body

class FunctionContinuation(Continuation):
  def __init__(self, function, *arguments): 
    Continuation.__init__(self, arguments[-1])
    self.arguments = arguments[:-1]
    self.function = function
  def activate(self, solver):
    self.function(self, solver.value, solver)
  def __repr__(self): return "%s%s"%(self.function.__name__,self.arguments[:-2])
  __str__ = __repr__

class FunctionContinuationConstructor:
  def __init__(self, function):
    self.function = function
  def __call__(self, *arguments):
    return FunctionContinuation(self.function, *arguments)

funcont = FunctionContinuationConstructor