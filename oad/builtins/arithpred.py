from oad import helper, error, builtin 
from oad.term import Var, ClosureVar, deref#, Integer, SUCCESS
##from oad.cont import Continuation, ChoiceContinuation

# arithmetic

class BetweenContinuation:#(ChoiceContinuation):
  def __init__(self, lower, varorint, upper, evaluator):
    ChoiceContinuation.__init__(self, evaluator)
    self.orig_fcont, self.undotrail = evaluator.fcont, evaluator.trail
    self.lower, self.varorint, self.upper = lower, varorint, upper
    self.value = lower

  def activate(self, evaluator):
    if self.value<=self.upper:
      self.prepare_more_solutions(evaluator)
      try: 
        self.varorint.unify(Integer(self.value), evaluator.trail)
      except error.UnifyFail, e: 
        return evaluator.set(evaluator.fcont, self.orig_fcont)
      finally: self.value +=1
      evaluator.value = SUCCESS
      return evaluator.set(self.cont)
    raise error.UnifyFail()

@builtin.function2()
def between(evaluator, lower, upper, varorint):
  lower = deref(lower, evaluator.trail)
  if isinstance(lower, Var): error.throw_instantiation_error()
  lower = helper.unwrap_int(lower)
  upper = deref(upper, evaluator.trail)
  if isinstance(upper, Var): error.throw_instantiation_error()
  upper = helper.unwrap_int(upper)
  varorint = varorint.deref(evaluator.trail)
  if isinstance(varorint, Var):
    evaluator.set(BetweenContinuation(lower,varorint,upper,evaluator))
  else:
    varorint = helper.unwrap_int(varorint)
    if not (lower<=varorint<=upper): raise error.UnifyFail
  return SUCCESS

@builtin.function2('====')
def equal(evaluator, left, right):
  if not deref(left, evaluator.trail)==deref(right, evaluator.trail): 
    raise error.UnifyFail
  evaluator.value = SUCCESS

class Is_Continuation:#(Continuation): 
  def __init__(self, arg, evaluator):
    Continuation.__init__(self, evaluator)
    self.arg = arg
  def activate(self, evaluator):
    self.arg.unify(evaluator.value, evaluator.trail)
    evaluator.value = SUCCESS
    evaluator.set(self.cont)
  def _repr(self): return 'is_cont'

@builtin.macro('is')
def is_(evaluator, var, func):
  func = func.deref(evaluator.trail)
  evaluator.set(Is_Continuation(var, evaluator))
  func.scont(evaluator)

# should I use static single assign in the oad language? means forbidding assign?
# implemented in compiler or as a rule of the oad language?
class AssignContinuation:#(Continuation): 
  def __init__(self, arg, evaluator):
    Continuation.__init__(self, evaluator)
    self.arg = arg
  def activate(self, evaluator):
    var = self.arg.var
    env = evaluator.env
    while env is not None:
      if env.hasBindings() and var not in env.bindings:
        env = env.outer
      else: break
    else: raise Exception('%s is not defined'%var)
    env[var] = evaluator.value
    evaluator.value = SUCCESS
    evaluator.set(self.cont)
  def _repr(self): return 'assign_cont'

@builtin.macro('set')
def assign(evaluator, var, value):
  evaluator.set(AssignContinuation(var, evaluator))
  return value.scont(evaluator)

@builtin.macro()
def  define(evaluator, var, value):
  if isinstance(var, ClosureVar): var = var.var
  value = deref(value, evaluator.env)
  evaluator.env[var] = value
  yield value

class ArgumentContinuation:#(Continuation): 
  def __init__(self, var1, evaluator, scont):
    Continuation.__init__(self, evaluator, scont)
    self.var1 = var1
  def activate(self, evaluator):
    self.cont.value0 = evaluator.value
    evaluator.set(self.cont)
    self.var1.scont(evaluator)
  def _repr(self): return 'arg_cont'
  
class ApplyContinuation:#(Continuation): 
  def __init__(self, function, evaluator):
    Continuation.__init__(self, evaluator)
    self.function = function
  def activate(self, evaluator):
    if not self.function(self.value0, evaluator.value): 
      raise error.UnifyFail()
    evaluator.value = SUCCESS
    evaluator.set(self.cont)
  def _repr(self): return 'apply_cont'

def arithmeticCmpPredicate(function, name):
  @builtin.macro(name)
  def pred(evaluator, var0, var1):
    var0 = deref(var0, evaluator.trail)
    var1 = deref(var1, evaluator.trail)
    scont = ApplyContinuation(function, evaluator)
    evaluator.set(ArgumentContinuation(var1, evaluator, scont))
    var0.scont(evaluator)
  return pred

eq = arithmeticCmpPredicate(lambda x,y: x==y, '===')
ne = arithmeticCmpPredicate(lambda x,y: x!=y, '!=')
lt = arithmeticCmpPredicate(lambda x,y: x<y, '<')
le = arithmeticCmpPredicate(lambda x,y: x<=y, '<=')
gt = arithmeticCmpPredicate(lambda x,y: x>y, '>')
ge = arithmeticCmpPredicate(lambda x,y: x>=y, '>=')
