from dao.term import Var, deref, CommandCall, getvalue
from dao import builtin
from dao.solve import CutException, mycont
from dao.builtin import Builtin, Function
from dao.term import CommandCall

# control predicates

# call with current continuation

class ContinuationFunction(Builtin, Function):
  def apply(self, solver, cont, values, signatures):
    return self.function(values[0], solver)
      
@builtin.macro('callcc', 'call/cc')
def callcc(solver, cont, fun):
  ''' call with current continuation '''
  yield solver.cont((fun, ContinuationFunction(cont)), cont), fun

# finding all solutions to a goal

@builtin.macro()
def findall(solver, cont, goal, template=None, bag=None):
  goal = deref(goal, solver.env)
  if bag is not None:
    result = []
    for c, x in solver.exp_run_cont(goal, cont):
      result.append(getvalue(template, solver.env))
    for x in bag.unify(result, solver.env):
      yield cont, True
  else:
    for c, x in solver.exp_run_cont(goal, cont):
      pass
    yield cont, True
    
# meta call predicates

@builtin.macro()
def call(solver, cont, pred): 
  yield solver.cont(getvalue(pred, solver.env), cont), True

@builtin.macro()
def once(solver, cont, pred):
  for c, x in solver.exp_run_cont(getvalue(pred, solver.env), cont):
    yield c, x
    return

@builtin.macro()
def succeed(solver, cont): 
  yield cont, True
succeed = succeed()

@builtin.macro()
def fail(solver, cont):  
  if 0: yield cont, True
  return
fail = fail()

@builtin.macro()
def repeat(solver, cont):
  @mycont(cont)
  def repeat_cont(value, solver):
    while 1: 
      yield cont, True
##  repeat_cont.cut = True
  yield repeat_cont, True

repeat = repeat()
  
@builtin.macro('cut', '!')
def cut(solver, cont):
  yield cont, True
  raise CutException
cut = cut()

@builtin.macro('and_p', '&!')
def and_p(solver, cont, *calls):
  if len(calls)==0:  
    yield value_cont(None, cont), True
  if len(calls)==1:
    yield solver.cont(calls[0], cont), True
  else:
    @mycont(cont)
    def and_cont(value, solver): 
      yield solver.cont(calls[-1], cont), value
    if len(calls)==2: 
      yield solver.cont(calls[0], and_cont), True
    else: 
      yield solver.exps_cont(calls[:-1], and_cont), True
    
@builtin.macro('or_p', '|!')
def or_p(solver, cont, *calls):
  if len(calls)==0:  
    yield value_cont(None, cont), True
  call0 = deref(calls[0], solver.env)
  if isinstance(call0, CommandCall) and call0.operator==if_p: # A -> B; C
    if_clause = deref(call0.operand[0], solver.env)
    then_clause = deref(call0.operand[1], solver.env)
    calls = (and_p(if_clause, cut, then_clause),)+calls[1:]
  for call in calls:
    yield solver.cont(call, cont), True

@builtin.macro('first_p', 'first!')
def first_p(solver, cont, *calls):
  solved = False
  for call in calls:
    for c, value in solver.exp_run_cont(call, cont):
      solved = True
      yield c, value
    if solved: return

@builtin.macro('if_p', '->')  
def if_p(solver, cont, if_clause, then_clause):
  # This unusual semantics is part of the ISO and all de-facto Prolog standards.
  # see SWI-Prolog help.
  if_clause = deref(if_clause, solver.env)
  then_clause = deref(then_clause, solver.env)
  @mycont(cont)
  def if_p_cont(value, solver):
##    if not value: return # important! logic predicate if_p decide whether to continue by the fail or succeed of the condition.
    yield solver.cont(then_clause, cont), True
  yield solver.cont(if_clause, if_p_cont), True

@builtin.macro('not', 'not!')  
def not_p(solver, cont, call):
  call = deref(call, solver.env)
  for c, x in solver.exp_run_cont(call, cont):
    return
  yield cont, True