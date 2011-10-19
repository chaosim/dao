from dao.term import Var, deref, Apply, getvalue
from dao import builtin
from dao.solve import CutException
from dao.builtin import Builtin, Function
from dao.term import Apply

# control predicates

# call with current continuation

class ContinuationFunction(Builtin, Function):
  def apply(self, solver, values, cont):
    return self.function(values[0], solver)
      
@builtin.macro()
def callcc(solver, cont, fun):
  ''' call with current continuation '''
  yield solver.cont(Apply(fun, ContinuationFunction(cont)), cont), fun

# finding all solutions to a goal

@builtin.macro()
def findall(solver, cont, goal, template, bag):
  goal = deref(goal, solver.env)
  result = []
  for c, x in solver.exp_run_cont(goal, cont):
    result.append(getvalue(template, solver.env))
  for x in bag.unify(result, solver.env):
    yield cont, True
    
# meta call predicates

@builtin.macro()
def call(solver, cont, pred): yield solver.cont(deref(pred, solver.env), cont), True

@builtin.macro()
def once(solver, cont, pred):
  for c, x in solver.exp_run_cont(deref(pred, solver.env), cont):
    yield c, x
    return

@builtin.macro()
def succeed(solver, cont): yield cont, True
succeed = succeed()

@builtin.macro()
def fail(solver, cont):  
  if 0: yield cont, True
  return
fail = fail()

@builtin.macro()
def repeat(solver, cont):
  def repeat_cont(value, solver):
    while 1: 
      yield cont, True
##  repeat_cont.cut = True
  yield repeat_cont, True

repeat = repeat()
  
@builtin.macro('!')
def cut(solver, cont):
  yield cont, True
  raise CutException
cut = cut()

@builtin.macro('and_p')
def and_p(solver, cont, *calls):
  if len(calls)==1:
    call = deref(calls[0], solver.env)
    yield solver.cont(call, cont), True
  elif len(calls)==2:
    call1 = deref(calls[0], solver.env)
    call2 = deref(calls[1], solver.env)
    def and_cont(value, solver): yield solver.cont(call2, cont), value
    yield solver.cont(call1, and_cont), True
  else: 
    call1 = deref(calls[:-1], solver.env)
    call2 = deref(calls[-1], solver.env)
    def and_cont(value, solver): yield solver.cont(call2, cont), value
    yield solver.cont(and_p(*call1), and_cont), True
    
@builtin.macro('or_p')
def or_p(solver, cont, call1, call2):
  call1 = deref(call1, solver.env)
  call2 = deref(call2, solver.env)
  if isinstance(call1, Apply) and call1.operator==if_p: # A -> B; C
    if_clause = deref(call1.operand[0], solver.env)
    then_clause = deref(call1.operand[1], solver.env)
    call1 = if_clause&cut&then_clause
  def or_cont(value, solver):
    yield solver.cont(call1, cont), True
    yield solver.cont(call2, cont), True
##  or_cont.cut = True
  yield or_cont, True

@builtin.macro('first_p')
def first_p(solver, cont, call1, call2):
  call1 = deref(call1, solver.env)
  solved = False
  for c, value in solver.exp_run_cont(call1, cont):
    solved = True
    yield c, value
  if solved: return
  call2 = deref(call2, solver.env)
  yield solver.cont(call2, cont), True

@builtin.macro('->')  
def if_p(solver, cont, if_clause, then_clause):
  # This unusual semantics is part of the ISO and all de-facto Prolog standards.
  # see SWI-Prolog help.
  if_clause = deref(if_clause, solver.env)
  then_clause = deref(then_clause, solver.env)
  def if_p_cont(value, solver):
    if not value: return
    yield solver.cont(then_clause, cont), True
  yield solver.cont(if_clause, if_p_cont), True

@builtin.macro('not')  
def not_p(solver, cont, call):
  call = deref(call, solver.env)
  for c, x in solver.exp_run_cont(call, cont):
    return
  yield cont, True