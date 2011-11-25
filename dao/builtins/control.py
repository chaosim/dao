from dao.term import Var, deref, CommandCall, getvalue
from dao import builtin
from dao.solve import CutException, mycont
from dao.builtin import Builtin, Function
from dao.term import CommandCall, unify
from dao.solve import DaoError

# control predicates

# call with current continuation

class ContinuationFunction(Builtin, Function):
  def apply(self, solver, values, signatures):
    return self.function(values, solver)
      
@builtin.macro('callcc', 'call/cc')
def callcc(solver, fun):
  ''' call with current continuation '''
  solver.scont = solver.cont((fun, ContinuationFunction(solver.scont, '', '', False)), solver.scont)
  return fun

# finding all solutions to a goal

@builtin.macro()
def findall(solver, goal, template=None, bag=None):
  goal = deref(goal, solver.env)
  if bag is not None:
    result = []
    for c, x in solver.exp_run_cont(goal, solver.scont):
      result.append(getvalue(template, solver.env, {}))
    return unify(bag, result, solver)
  else:
    for c, x in solver.exp_run_cont(goal, solver.scont):
      pass
    return True
    
# meta call predicates

@builtin.macro()
def call(solver, pred): 
  solver.scont = solver.cont(getvalue(pred, solver.env, {}), solver.scont)
  return True

@builtin.macro()
def once(solver, pred):
  for c, x in solver.exp_run_cont(getvalue(pred, solver.env, {}), solver.scont):
    solver.scont = c
    return x

@builtin.function('succeed')
def error(*args): 
  raise DaoError(' '.join([repr(x) for x in args]))

@builtin.macro('succeed')
def Succeed(solver): 
  return True
succeed = Succeed()

#Succeed.compile_to_cont 

@builtin.macro('fail')
def Fail(solver):  
  solver.scont = solver.fcont
fail = Fail()

@builtin.macro()
def Repeat(solver):
  cont = solver.scont
  @mycont(cont)
  def repeat_cont(value, solver):
    solver.scont = cont
    return True
##  repeat_cont.cut = True
  solver.scont = solver.fcont = repeat_cont
  return True

repeat = Repeat()
  
@builtin.macro('cut', '!')
def Cut(solver):
  old_fcont = solver.fcont
  @mycont(old_fcont)
  def fcont(value, solver):
    cont = old_fcont
    while cont is not solver.stop_cont and cont is not solver.fail_stop:
      try: cont.cut
      except: 
        cont = cont.cont
        continue
      solver.scont = cont.cont
      return
    solver.scont = old_fcont
  solver.fcont = fcont     
  return True
cut = Cut()

@builtin.macro('cut_or', '!!')
def CutOr(solver):
  old_fcont = solver.fcont
  @mycont(old_fcont)
  def fcont(value, solver):
    cont = old_fcont
    while cont is not solver.stop_cont and cont is not solver.fail_stop:
      try: cont.cut_or
      except: 
        cont = cont.cont
        continue
      solver.scont = cont.cont
      return
    solver.scont = old_fcont
  solver.fcont = fcont     
  return True
cut = Cut()

@builtin.macro('and_p', '&!')
def and_p(solver, *calls):
  cont = solver.scont
  if len(calls)==0:  
    solver.scont = value_cont(None, cont)
    return True
  if len(calls)==1:
    solver.scont = solver.cont(calls[0], cont)
    return True
  else:
    @mycont(cont)
    def and_cont(value, solver): 
      solver.scont = solver.exps_cont(calls[1:], cont)
      return value
    solver.scont = solver.cont(calls[0], and_cont)
    return True
    
@builtin.macro('or_p', '|!')
def or_p(solver, *calls):
  cont = solver.scont
  if len(calls)==0:  
    solver.scont = value_cont(None, cont)
    return True
  call0 = deref(calls[0], solver.env)
  if call0[0]==if_p: # A -> B; C
    if_clause = deref(call0[1], solver.env)
    then_clause = deref(call0[2], solver.env)
    call0 = (and_p, if_clause, (Cut, ), then_clause)
  old_fcont = solver.fcont
  @mycont(old_fcont)
  def or_cut_cont(value, solver):
    solver.scont = old_fcont
  or_cut_cont.cut_or = True
  solver.fcont = or_cut_cont
  env = solver.env
  @mycont(or_cut_cont)
  def or_fcont(value, solver):
    solver.fcont = or_cut_cont
    solver.env = env
    if len(calls[1:])==1:
      solver.scont = solver.cont(calls[1], cont)
      return True
    else:
      solver.scont = solver.cont((or_p, )+calls[1:], cont)
      return True
  solver.fcont = or_fcont
  solver.scont = solver.cont(call0, cont)
  return True
  
@builtin.macro('first_p', 'first!')
def first_p(solver, *calls):
  cont = solver.scont
  for call in calls:
    for c, value in solver.exp_run_cont(call, cont):
      solver.scont = c
      return value

@builtin.macro('if_p', '->')  
def if_p(solver, if_clause, then_clause):
  # This unusual semantics is part of the ISO and all de-facto Prolog standards.
  # see SWI-Prolog help.
  cont = solver.scont
  if_clause = deref(if_clause, solver.env)
  then_clause = deref(then_clause, solver.env)
  @mycont(cont)
  def if_p_cont(value, solver):
    # if not value: return !!! It's is necessary to comment this line
    # important! logic predicate if_p decide whether to continue 
    # by the fail or succeed of the condition.
    solver.scont = solver.cont(then_clause, cont)
    return True
  solver.scont = solver.cont(if_clause, if_p_cont)
  return True

@builtin.macro('not_p', 'not_p')  
def not_p(solver, call):
  call = deref(call, solver.env)
  parse_state = solver.parse_state
  for c, x in solver.exp_run_cont(call, solver.scont):
    solver.parse_state = parse_state
    solver.scont = solver.fcont
    return False
  return True