# logic control predicates

from dao.command import special, Command, SpecialCall
import dao.interlang as il
from dao.compilebase import CompileTypeError

from dao.interlang import TRUE, FALSE, NONE

v0, fc0 = il.LocalVar('v'), il.LocalVar('fc')

@special
def succeed(compiler, cont):
  return cont(TRUE)

succeed = succeed()

@special
def fail(compiler, cont):
  return il.failcont(TRUE)

fail = fail()

@special
def not_p(compiler, cont, clause):
  fc = compiler.new_var(il.LocalVar('old_fail_cont'))
  return il.begin(il.Assign(fc, il.failcont), 
                  il.SetFailCont(cont),
                  clause.cps_convert(compiler, fc))
  
@special
def cut(compiler, cont):
  return il.begin(il.SetFailCont(il.cut_cont), 
                  cont(NONE))
cut = cut()

@special
def cut_or(compiler, cont):
  return il.begin(il.SetFailCont(il.cut_or_cont), 
                  cont(NONE))
cut_or = cut_or()

from special import begin as and_

def or_(*clauses):
  if not clauses: raise CompileTypeError("should have least 1 clauses in or")
  if len(clauses)==1: return clauses[0]
  elif len(clauses)==2: return or2(*clauses)
  else:
    return or2(clauses[0], or_(*clauses[1:]))
  
#@special
#def or2(compiler, cont, clause1, clause2):
  #v = compiler.new_var(v0)
  #v1 = compiler.new_var(v0)
  #cut_or_cont = compiler.new_var(il.LocalVar('cut_or_cont'))
  #or_cont = il.clamda(v, il.SetCutOrCont(cut_or_cont), cont(v))
  #return il.begin(
    #il.Assign(cut_or_cont, il.cut_or_cont),
    #il.SetCutOrCont(il.failcont),  
    #il.SetFailCont(il.clamda(v1, clause2.cps_convert(compiler, or_cont))),
    #clause1.cps_convert(compiler, or_cont))

@special
def or2(compiler, cont, clause1, clause2):
  v = compiler.new_var(v0)
  v1 = compiler.new_var(v0)
  fc = compiler.new_var(il.LocalVar('old_failcont'))
  cut_or_cont = compiler.new_var(il.LocalVar('cut_or_cont'))
  or_cont = il.clamda(v, il.SetCutOrCont(cut_or_cont), cont(v))
  return il.begin(
    il.Assign(cut_or_cont, il.cut_or_cont),
    il.SetCutOrCont(il.failcont),
    il.Assign(fc, il.failcont),
    il.SetFailCont(il.clamda(v1, 
      il.SetFailCont(fc),
      clause2.cps_convert(compiler, or_cont))),
    clause1.cps_convert(compiler, or_cont))

@special
def first_p(compiler, cont, clause1, clause2):
  v = compiler.new_var(v0)
  fc = compiler.new_var(fc)
  first_cont = il.Clamda(v, il.SetFailCont(fc), cont(v))
  return il.Begin(
    il.Assign(fc, il.failcont),
    il.AppendFailCont(clause2.cps_convert(compiler, first_cont)),
    cps_convert(clause1.compiler, first_cont))

@special
def if_p(compiler, cont, condition, action):
  v = compiler.new_var(v0)
  return condition.cps_convert(compiler, 
                    il.Clamda(v, action.cps_convert(compiler, cont)))

# finding all solutions to a goal

@special
def findall(compiler, cont, goal, template=NONE, bag=None):
  v = compiler.new_var(v0)
  v2 = compiler.new_var(v0)
  fc = compiler.new_var(il.LocalVar('old_failcont'))
  if bag is None:
    return il.begin(
      il.Assign(fc, il.failcont), 
      il.SetFailCont(il.clamda(v2, 
            il.SetFailCont(fc),
            cont(v2))),
      goal.cps_convert(compiler, il.Clamda(v, il.failcont(v)))
      )
  else:
    result = compiler.new_var(il.LocalVar('findall_result')) # variable capture
    return il.begin(
       il.Assign(result, il.empty_list()),
       il.Assign(fc, il.failcont), 
       il.SetFailCont(il.clamda(v2,
          il.SetFailCont(fc),
          unify(bag, result).cps_convert(compiler, cont))),
        goal.cps_convert(compiler, 
          il.clamda(v, 
            il.ListAppend(result, il.GetValue(template)),
            il.failcont(v)))
        )

#findall:
  #findall goal: goal, fail
    
# infinite recursive, maxizism recursive level
# solutions: trampoline
@special
def repeat(compiler, cont):
  v = compiler.new_var(v0)
  function = compiler.new_var(il.LocalVar('function'))
  return il.begin(il.SetFailCont(function), 
                  il.cfunction(function, v, cont(v)))

repeat = repeat()
  
def xxxif_p(solver, if_clause, then_clause):
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
