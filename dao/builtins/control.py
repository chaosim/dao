# logic control predicates

from dao.command import special, Command, SpecialCall
import dao.interlang as il
from dao.compilebase import CompileTypeError

from dao.interlang import TRUE, FALSE, NONE

v0, fc0 = il.Var('v'), il.Var('fc')

@special
def succeed(compiler, cont):
  return cont(TRUE)

succeed = succeed()

@special
def fail(compiler, cont):
  return il.failcont(TRUE)

fail = fail()

@special
def cut(compiler, cont):
  v = compiler.new_var(v0)
  return il.Begin(il.SetFailCont(il.cut_cont), 
                  il.Clamda(v, cont(v)))

@special
def not_p(compiler, cont, clause):
  fc = compiler.new_var(il.Var('old_fail_cont'))
  return il.begin(il.Assign(fc, il.failcont), 
                  il.SetFailCont(cont),
                  clause.cps_convert(compiler, fc))
  
#@special
#def cut_or(compiler, cont):
  #return il.Begin(il.SetFailCont(il.cut_or_cont), 
                  #il.Clamda(v, cont(v)))
  
@special
def or_(compiler, cont, clause1, clause2):
  v = compiler.new_var(v0)
  cut_or_cont = compiler.new_var(il.Var('cut_or_cont'))
  or_cont = il.clamda(v, il.SetCutOrCont(cut_or_cont), cont(v))
  return il.begin(
    il.Assign(cut_or_cont, il.cut_or_cont),
    il.SetCutOrCont(il.failcont),  
    il.append_fail_cont(compiler, clause2.cps_convert(compiler, or_cont)),
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
  if bag is None:
    return il.begin(
      il.AppendFailCont(cont(NONE)),
      cps_convert(compiler, goal, il.Clamda(v, il.failcont(v)))
      )
  else:
    result = il.Var('findall_result') # variable capture
    return il.begin(
       il.Assign(result, il.empty_list()),
       il.AppendFailCont(
          cps_convert(compiler, unify(bag, result), cont)),
        cps_convert(compiler, goal, 
          il.clamda(v, 
            il.ListAppend(result, il.GetValue(template)),
            il.failcont(v)))
        )
  
# infinite recursive, maxizism recursive level
# solutions: trampoline
@special
def repeat(compiler, cont):
  v = compiler.new_var(v0)
  function = compiler.new_var(il.Var('function'))
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
