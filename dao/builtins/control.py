# logic control predicates

from dao.compilebase import CompileTypeError
from dao.command import special, Command, SpecialCall, Const, element, Var
import dao.interlang as il
from dao.interlang import TRUE, FALSE, NONE
from special import begin
from term import unify

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
  fc = compiler.new_var(il.ConstLocalVar('old_fail_cont'))
  return il.begin(il.Assign(fc, il.failcont), 
                  il.SetFailCont(cont),
                  clause.cps_convert(compiler, fc))
  
@special
def cut(compiler, cont):
  return il.begin(il.SetFailCont(il.cut_cont), 
                  cont(NONE))
cut = cut()

def has_cut(exp):
  if exp is cut: 
    return True
  if isinstance(exp, SpecialCall):
    for arg in exp.args:
      if has_cut(arg): 
        return True
    return False
  return False       
  
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

def has_cut_or(exp):
  if exp is cut_or: 
    return True
  if isinstance(exp, SpecialCall):
    if exp.command is or2:
      return False
    else:
      for arg in exp.args:
        if has_cut_or(arg): 
          return True
      return False
  return False

def or2_fun(compiler, cont, clause1, clause2):
  v = compiler.new_var(il.ConstLocalVar('v'))
  v1 = compiler.new_var(il.ConstLocalVar('v'))
  fc = compiler.new_var(il.ConstLocalVar('old_failcont'))
  if has_cut_or(clause1) or has_cut_or(clause2):
    cut_or_cont = compiler.new_var(il.ConstLocalVar('cut_or_cont'))
    or_cont = il.clamda(v, il.SetCutOrCont(cut_or_cont), cont(v))
    return il.begin(
      il.Assign(cut_or_cont, il.cut_or_cont),
      il.SetCutOrCont(il.failcont),
      il.Assign(fc, il.failcont),
      il.SetFailCont(il.clamda(v1, 
        il.SetFailCont(fc),
        clause2.cps_convert(compiler, or_cont))),
      clause1.cps_convert(compiler, or_cont))
  else:
    return il.begin(
      il.Assign(fc, il.failcont),
      il.SetFailCont(il.clamda(v1, 
        il.SetFailCont(fc),
        clause2.cps_convert(compiler, cont))),
      clause1.cps_convert(compiler, cont))

or2 = special(or2_fun)

@special
def first_(compiler, cont, clause1, clause2):
  v = compiler.new_var(il.ConstLocalVar('v'))
  fc = compiler.new_var(il.ConstLocalVar('fc'))
  first_cont = il.clamda(v, il.SetFailCont(fc), cont(v))
  return il.begin(
    il.Assign(fc, il.failcont),
    il.append_failcont(compiler, clause2.cps_convert(compiler, first_cont)),
    clause1.cps_convert(compiler, first_cont))

def first_p(*exps):
  if not exps: raise
  elif len(exps)==1: return once(exps[0])
  elif len(exps)==2: return first_(*exps)
  else:
    return frist_(exps[0], first_p(*exps[1:]))
  
@special
def once(compiler, cont, exp):
  v = compiler.new_var(il.ConstLocalVar('v'))
  fc = compiler.new_var(il.ConstLocalVar('fc'))
  return il.begin(
    il.Assign(fc, il.failcont),
    exp.cps_convert(compiler, 
                        il.clamda(v, il.SetFailCont(fc), cont(v))))

@special
def if_p(compiler, cont, condition, action):
  v = compiler.new_var(il.ConstLocalVar('v'))
  return condition.cps_convert(compiler, 
                    il.Clamda(v, action.cps_convert(compiler, cont)))

# finding all solutions to a goal

@special
def findall_1(compiler, cont, goal):
  v = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  fc = compiler.new_var(il.ConstLocalVar('old_failcont'))
  return il.begin(
    il.Assign(fc, il.failcont), 
    il.SetFailCont(il.clamda(v2, 
          il.SetFailCont(fc),
          cont(v2))),
    goal.cps_convert(compiler, il.Clamda(v, il.failcont(v)))
    )

@special
def findall_2(compiler, cont, goal, template, bag):
  v = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  fc = compiler.new_var(il.ConstLocalVar('old_failcont'))
  bag = bag.interlang()
  template = template.interlang()
  return il.begin(
     il.Assign(bag, il.empty_list),
     il.Assign(fc, il.failcont), 
     il.SetFailCont(il.clamda(v2,
        il.SetFailCont(fc),
        cont(v2))),
      goal.cps_convert(compiler, 
        il.clamda(v, 
          il.ListAppend(bag, il.GetValue(template)),
          il.failcont(v))))

@special
def findall(compiler, cont, goal, template=None, bag=None):
  if bag is None: 
    return findall_1(goal).cps_convert(compiler, cont)
  else:
    _bag  = compiler.new_var(Var(bag.name))
    return begin(findall_2(goal, element(template), _bag), 
                 unify(bag, _bag)).cps_convert(compiler, cont)
#findall:
  #findall goal: goal, fail
    
# infinite recursive, maxizism recursive level
# solutions: cfunction become while loop.
@special
def repeat(compiler, cont):
  v = compiler.new_var(il.ConstLocalVar('v'))
  function = compiler.new_var(il.ConstLocalVar('function'))
  return il.begin(il.cfunction(function, v, cont(v)),
                  il.PrintLn(il.String('repeat')),
                  il.SetFailCont(function), 
                  function(NONE))

repeat = repeat()
 