from dao.compilebase import CompileTypeError
from dao.command import special, Command, SpecialCall
import dao.interlang as il
from dao.interlang import TRUE, FALSE, NONE
from dao.interlang import LogicVar

v0, fc0 = il.LocalVar('v'), il.LocalVar('fc')

# analysing and construction terms

@special
def unify(compiler, cont, x, y):
  try: 
    x_cps_convert_unify = x.cps_convert_unify
  except:
    try: y_cps_convert_unify = y.cps_convert_unify
    except:
      if x==y: return cont(TRUE)
      else: return il.failcont(TRUE)
    return y_cps_convert_unify(x, compiler, cont)
  return x_cps_convert_unify(y, compiler, cont)


'''
@builtin.macro()
def getvalue(solver, item):
  return term.getvalue(item, solver.env, {})
  
@builtin.macro('getvalue_default', 'getvalue@')
def getvalue_default(solver, item, default=None):
  value = term.getvalue(item, solver.env, {})
  if isinstance(value, Var): value = default
  return value
  
def is_ground(term):
  if isinstance(term, Var): return False
  if isinstance(term, Cons): 
    if not is_ground(term.head): return False
    if not is_ground(term.tail): return False
  return True
  
@builtin.macro('ground')
def ground(solver, item):
  return is_ground(term.getvalue(item, solver.env, {}))
  
@builtin.macro('ground_p', 'ground!')
def ground_p(solver, item):
  if is_ground(term.getvalue(item, solver.env, {})): return True
  else: solver.scont = solver.fcont
  
@builtin.macro()
def setvalue(solver, var, value):
  # necessary to deref, see Colosure.deref for the reason
  if isinstance(var, ClosureVar): var = var.var 
  var = deref(var, solver.env)
  assert isinstance(var, Var)
  @mycont(cont)
  def setvalue_cont(value, solver):
    old = var.getvalue(solver.env, {})
    var.setvalue(value, solver.env)
    solver.scont = cont
    old_fcont = solver.fcont
    @mycont(old_fcont)
    def fcont(value, solver):
      var.setvalue(old, solver.env)
      solver.scont = old_fcont
    solver.fcont = fcont
    return True
  solver.scont = solver.cont(value, setvalue_cont)
  return True

@builtin.macro('is', '<-')
def is_(solver, var, func):
  cont = solver.scont
  @mycont(cont)
  def is_cont(value, solver):
    if term.unify(var, value, solver):
      solver.scont = cont
      return True
  solver.scont = solver.cont(func, is_cont)
  return True

@builtin.macro()
def define(solver, var, value):
  if isinstance(var, ClosureVar): var = var.var
  value = deref(value, solver.env)
  cont = solver.scont
  @mycont(cont)
  def define_cont(value, solver):
    bindings = solver.env.bindings
    try:
      old = bindings[var]
      old_fcon = solver.fcont
      @mycont(old_fcon)
      def fcont(value, solver):
        bindings[var] = old
        solver.fcont = old_fcon
      solver.fcont = fcont
      solver.scont = cont
      return value
    except KeyError:
      bindings[var] = value
      old_fcon = solver.fcont
      @mycont(old_fcon)
      def fcont(value, solver):
        del bindings[old]
        solver.fcont = old_fcon
      solver.fcont = fcont
      solver.scont = cont
      return value
  solver.scont = solver.cont(value, define_cont)
  return True

@builtin.macro()
def define_outer(solver, var, value):
  if isinstance(var, ClosureVar): var = var.var
  value = deref(value, solver.env)
  @mycont(cont)
  def define_cont(value, solver):
    bindings = solver.env.outer.bindings
    try:
      old = bindings[var]
      old_fcon = solver.fcont
      @mycont(old_fcon)
      def fcont(value, solver):
        bindings[var] = old
        solver.fcont = old_fcon
      solver.fcont = fcont
      solver.scont = cont
      return value
    except:
      old_fcon = solver.fcont
      @mycont(old_fcon)
      def fcont(value, solver):
        del bindings[old]
        solver.fcont = old_fcon
      solver.fcont = fcont
      solver.scont = cont
      return value
  yield solver.cont(value, define_cont), True

@builtin.macro()
def define_global(solver, var, value):
  if isinstance(var, ClosureVar): var = var.var
  value = deref(value, solver.env)
  @mycont(cont)
  def define_cont(value, solver):
    bindings = solver.global_env.bindings
    try:
      old = bindings[var]
      old_fcon = solver.fcont
      @mycont(old_fcon)
      def fcont(value, solver):
        bindings[var] = old
        solver.fcont = old_fcon
      solver.fcont = fcont
      solver.scont = cont
      return value
    except:
      old_fcon = solver.fcont
      @mycont(old_fcon)
      def fcont(value, solver):
        del bindings[old]
        solver.fcont = old_fcon
      solver.fcont = fcont
      solver.scont = cont
      return value
  solver.scont = solver.cont(value, define_cont)
  return True

@builtin.macro()
def copy_term(solver, item, copy):
  return term.unify(copy, term.copy(item, {}), solver)
 
# comparison and unification of terms

@builtin.macro('unify', '=:=')
def unify(solver, v0, v1):
  return term.unify(v0, v1, solver)

@builtin.macro('unify_with_occurs_check')
def unify_with_occurs_check(solver, v0, v1):
  return term.unify(v0, v1, solver.env, occurs_check=True)

@builtin.macro('notunify', '=\=')
def notunify(solver, var0, var1):
  fcont = solver.fcont
  scont = solver.scont
  if term.unify(var0, var1, solver.env): 
    solver.scont = fcont
    return False
  else: 
    solver.scont = scont
    return True
  
# type verifications

@builtin.macro('var')
def isvar(solver, arg):
  return isinstance(arg, Var)

@builtin.macro('var', 'var!')
def isvar_p(solver, arg):
  if isinstance(arg, Var): return True
  else: solver.scont = solver.fcont

@builtin.macro('nonvar')
def nonvar(solver, arg):  
  return not isinstance(arg, Var)

@builtin.macro('nonvar', 'nonvar!')
def nonvar_p(solver, arg):  
  if not isinstance(arg, Var): return True
  else: solver.scont = solver.fcont
  
def is_free(var, env):
  if isinstance(var, ClosureVar): var = var.var
  return isinstance(deref(var, env), Var)
  
@builtin.macro('free')
def free(solver, arg):
  return is_free(arg, solver.env)

@builtin.macro('free_p', 'free!')
def free_p(solver, arg):
  if is_free(arg, solver.env): return True
  else: solver.scont = solver.fcont

@builtin.macro('bound')
def bound(solver, var):
  assert(isinstance(var, Var))
  if isinstance(var, ClosureVar): var = var.var
  return solver.env[var] is not var
  
@builtin.macro('bound_p', 'bound!')
def bound_p(solver, var):
  assert(isinstance(var, Var))
  if isinstance(var, ClosureVar): var = var.var
  if solver.env[var] is not var: return True
  else: solver.scont = solver.fcont
    
@builtin.macro('unbind')
def unbind(solver, var):
  if isinstance(var, ClosureVar): var = var.var
  env = solver.env
  bindings = []
  while env is not solver.global_env:
    try: 
      bindings.append((env.bindings, env.bindings[var]))
    except: pass
    env = env.outer
  for b, _ in bindings:
    del b[var]
  old_fcont = solver.fcont
  @mycont(old_fcont)
  def fcont(value, solver):
    for b, v in bindings:
      b[var] = v
    solver.scont = old_fcont
  solver.fcont = fcont
  return True
  
@builtin.macro('isinteger', 'isinteger!')
def isinteger(solver, arg):
 return isinstance(getvalue(arg, env, {}), int)

@builtin.macro('isinteger_p', 'isinteger!')
def isinteger_p(solver, arg):
  if isinstance(getvalue(arg, env, {}), int): return True
  else: solver.scont = solver.fcont

@builtin.macro('isfloat', 'isfloat')
def isfloat(solver, arg):
  return isinstance(getvalue(arg, env, {}), float)

@builtin.macro('isfloat_p', 'isfloat!')
def isfloat_p(solver, arg):
  if isinstance(getvalue(arg, env, {}), float): return True
  else: solver.scont = solver.fcont

@builtin.macro('isnumber', 'isnumber')
def isnumber(solver, arg):
  return isinstance(getvalue(arg, env, {}), int) \
              or isinstance(getvalue(arg, env, {}), float)

@builtin.macro('isnumber_p', 'isnumber!')
def isnumber_p(solver, arg):
  if isinstance(getvalue(arg, env, {}), int) \
     or isinstance(getvalue(arg, env, {}), float): 
    return True
  else: solver.scont = solver.fcont

@builtin.function('istuple?')
def istuple(x):  return isinstance(x, tuple)

@builtin.function('islist?')
def islist(x): return isinstance(x, list)

@builtin.macro('iscons_p', 'iscons!')
def iscons_p(solver, arg):
  if isinstance(getvalue(arg, env, {}), Cons): return True
  else: solver.scont = solver.fcont

@builtin.macro('iscons')
def iscons2(solver, arg):
  return isinstance(arg, Cons)

@builtin.function('cons_f', 'iscons?')
def is_cons(x): return isinstance(x, Cons)

@builtin.function('pycall')
def pycall(fun, *args):  
  return fun(*args)

@builtin.function('py_apply')
def py_apply(fun, args):  
  return fun(*args)

'''