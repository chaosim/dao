from dao.term import Var, unify, deref, Cons, ClosureVar
import dao.term as term
from dao import builtin
from dao.solve import mycont

# analysing and construction terms

@builtin.macro()
def getvalue(solver, item):
  yield cont, term.getvalue(item, solver.env, {})
  
@builtin.macro('getvalue_default', 'getvalue@')
def getvalue_default(solver, item, default=None):
  value = term.getvalue(item, solver.env, {})
  if isinstance(value, Var): value = default
  yield cont, value
  
def is_ground(term):
  if isinstance(term, Var): return False
  if isinstance(term, Cons): 
    if not is_ground(term.head): return False
    if not is_ground(term.tail): return False
  return True
  
@builtin.macro('ground')
def ground(solver, item):
  yield cont, is_ground(term.getvalue(item, solver.env, {}))
  
@builtin.macro('ground_p', 'ground!')
def ground_p(solver, item):
  if is_ground(term.getvalue(item, solver.env, {})): yield cont, True
  
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
    yield cont, True
    var.setvalue(old, solver.env)
  yield solver.cont(value, setvalue_cont), True

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
      def fcont(value, solver):
        bindings[var] = old
        solver.fcont = old_fcon
      solver.fcont = fcont
      solver.scont = cont
      return value
    except KeyError:
      bindings[var] = value
      old_fcon = solver.fcont
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
      yield cont, value
      binsings[old] = value
    except:
      bindings[var] = value
      yield cont, value
      del bindings[var]
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
      yield cont, value
      binsings[old] = value
    except:
      bindings[var] = value
      yield cont, value
      del bindings[var]
  yield solver.cont(value, define_cont), True

@builtin.macro()
def copy_term(solver, item, copy):
  for _ in term.unify(copy, term.copy(item, {}), solver.env):
    yield cont, True
 
# comparison and unification of terms

@builtin.macro('unify', '=:=')
def unify(solver, v0, v1):
  for _ in term.unify(v0, v1, solver.env): 
    yield cont, True

@builtin.macro('unify_with_occurs_check')
def unify_with_occurs_check(solver, v0, v1):
  for _ in term.unify(v0, v1, solver.env, occurs_check=True): 
    yield cont, True

@builtin.macro('notunify', '=\=')
def notunify(solver, var0, var1):
  for _ in term.unify(var0, var1, solver.env): 
    return
  else: yield cont, True
  
# type verifications

@builtin.macro('var')
def isvar(solver, arg):
  yield cont, isinstance(arg, Var)

@builtin.macro('var', 'var!')
def isvar_p(solver, arg):
  if isinstance(arg, Var): yield cont, True

@builtin.macro('nonvar')
def nonvar(solver, arg):  
  yield cont, not isinstance(arg, Var)

@builtin.macro('nonvar', 'nonvar!')
def nonvar_p(solver, arg):  
  if not isinstance(arg, Var): yield cont, True

def is_free(var, env):
  if isinstance(var, ClosureVar): var = var.var
  return isinstance(deref(var, env), Var)
  
@builtin.macro('free')
def free(solver, arg):
  yield cont, is_free(arg, solver.env)

@builtin.macro('free_p', 'free!')
def free_p(solver, arg):
  if is_free(arg, solver.env):
    yield cont, True

@builtin.macro('bound')
def bound(solver, var):
  assert(isinstance(var, Var))
  if isinstance(var, ClosureVar): var = var.var
  yield cont, solver.env[var] is not var
  
@builtin.macro('bound_p', 'bound!')
def bound_p(solver, var):
  assert(isinstance(var, Var))
  if isinstance(var, ClosureVar): var = var.var
  if solver.env[var] is not var:
    yield cont, True
  
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
  yield cont, True
  for b, v in bindings:
    b[var] = v
  
@builtin.macro('isinteger', 'isinteger!')
def isinteger(solver, arg):
  yield cont, isinstance(getvalue(arg, env, {}), int)

@builtin.macro('isinteger_p', 'isinteger!')
def isinteger_p(solver, arg):
  if isinstance(getvalue(arg, env, {}), int): yield cont, True

@builtin.macro('isfloat', 'isfloat')
def isfloat(solver, arg):
  yield cont, isinstance(getvalue(arg, env, {}), float)

@builtin.macro('isfloat_p', 'isfloat!')
def isfloat_p(solver, arg):
  if isinstance(getvalue(arg, env, {}), float): yield cont, True

@builtin.macro('isnumber', 'isnumber')
def isnumber(solver, arg):
  yield cont, isinstance(getvalue(arg, env, {}), int) \
              or isinstance(getvalue(arg, env, {}), float)

@builtin.macro('isnumber_p', 'isnumber!')
def isnumber_p(solver, arg):
  if isinstance(getvalue(arg, env, {}), int) \
     or isinstance(getvalue(arg, env, {}), float): 
    yield cont, True

@builtin.function('istuple?')
def istuple(x):  return isinstance(x, tuple)

@builtin.function('islist?')
def islist(x): return isinstance(x, list)

@builtin.macro('iscons')
def iscons(solver, arg):
  if isinstance(getvalue(arg, env, {}), Cons): yield cont, True

@builtin.macro('iscons_p', 'iscons!')
def iscons_p(solver, arg):
  if isinstance(getvalue(arg, env, {}), Cons): yield cont, True

@builtin.function('cons_f', 'iscons?')
def is_cons(x): return isinstance(x, Cons)

@builtin.function('pycall')
def pycall(fun, *args):  
  return fun(*args)

@builtin.function('py_apply')
def py_apply(fun, args):  
  return fun(*args)

