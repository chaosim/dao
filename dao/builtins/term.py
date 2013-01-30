from dao.compilebase import CompileTypeError
from dao.command import special, Command, SpecialCall,NONE, Var, Cons
from dao.command import cps_convert_unify
import dao.interlang as il

# analysing and construction terms

@special
def unify(compiler, cont, x, y):
  return cps_convert_unify(x, y, compiler, cont)

@special
def notunify(compiler, cont, x, y):
  v = compiler.new_var(il.ConstLocalVar('v'))
  cont1 = il.clamda(v, il.failcont(il.FALSE))
  cont2 = il.clamda(v, cont(il.TRUE))
  return il.begin(il.SetFailCont(cont2),
                  cps_convert_unify(x, y, compiler, cont1))


@special
def eval_unify(compiler, cont, x, y):
  x1 = compiler.new_var(il.ConstLocalVar('x'))
  y1 = compiler.new_var(il.ConstLocalVar('y'))
  return x.cps_convert(compiler, il.clamda(x1, 
    y.cps_convert(compiler, il.clamda(y1,
      il.If(il.IsLogicVar(x1),
         il.begin(il.SetBinding(x1, y1),
                  il.append_failcont(compiler, il.DelBinding(x1)),
                  cont(il.TRUE)),
        il.If(il.IsLogicVar(y1),
              il.begin(il.SetBinding(y1, x1),
                       il.append_failcont(compiler, il.DelBinding(y1)),
                       cont(il.TRUE)),              
              il.If(il.Eq(x1, y1), cont(il.TRUE), il.failcont(il.TRUE))))))))

@special
def is_(compiler, cont, var, exp):
  var = il.LogicVar(var.name)
  v = compiler.new_var(il.ConstLocalVar('v'))
  return exp.cps_convert(compiler, il.clamda(v,
      il.SetBinding(var, v), cont(v)))

@special
def derefence(compiler, cont, item):
  return cont(il.Deref(item.interlang()))

@special
def getvalue(compiler, cont, item):
  if isinstance(item, Var) or isinstance(item, Cons):
    return cont(il.GetValue(item.interlang()))
  else:
    return cont(item.interlang())
 
@special
def getvalue_default(compiler, cont, item, default=None):
  if default is None: default = NONE
  v = compiler.new_var(il.ConstLocalVar('v'))
  return il.begin(
    il.Assign(v, il.GetValue(item.interlang())),
    il.If(il.IsLogicVar(v),
          default.cps_convert(compiler,cont),
          cont(v)))

@special
def isinteger(compiler, cont, item):
 return cont(il.Isinstance(item.interlang(), il.Symbol('int')))

@special
def isfloat(compiler, cont, item):
 return cont(il.Isinstance(item.interlang(), il.Symbol('float')))

@special
def isnumber(compiler, cont, item): 
  return cont(il.or_(il.Isinstance(item.interlang(), il.Symbol('int')),
                     il.Isinstance(item.interlang(), il.Symbol('float'))))
  
@special
def isstr(compiler, cont, item):
 return cont(il.Isinstance(item.interlang(), il.Symbol('str')))


@special
def istuple(compiler, cont, item):  
  return cont(il.Isinstance(item.interlang(), il.Symbol('tuple')))

@special
def islist(compiler, cont, item):
  return cont(il.Isinstance(item.interlang(), il.Symbol('list')))
  
@special
def isdict(compiler, cont, item): 
  return cont(il.Isinstance(item.interlang(), il.Symbol('dict')))
  
'''
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

# comparison and unification of terms

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