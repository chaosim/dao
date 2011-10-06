from oad import helper 
from oad import error
from oad.term import Var, unify, deref
import oad.term as term
from oad import builtin

# analysing and construction terms

@builtin.macro()
def getvalue(solver, cont, item):
  yield cont, term.getvalue(item, solver.env)
  
@builtin.macro()
def ground_value(solver, cont, item, default=None):
  v = term.getvalue(item, solver.env)
  if isinstance(v, Var): v = default #deref(default, solver.env)
  yield cont, v
  
@builtin.macro()
def setvalue(solver, cont, var, value):
  var = deref(var, solver.env)
  value = deref(value, solver.env)
  @mycont(cont)
  def setvalue_cont(value, solver):
    var = var.var
    if var in solver.env.bindings:
      v = solver.env.bindings[var]
      if isinstance(v, Var): var = v
    old = var.getvalue(solver.env)
    var.setvalue(value, solver.env)
    yield cont, True
    var.setvalue(old, solver.env)
  yield solver.cont(value, set_cont), True

@builtin.macro()
def copy_term(solver, cont, item, copy):
  for _ in unify(copy, term.copy(item, {}), solver.env):
    yield cont, True
    