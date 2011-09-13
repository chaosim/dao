from oad.term import Var, ClosureVar
from oad import builtin
from oad.solve import mycont

@builtin.macro()
def from_(solver, cont, module, var):
  if isinstance(var, ClosureVar): var = var.var
  @mycont(cont)
  def from_module_cont(module, solver): 
    yield cont, module[var]
  yield solver.cont(module, from_module_cont), module
