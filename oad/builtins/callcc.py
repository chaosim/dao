from oad import builtin
from oad.builtin import Builtin, Function
from oad.term import Apply

# call with current continuation

class ContinuationFunction(Builtin, Function):
  def apply(self, solver, values, cont):
    return self.function(values[0], solver)
      
@builtin.macro()
def callcc(solver, cont, fun):
   yield solver.cont(Apply(fun, ContinuationFunction(cont)), cont), fun

