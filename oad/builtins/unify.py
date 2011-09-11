from oad import builtin
import oad.term as term

# comparison and unification of terms

@builtin.macro('=:=')
def unify(solver, v0, v1):
  for x in term.unify(v0, v1, solver.env):
    yield x

@builtin.macro('=::=')
def unify_with_occurs_check(solver, v0, v1):
  for x in term.unify(v0, v1, solver.env, occurs_check=True):
    yield x

@builtin.macro('=/=')
def notunify(solver, var0, var1):
  for x in term.unify(var0, var1, solver.env):
    return
  else: yield True