from oad import builtin
import oad.term as term

# comparison and unification of terms

@builtin.macro('=:=')
def unify(solver, cont, v0, v1):
  for _ in term.unify(v0, v1, solver.env): yield cont, True

@builtin.macro('=::=')
def unify_with_occurs_check(solver, cont, v0, v1):
  for _ in term.unify(v0, v1, solver.env, occurs_check=True): yield cont, True

@builtin.macro('=/=')
def notunify(solver, cont, var0, var1):
  for _ in term.unify(var0, var1, solver.env): 
    return
  else: yield cont, True
  
  
  
  