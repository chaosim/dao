from oad import builtin
import oad.term as term

# comparison and unification of terms

@builtin.macro('=:=')
def unify(evaluator, v0, v1):
  for x in term.unify(v0, v1, evaluator.env):
    yield x

@builtin.macro('=::=')
def unify_with_occurs_check(evaluator, v0, v1):
  for x in term.unify(v0, v1, evaluator.env, occurs_check=True):
    yield x

@builtin.macro('=/=')
def notunify(evaluator, var0, var1):
  for x in term.unify(var0, var1, evaluator.env):
    return
  else: yield True