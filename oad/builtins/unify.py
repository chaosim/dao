from oad import error
from oad import builtin
##from oad.term import SUCCESS

# comparison and unification of terms

@builtin.macro('=:=')
def unify(evaluator, var0, var1):
  var0.unify(var1, evaluator.trail)
  evaluator.value = SUCCESS

@builtin.macro()
def unify_with_occurs_check(evaluator, var0, var1):
  var0.unify(var1, evaluator.trail, occurs_check=True)
  evaluator.value = SUCCESS

@builtin.macro('=/=')
def notunify(evaluator, var0, var1):
  new_trail = evaluator.trail.branch()
  try: var0.unify(var1, evaluator.trail)
  except error.UnifyFail: 
    evaluator.value = SUCCESS
    return new_trail.revert_upto(evaluator.trail)
  raise error.UnifyFail()

#from oad.term import cmp_standard_order

#def standardCmpPred(fun, name):
  #@builtin.macro(name)
  #def pred(evaluator, var0, var1):
    #var0 = var0.deref(trail)
    #var1 = var1.deref(trail)
    #c = term.cmp_standard_order(var0, var1, trail)
    #if not fun(c): raise error.UnifyFail()
    #evaluator.value = SUCCESS
  #return pred

#seq = standardCmpPred(lambda c: c==0, 'seq')
#sne = standardCmpPred(lambda c: c!=0, 'sne')
#slt = standardCmpPred(lambda c: c==-1, 'slt')
#sle = standardCmpPred(lambda c: c!=1, 'sle')
#sgt = standardCmpPred(lambda c: c==1, 'sgt')
#sge = standardCmpPred(lambda c: c==-1, 'sge')
