def _import_builtins():
  from oad import builtin
  from oad.builtins.matcher import Matcher
  from oad.dinpy.dexpr import _Builtin
  globls = globals()
  for name in globls: 
    obj = globls[name]
    if not isinstance(obj, builtin.Builtin): del globls[name]
    if isinstance(obj, Matcher): continue
    else: globls[name] = _Builtin(obj)
_import_builtins()

##def _import_builtins():
##  from oad import builtin
##  import oad.builtins
##  from oad.builtins.matcher import Matcher
##  from oad.dexpr import _Builtin
##  globls = globals()
##  for mo_name in oad.builtins.__dict__:
##    mo = oad.builtins.__dict__[mo_name]
##    if type(mo)!=type(builtin): continue # skip non module
##    for name in dir(mo): 
##      obj = mo.__dict__[name]
##      if not isinstance(obj, builtin.Builtin): del mo.__dict__[name]
##      if isinstance(obj, Matcher): continue
##      else:mo.__dict__[name] = _Builtin(obj)
##_import_builtins()
##
##__all__ = globals().keys()+['_']
##
##globls = globals()
##for x in globals(): print x, globls[x], type(globls[x])
##print arith.abs, type(arith.abs), arith.abs.__class__
##print globals()['_']
##print _
##pass
