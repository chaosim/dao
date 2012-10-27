def _import_builtins():
  from dao import builtin
  from dao.builtins.matcher import Matcher
  from dao.dinpy.dexpr import _BuiltinSymbol
  globls = globals()
  for name in globls: 
    obj = globls[name]
    if not isinstance(obj, builtin.Builtin): del globls[name]
    if isinstance(obj, Matcher): continue
    else: globls[name] = _BuiltinSymbol(obj)
_import_builtins()
