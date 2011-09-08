from oad.term import Var
from oad import error

def unwrap_atom(obj):
  if isinstance(obj, Atom): return obj.name   
  error.throw_type_error('atom', obj)

def atomic(obj):
  return isinstance(obj, UEntity)

