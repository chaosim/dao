from oad.term import Var#, atom, Integer, Float, Number, Atom, UEntity
from oad import error

def unwrap_atom(obj):
  if isinstance(obj, Atom): return obj.name   
  error.throw_type_error('atom', obj)

def unwrap_number(obj):
  if isinstance(obj, Number): return obj.val   
  error.throw_type_error('number', obj)

def unwrap_int(obj):
  if isinstance(obj, Integer): return obj.val   
  error.throw_type_error('intger', obj)

def atomic(obj):
  return isinstance(obj, UEntity)

