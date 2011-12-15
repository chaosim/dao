from dao.base import is_subclass, classname

class Type:
  def __repr__(self):
    return "<type 'root'>"

root = Type()

class Atom(Type):
  def __init__(self): pass
    
  def __eq__(self, other):
    return isinstance(other, Atom)
  
  def __repr__(self): return 'atom'

atom = Atom()

class Dummy(Type):
  def __init__(self): pass
    
  def __eq__(self, other):
    return isinstance(other, Dummy)
  
  def __repr__(self): return 'dummy'

dummy = Dummy()


def is_sub_type(type1, type2):
  if is_subclass(type1, type2): return True
  else: return False
  
class Map(Type):
  def __init__(self, result_type):
    self.result_type = result_type
  def __eq__(self, other):
    return isinstance(other, self.__class__) and self.result_type==other.result_type
  def __repr__(self):
    return "%s(%s)"%(classname(self), self.result_type)

class Function(Map):
  def apply(self, args_types):
    return self.result_type

function = Function(atom)

class Macro(Map):
  def apply(self, args_types):
    return self.result_type


macro = Macro(root)

class UserFunction(Function):
  pass

class RecursiveUserFunction(Function):
  pass

recursive_user_function = RecursiveUserFunction(root)

class UserMacro(Macro):
  pass

class BuiltinFunction(Function):
  pass

builtin_function = BuiltinFunction(atom)

class BuiltinPredicate(Function):
  pass

builtin_predicate = BuiltinPredicate(atom)

class BuiltinMacro(Macro):
  pass

builtin_macro = BuiltinMacro(atom)

#logic Var, free var
class Var(Type):
  pass 

var = Var()

class Or(Type):
  def __init__(self, *types):
    self.types = types
  def __eq__(self, other):
    return isinstance(other, Or) and self.types==other.types

def make_or(type1, type2):
  if type1==type2: return type1
  if is_sub_type(type1, type2): return type2
  if is_sub_type(type2, type1): return type1
  return Or(type1, type2)

class And(Type):
  pass

