from dao.base import is_subclass

class Type:
  def __repr__(self):
    return "<type 'root'>"

root_type = Type()

def is_sub_type(type1, type2):
  if is_subclass(type1, type2): return True
  else: return False
  
class Map(Type):
  def __init__(self, args_types, result_type):
    self.args_types, self.result_type = args_types, result_type
  def __eq__(self, other):
    return isinstance(other, self.__class__) and self.args_types==other.args_types \
           and self.result_type==other.result_type

class Function(Map):
  def apply(self, args_types):
    return self.result_type

function = Function([root_type], root_type)

class Macro(Map):
  pass

macro = Macro([root_type], root_type)

class UserFunction(Function):
  pass

class UserMacro(Macro):
  pass

class BuiltinFunction(Function):
  pass

builtin_function = BuiltinFunction([root_type], root_type)

class BuiltinPredicate(Function):
  pass

builtin_predicate = BuiltinPredicate([root_type], root_type)

class BuiltinMacro(Map):
  pass

builtin_macro = BuiltinMacro([root_type], root_type)

#var argument
class Unknown(Type):
  pass 

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

class Atom(Type):
  def __init__(self, type):
    self.type = type
    
  def __eq__(self, other):
    return isinstance(other, Atom) and self.type==other.type
  
  def __repr__(self): return repr(self.type)
    
int = Atom(int)
float = Atom(float)
str = Atom(str)
NoneType = Atom(type(None))

def py_type(t):
  return Atom(t)