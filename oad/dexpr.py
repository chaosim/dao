# -*- coding: utf-8 -*-

# What will happen when lisp meets prolog in python?

'''dao expression embeded in python'''

from oad import special

class Expression: 
  def __lt__(self, other): return  __lt__(self, other)
  def __le__(self, other): return  __le__(self, other)
  def __eq__(self, other): return  __eq__(self, other)
  def __ne__(self, other): return  __ne__(self, other)
  def __gt__(self, other): return  __gt__(self, other)
  def __ge__(self, other): return  __ge__(self, other)
##  def __getattr__(self, other): 
##    return  __getattr__(self, other)
  def __call__(self, *args, **kw): return  __call__(self, args, kw)
  def __getitem__(self, key): return  __getitem__(self, other)
  def __add__(self, other): return  __add__(self, other)
  def __sub__(self, other): return  __sub__(self, other)
  def __mul__(self, other): return  __mul__(self, other)
  def __floordiv__(self, other): return  __floordiv__(self, other)
  def __div__(self, other): return  __div__(self, other)
  def __truediv__(self, other): return  __truediv__(self, other)
  def __mod__(self, other): return  __mod__(self, other)
  def __pow__(self, other): return  __pow__(self, other)
  def __lshift__(self, other): return  __lshift__(self, other)
  def __rshift__(self, other): return  __rshift__(self, other)
  def __and__(self, other): return  __and__(self, other)
  def __xor__(self, other): return  __xor__(self, other) 
  def __or__(self, other): return  __or__(self, other)
##  def __iter__(self): return  __iter__(self)
  def __neg__(self): return  __neg__(self)
  def __pos__(self): return  __pos__(self)
  def __abs__(self): return  __abs__(self)
  def __invert__(self): return  __invert__(self)
  
from oad.term import Var, DummyVar

_var_cache = {}
def varcache(name, klass=Var):
  return _var_cache.setdefault(klass, {}).setdefault(name, klass(name))

class VarSymbol(Expression): 
  def __init__(self, name): 
    self.name = name
##  def __hash__(self): return hash(self.name)
# because this can not be done, so let({var:value}) can not be used.
# a better form can be used, see sampe.py.
##  def __eq__(self, other): 
##    return self.__class__==other.__class__ and self.name==other.name
  def ___parse___(self, parser): 
    return varcache(self.name, Var)
  def __repr__(self): return 'Symbol(%s)'%self.name

class DummyVarSymbol(VarSymbol): 
  def ___parse___(self, parser): 
    return varcache(self.name, DummyVar)
  def __repr__(self): return 'DummySymbol(%s)'%self.name

class Binary(Expression): 
  def __init__(self, x, y): 
    self.x, self.y = x, y
  def ___parse___(self, parser): 
    return self.operator(parser.parse(self.x), parser.parse(self.y))

class Unary(Expression): 
  def __init__(self, x): 
    self.x = x
  def ___parse___(self, parser): 
    return operator(parser.parse(self.x))

from oad.builtins import arith

class __lt__(Binary): 
  operator = arith.__lt__  
class __le__(Binary): 
  operator = arith.__le__  
class __eq__(Binary): 
  operator = arith.__eq__
  def ___parse_block___(self, parser):
    # x==y==z==value
    y = parser.parse(self.y)
    if isinstance(self.x, VarSymbol):
      return special.set(varcache(self.x.name), y)
    vars = get_eq_vars_chain(self.x)
    if vars: 
      return special.begin(*[special.set(varcache(x.name), y) for x in left])
    else: return arith.eq(parser.parse(self.x), y)

class __ne__(Binary): 
  operator = arith.__ne__  
class __gt__(Binary): 
  operator = arith.__gt__  
class __ge__(Binary): 
  operator = arith.__ge__  
##class __getattr__(Binary): operator = arith.__getattr__  
class __getitem__(Binary): 
  operator = arith.__getitem__
class __add__(Binary): 
  operator = arith.__add__  
class __sub__(Binary): 
  operator = arith.__sub__  
class __mul__(Binary): 
  operator = arith.__mul__  
class __floordiv__(Binary): 
  operator = arith.__floordiv__  
class __div__(Binary): 
  operator = arith.__div__  
class __truediv__(Binary): 
  operator = arith.__truediv__  
class __mod__(Binary): 
  operator = arith.__mod__  
class __pow__(Binary): 
  operator = arith.__pow__  
class __lshift__(Binary): 
  operator = arith.__lshift__
class __rshift__(Binary): 
  operator = arith.__rshift__  
class __and__(Binary): 
  operator = arith.__and__  
class __xor__(Binary): 
  operator = arith.__xor__  
class __or__(Binary): 
  operator = arith.__or__  
##class __iter__(Unary): operator = arith.__iter__ 
class __neg__(Unary):   
  def ___parse___(self, parser): 
    if isinstance(self.x, Unary) and isinstance(self.x.x, VarSymbol):
      x = varcache(self.x.x.name)
      return special.set(x, arith.sub(x, 1))
    else: return -parser.parse(self.x)
class __pos__(Unary): 
  def ___parse___(self, parser): 
    if isinstance(self.x, Unary) and isinstance(self.x.x, VarSymbol):
      x = varcache(self.x.x.name)
      return special.set(x, arith.add(x, 1))
    else: return parser.parse(self.x)
class __abs__(Unary): 
  operator = arith.__abs__  
class __invert__(Unary): 
  operator = arith.__invert__

class __call__(Expression):
  def __init__(self, caller, *args, **kwargs): 
    self.caller, self.args, self.kwargs = caller, args, kwargs
  def ___parse___(self, parser): 
    return self.caller(*parser.parse(self.args), **parser.parse(self.kwargs))
