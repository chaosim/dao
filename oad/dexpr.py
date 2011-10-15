# -*- coding: utf-8 -*-

# What will happen when lisp meets prolog in python?

'''dao expression embeded in python'''

from oad import special
from oad.pysyntax import DinpySyntaxError

class Expression: 
  def __lt__(self, other): return  lt(self, other)
  def __rlt__(self, other): return  lt(other, self)
  def __le__(self, other): return  le(self, other)
  def __rle__(self, other): return  le(other, self)
  def __eq__(self, other): return  eq(self, other)
  def __req__(self, other): return  eq(self, other)
  def __ne__(self, other): return  ne(self, other)
  def __rne__(self, other): return  ne(other, self)
  def __gt__(self, other): return  gt(self, other)
  def __rgt__(self, other): return  gt(other, self)
  def __ge__(self, other): return  ge(self, other)
  def __rge__(self, other): return  ge(other, self)
##  def __getattr__(self, other): 
##    return  getattr__(self, other)
  def __call__(self, *args, **kw): 
    return  call(self, args, kw)
  def __getitem__(self, other): return  getitem(self, other)
  def __add__(self, other): return  add(self, other)
  def __radd__(self, other): return  add(other, self)
  def __sub__(self, other): return  sub(self, other)
  def __rsub__(self, other): return  sub(other, self)
  def __mul__(self, other): return  mul(self, other)
  def __rmul__(self, other): return  mul(other, self)
  def __floordiv__(self, other): return  floordiv__(self, other)
  def __rfloordiv__(self, other): return  floordiv__(other, self)
  def __div__(self, other): return  div(self, other)
  def __rdiv__(self, other): return  div(other, self)
  def __truediv__(self, other): return  truediv(self, other)
  def __rtruediv__(self, other): return  truediv(other, self)
  def __mod__(self, other): return  mod(self, other)
  def __rmod__(self, other): return  mod(other, self)
  def __pow__(self, other): return  pow(self, other)
  def __rpow__(self, other): return  pow(other, self)
  def __lshift__(self, other): return  lshift(self, other)
  def __rlshift__(self, other): return  lshift(other, self)
  def __rshift__(self, other): return  rshift(self, other)
  def __rrshift__(self, other): return  rshift(other, self)
  def __and__(self, other): return  and_(self, other)
  def __rand__(self, other): return  and_(other, self)
  def __xor__(self, other): return  xor(self, other) 
  def __rxor__(self, other): return  xor(other, self) 
  def __or__(self, other): return  or_(self, other)
  def __ror__(self, other): return  or_(other, self)
##  def __iter__(self): return  iter(self)
  def __neg__(self): return  neg(self)
  def __pos__(self): return  pos(self)
  def __abs__(self): return  abs(self)
  def __invert__(self): return  invert(self)
  def __nonzero__(self): return False
  
from oad.term import Var, DummyVar

_var_cache = {}
def varcache(name, klass=Var):
  return _var_cache.setdefault(klass, {}).setdefault(name, klass(name))

class Builtin(Expression):
  def __init__(self, builtin): self.builtin = builtin
  def ___parse___(self, parser): return self.builtin
##  def __lt__(self, other): return  lt(self, other)
##  def __rlt__(self, other): return  lt(other, self)
##  def __le__(self, other): return  le(self, other)
##  def __rle__(self, other): return  le(other, self)
  def __eq__(self, other): return  eq(self, other)
  def __req__(self, other): return  eq(self, other)
  def __ne__(self, other): return  ne(self, other)
  def __rne__(self, other): return  ne(other, self)
##  def __gt__(self, other): return  gt(self, other)
##  def __rgt__(self, other): return  gt(other, self)
##  def __ge__(self, other): return  ge(self, other)
##  def __rge__(self, other): return  ge(other, self)
##  def __getattr__(self, other): 
##    return  getattr__(self, other)
##  def __call__(self, *args, **kw): return  call(self, args, kw)
##  def __getitem__(self, key): return  getitem(self, other)
##  def __add__(self, other): return  add(self, other)
##  def __radd__(self, other): return  add(other, self)
##  def __sub__(self, other): return  sub(self, other)
##  def __rsub__(self, other): return  sub(other, self)
##  def __mul__(self, other): return  mul(self, other)
##  def __rmul__(self, other): return  mul(other, self)
##  def __floordiv__(self, other): return  floordiv__(self, other)
##  def __rfloordiv__(self, other): return  floordiv__(other, self)
##  def __div__(self, other): return  div(self, other)
##  def __rdiv__(self, other): return  div(other, self)
##  def __truediv__(self, other): return  truediv(self, other)
##  def __rtruediv__(self, other): return  truediv(other, self)
##  def __mod__(self, other): return  mod(self, other)
##  def __rmod__(self, other): return  mod(other, self)
##  def __pow__(self, other): return  pow(self, other)
##  def __rpow__(self, other): return  pow(other, self)
##  def __lshift__(self, other): return  lshift(self, other)
  def __rlshift__(self, other): return  lshift(other, self)
  def __rshift__(self, other): return  rshift(self, other)
##  def __rrshift__(self, other): return  rshift(other, self)
##  def __and__(self, other): return  and_(self, other)
##  def __rand__(self, other): return  and_(other, self)
##  def __xor__(self, other): return  xor(self, other) 
##  def __rxor__(self, other): return  xor(other, self) 
##  def __or__(self, other): return  or_(self, other)
##  def __ror__(self, other): return  or_(other, self)
##  def __iter__(self): return  iter(self)
##  def __neg__(self): return  neg(self)
##  def __pos__(self): return  pos(self)
##  def __abs__(self): return  abs(self)
##  def __invert__(self): return  invert(self)
  def __nonzero__(self): return False
  def __repr__(self): return repr(self.builtin)
  
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
  def __repr__(self): return '%s(%s, %s)'%(self.operator, self.x, self.y)

class Unary(Expression): 
  def __init__(self, x): 
    self.x = x
  def ___parse___(self, parser): 
    return self.operator(parser.parse(self.x))
  def __repr__(self): return '%s(%s)'%(self.__class__.__name__, self.x)

from oad.builtins import arith

class lt(Binary): 
  operator = arith.lt  
class le(Binary): 
  operator = arith.le  
class eq(Binary): 
  operator = arith.eq

class ne(Binary): 
  operator = arith.ne  
class gt(Binary): 
  operator = arith.gt  
class ge(Binary): 
  operator = arith.ge  
##class getattr__(Binary): operator = arith.getattr__  
class getitem(Binary): 
  operator = arith.getitem
class add(Binary): 
  operator = arith.add  
class sub(Binary): 
  operator = arith.sub  
class mul(Binary): 
  operator = arith.mul  
class floordiv(Binary): 
  operator = arith.floordiv  
class div(Binary): 
  operator = arith.div  
class truediv(Binary): 
  operator = arith.truediv  
class mod(Binary): 
  operator = arith.mod  
class pow(Binary): 
  operator = arith.pow  

def get_assign_vars_chain(exp):
  if isinstance(exp, VarSymbol): return (varcache(exp.name),)
  if not isinstance(exp, lshift): raise DinpySyntaxError
  return get_assign_vars_chain(exp.x)+(varcache(exp.y.name),)
  
class lshift(Binary): 
  operator = arith.lshift
  def ___parse___(self, parser):
    # x << y << z << value
    y = parser.parse(self.y)
    if isinstance(self.x, VarSymbol):
      return special.set(varcache(self.x.name), y)
##    vars = None
    if isinstance(self.x, tuple) or isinstance(self.x, list):
##      for x in self.x:
##        if not(isinstance, VarSymbol): raise DinpySyntaxError
##      vars.append(varcache(x,name))
      # don't use [i.j] << (1,2), use put.i.j << (1,2)
      raise DinpySyntaxError
    else: 
      vars = get_assign_vars_chain(self.x)
    i = len(vars)-1
    set_stmts = [special.set(vars[i], y)]
    while i>0:
      i -= 1
      set_stmts.append(special.set(vars[i], vars[i+1]))
    return special.begin(*set_stmts)

class rshift(Binary): 
  operator = arith.rshift  
class and_(Binary): 
  operator = arith.and_  
class xor(Binary): 
  operator = arith.xor  
class or_(Binary): 
  operator = arith.or_  
##class iter__(Unary): operator = arith.iter 
class neg(Unary):   
  def ___parse___(self, parser): 
    if isinstance(self.x, Unary) and isinstance(self.x.x, VarSymbol):
      x = varcache(self.x.x.name)
      return special.set(x, arith.sub(x, 1))
    else: return arith.neg(parser.parse(self.x))
class pos(Unary): 
  def ___parse___(self, parser): 
    if isinstance(self.x, Unary) and isinstance(self.x.x, VarSymbol):
      x = varcache(self.x.x.name)
      return special.set(x, arith.add(x, 1))
    else: return parser.parse(self.x)
class abs(Unary): 
  operator = arith.abs  
class invert__(Unary): 
  operator = arith.invert

class call(Expression):
  def __init__(self, caller, args, kwargs): 
    self.caller, self.args, self.kwargs = caller, args, kwargs
  def ___parse___(self, parser):
    caller = parser.parse(self.caller)
    return caller(*parser.parse(self.args), **parser.parse(self.kwargs))
