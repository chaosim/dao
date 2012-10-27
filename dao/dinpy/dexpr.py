# -*- coding: utf-8 -*-

# What will happen when lisp meets prolog in python?

'''dao expression embeded in python'''

from dao.builtins import arith
from dao import special
from dao.solve import run_mode, interactive, dao_repr, to_sexpression
from dao.solve import interactive_solver, interactive_tagger, interactive_parser

all = ['DinpySyntaxError']

class _SymbolExpression: 
  def __lt__(self, other): return  _lt(self, other)
  def __rlt__(self, other): return  _lt(other, self)
  def __le__(self, other): return  _le(self, other)
  def __rle__(self, other): return  _le(other, self)
  def __eq__(self, other): return  _eq(self, other)
  def __req__(self, other): return  _eq(self, other)
  def __ne__(self, other): return  _ne(self, other)
  def __rne__(self, other): return  _ne(other, self)
  def __gt__(self, other): return  _gt(self, other)
  def __rgt__(self, other): return  _gt(other, self)
  def __ge__(self, other): return  _ge(self, other)
  def __rge__(self, other): return  _ge(other, self)
##  def __getattr__(self, other): 
##    return  getattr__(self, other)
  def __call__(self, *args, **kw): 
    return  _call(self, args, kw)
  def __getitem__(self, other): return  _getitem(self, other)
  def __add__(self, other): return  _add(self, other)
  def __radd__(self, other): return  _add(other, self)
  def __sub__(self, other): return  _sub(self, other)
  def __rsub__(self, other): return  _sub(other, self)
  def __mul__(self, other): return  _mul(self, other)
  def __rmul__(self, other): return  _mul(other, self)
  def __floordiv__(self, other): return  _floordiv(self, other)
  def __rfloordiv__(self, other): return  _floordiv(other, self)
  def __div__(self, other): return  _div(self, other)
  def __rdiv__(self, other): return  _div(other, self)
  def __truediv__(self, other): return  _truediv(self, other)
  def __rtruediv__(self, other): return  _truediv(other, self)
  def __mod__(self, other): return  _mod(self, other)
  def __rmod__(self, other): return  _mod(other, self)
  def __pow__(self, other): return  _pow(self, other)
  def __rpow__(self, other): return  _pow(other, self)
  def __lshift__(self, other): return  _lshift(self, other)
  def __rlshift__(self, other): return  _lshift(other, self)
  def __rshift__(self, other): return  _rshift(self, other)
  def __rrshift__(self, other): return  _rshift(other, self)
  def __and__(self, other): return  _and(self, other)
  def __rand__(self, other): return  _and(other, self)
  def __xor__(self, other): return  _xor(self, other) 
  def __rxor__(self, other): return  _xor(other, self) 
  def __or__(self, other): return  _or(self, other)
  def __ror__(self, other): return  _or(other, self)
##  def __iter__(self): return  _iter(self)
  def __neg__(self): return  _neg(self)
  def __pos__(self): return  _pos(self)
  def __abs__(self): return  _abs(self)
  def __invert__(self): return  _invert(self)
  def __nonzero__(self): return False
  def __repr__(self): 
    if run_mode() is interactive:
      code = interactive_parser().parse(self)
      code = interactive_tagger().tag_loop_label(code)
      code = to_sexpression(code)
      result = interactive_solver().eval(code)
      return repr(result) if result is not None else ''
    else: return self.____repr____()
  
from dao.term import Var, DummyVar

_var_cache = {}
def varcache(name, klass=Var):
  return _var_cache.setdefault(klass, {}).setdefault(name, klass(name))

class _BuiltinSymbol(_SymbolExpression):
  def __init__(self, builtin): self.builtin = builtin
  def ___parse___(self, parser): return self.builtin
##  def __lt__(self, other): return  _lt(self, other)
##  def __rlt__(self, other): return  _lt(other, self)
##  def __le__(self, other): return  _le(self, other)
##  def __rle__(self, other): return  _le(other, self)
  def __eq__(self, other): return  _eq(self, other)
  def __req__(self, other): return  _eq(self, other)
  def __ne__(self, other): return  _ne(self, other)
  def __rne__(self, other): return  _ne(other, self)
##  def __gt__(self, other): return  _gt(self, other)
##  def __rgt__(self, other): return  _gt(other, self)
##  def __ge__(self, other): return  _ge(self, other)
##  def __rge__(self, other): return  _ge(other, self)
##  def __getattr__(self, other): 
##    return  getattr__(self, other)
##  def __call__(self, *args, **kw): return  _call(self, args, kw)
##  def __getitem__(self, key): return  _getitem(self, other)
##  def __add__(self, other): return  _add(self, other)
##  def __radd__(self, other): return  _add(other, self)
##  def __sub__(self, other): return  _sub(self, other)
##  def __rsub__(self, other): return  _sub(other, self)
##  def __mul__(self, other): return  _mul(self, other)
##  def __rmul__(self, other): return  _mul(other, self)
##  def __floordiv__(self, other): return  _floordiv(self, other)
##  def __rfloordiv__(self, other): return  _floordiv(other, self)
##  def __div__(self, other): return  _div(self, other)
##  def __rdiv__(self, other): return  _div(other, self)
##  def __truediv__(self, other): return  _truediv(self, other)
##  def __rtruediv__(self, other): return  _truediv(other, self)
##  def __mod__(self, other): return  _mod(self, other)
##  def __rmod__(self, other): return  _mod(other, self)
##  def __pow__(self, other): return  _pow(self, other)
##  def __rpow__(self, other): return  _pow(other, self)
##  def __lshift__(self, other): return  _lshift(self, other)
  def __rlshift__(self, other): return  _lshift(other, self)
##  def __rshift__(self, other): return  _rshift(self, other)
##  def __rrshift__(self, other): return  _rshift(other, self)
##  def __and__(self, other): return  _and(self, other)
##  def __rand__(self, other): return  _and(other, self)
##  def __xor__(self, other): return  _xor(self, other) 
##  def __rxor__(self, other): return  _xor(other, self) 
##  def __or__(self, other): return  _or(self, other)
##  def __ror__(self, other): return  _or(other, self)
##  def __iter__(self): return  _iter(self)
##  def __neg__(self): return  _neg(self)
##  def __pos__(self): return  _pos(self)
##  def __abs__(self): return  _abs(self)
##  def __invert__(self): return  _invert(self)
  def __nonzero__(self): return False
  def ____repr____(self): return repr(self.builtin)
  
def symbols(text):
  return tuple(_VarSymbol(x.strip()) for x in text.split(','))

##def symbols(text):
##  result = []
##  globls = globals()
##  for x in text.split(','):
##    x = x.strip()
##    symbol = globls[x] = _VarSymbol(x)
##    result.append(symbol)
##  return result

class _VarSymbol(_SymbolExpression): 
  def __init__(self, name): 
    self.name = name
  def ___parse___(self, parser): 
    return varcache(self.name, Var)
  def ____repr____(self): return 'Symbol(%s)'%self.name

def dummies(text):
  return tuple(_DummyVarSymbol(x.strip()) for x in text.split(','))

##def dummies(text):
##  result = []
##  globls = globals()
##  for x in text.split(','):
##    x = x.strip()
##    symbol = globls[x] = _DummyVarSymbol(x)
##    result.append(symbol)
##  return result
##

class _DummyVarSymbol(_VarSymbol): 
  def ___parse___(self, parser): 
    return varcache(self.name, DummyVar)
  def __repr__(self): return 'DummySymbol(%s)'%self.name

class _Binary(_SymbolExpression): 
  def __init__(self, x, y): 
    self.x, self.y = x, y
  def ___parse___(self, parser): 
    return self.operator(parser.parse(self.x), parser.parse(self.y))
  def ____repr____(self): 
    return '%s(%s, %s)'%(dao_repr(self.operator), dao_repr(self.x), dao_repr(self.y))
  __repr__ = ____repr____

class _Unary(_SymbolExpression): 
  def __init__(self, x): 
    self.x = x
  def ___parse___(self, parser): 
    return self.operator(parser.parse(self.x))
  def __repr__(self): 
    return '%s(%s)'%(dao_repr(self.operator), dao_repr(self.x))

class _lt(_Binary): 
  operator = arith.lt  
class _le(_Binary): 
  operator = arith.le  
class _eq(_Binary): 
  operator = arith.eq

class _ne(_Binary): 
  operator = arith.ne  
class _gt(_Binary): 
  operator = arith.gt  
class _ge(_Binary): 
  operator = arith.ge  
##class getattr__(_Binary): operator = arith.getattr__  
class _getitem(_Binary): 
  operator = arith.getitem
class _add(_Binary): 
  operator = arith.add  
class _sub(_Binary): 
  operator = arith.sub  
class _mul(_Binary): 
  operator = arith.mul  
class _floordiv(_Binary): 
  operator = arith.floordiv  
class _div(_Binary): 
  operator = arith.div  
class _truediv(_Binary): 
  operator = arith.truediv  
class _mod(_Binary): 
  operator = arith.mod  
class _pow(_Binary): 
  operator = arith.pow  

def _get_assign_vars_chain(exp):
  if isinstance(exp, _VarSymbol): return (varcache(exp.name),)
  elif isinstance(exp, _lshift): 
    return _get_assign_vars_chain(exp.x)+(varcache(exp.y.name),)
  else: raise DinpySyntaxError()  
def _get_assign_vars_list(exp):
  if isinstance(exp, _VarSymbol): return (varcache(exp.name),)
  elif isinstance(exp, _div): 
    return _get_assign_vars_list(exp.x)+(varcache(exp.y.name),)
  else: raise DinpySyntaxError()
  
class _lshift(_Binary): 
  operator = arith.lshift
  def ___parse___(self, parser):
    # x << y << z << value
    y = parser.parse(self.y)
    if isinstance(self.x, _VarSymbol):
      return special.set(varcache(self.x.name), y)
    if isinstance(self.x, _lshift): 
      vars = _get_assign_vars_chain(self.x)
      i = len(vars)-1
      set_stmts = [special.set(vars[i], y)]
      while i>0:
        i -= 1
        set_stmts.append(special.set(vars[i], vars[i+1]))
      return special.begin(*set_stmts)
    elif isinstance(self.x, tuple) or isinstance(self.x, list):
      vars = self.x
    elif isinstance(self.x, _div):
      vars = _get_assign_vars_list(self.x)
    else: raise DinpySyntaxError()
    return special.set_list(vars, y)
  def __repr__(self): 
    if run_mode() is interactive:
      code = interactive_parser().parse(self)
      code = interactive_tagger().tag_loop_label(code)
      code = to_sexpression(code)
      result = interactive_solver().eval(code)
      return repr(result) if result is not None else ''
    else: return self.____repr____()

class _rshift(_Binary): 
  operator = arith.rshift  
class _and(_Binary): 
  operator = arith.and_  
class _xor(_Binary): 
  operator = arith.xor  
class _or(_Binary): 
  operator = arith.or_  
##class iter__(_Unary): operator = arith.iter 
class _neg(_Unary):   
  def ___parse___(self, parser): 
    if isinstance(self.x, _Unary) and isinstance(self.x.x, _VarSymbol):
      x = varcache(self.x.x.name)
      return special.set(x, arith.sub(x, 1))
    else: return arith.neg(parser.parse(self.x))
class _pos(_Unary): 
  def ___parse___(self, parser): 
    if isinstance(self.x, _Unary) and isinstance(self.x.x, _VarSymbol):
      x = varcache(self.x.x.name)
      return special.set(x, arith.add(x, 1))
    else: return parser.parse(self.x)
class _abs(_Unary): 
  operator = arith.abs  
class invert__(_Unary): 
  operator = arith.invert

class _call(_SymbolExpression):
  def __init__(self, caller, args, kwargs): 
    self.caller, self.args, self.kwargs = caller, args, kwargs
  def ___parse___(self, parser):
    caller = parser.parse(self.caller)
    return caller(*parser.parse(self.args), **parser.parse(self.kwargs))
  def ____repr____(self): 
    return '%s(%s,%s)'%(','.join([dao_repr(a) for a in self.args]),
                        ', '+','.join(['%s=%s' %(dao_reprk, dao_repr(a)) 
                                       for k, a in self.kwargs]) 
                            if self.kwargs else '')
