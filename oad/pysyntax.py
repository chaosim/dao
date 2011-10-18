# -*- coding: utf-8 -*-

''' 
some tools that help you define operator grammars to preparse python expression.
see dinpy.py for a real sample.

>>> from oad.solve import set_run_mode, noninteractive
>>> set_run_mode(noninteractive)
>>> from oad.term import Var
>>> from oad.builtins.terminal import eos
>>> from oad.builtins.term import pytuple, first
>>> bindings, body = Var('bindings'), Var('body')
>>> do = word('do')
>>> let = element('let', call(bindings)+do+getitem(body)+eos+pytuple(first(bindings), body))
>>> preparse(let({'x':1}).do[1,2])
({'x': 1}, (1, 2))
'''

__all__ = ['element', 'preparse', 'lead',  
  'lt', 'le', 'eq', 'ne', 'gt', 'ge', 'bitor', 'xor', 'bitand', 
  'lshift', 'rshift', 'add', 'sub', 'mul', 'div', 'floordiv', 'mod',
  'pos', 'neg', 'invert', 'abs', 'pow', 
  'getattr', 'call', 'getitem', 'iterator', 
  'attr_item', 'attr_call', 'word', 'words', 'getitem_to_list',
  'DinpySyntaxError']

from oad.solve import run_mode, interactive
from oad.solve import interactive_solver, interactive_tagger, interactive_parser
from oad.term import deref, unify, DummyVar
from oad.solve import eval, preparse
from oad import special
from oad.builtins.matcher import matcher
from oad.builtins.parser import parse as dao_parse
from oad.builtins.term import to_list
from oad.builtins.control import and_p

##from oad.dinpy.dexpr import DinpySyntaxError

class DinpySyntaxError(Exception): pass

def element(name, grammar):
  ''' name = element(grammar)'''
  return _lead_element_class(FormTraveller)(name, preparse(grammar))

def lead(klass): 
  '''
# use case:
# var.a.b.c
class VarForm(object):
  def __init__(self):
    self.__vars__ = []
  def __getattr__(self, var):
    self.__vars__.append(Var(var))
    return self
  def __len__(self): return len(self.__vars__)
  def __iter__(self): return iter(self.__vars__)
var = lead(VarForm)
'''
  return lead_class(klass)()

def lead_class(klass):
  attrs = {}
  for a, value in klass.__dict__.items():
    if not a.startswith('__init__') and isinstance(value, type(lead_class)): #type(lead): function type
##      attrs[a] = lambda self, *args, **kw: value(klass(), *args, **kw) # why error?
      attrs[a] = _lead_function(klass, value)
    else: attrs[a] = value
  return type('Lead'+klass.__name__, klass.__bases__, attrs)

def _lead_function(klass, function):
  return lambda self, *args, **kw: function(
    klass(self.__form_name__, self.__form_grammar__), *args, **kw)

def _lead_element_function(klass, function):
  return lambda self, *args, **kw: function(
    klass(self.__form_name__, self.__form_grammar__), *args, **kw)

def _lead_element_class(klass):
  attrs = {}
  for a, value in klass.__dict__.items():
    if not a.startswith('__init__') and isinstance(value, type(lead_class)): #type(lead): function type
##      attrs[a] = lambda self, *args, **kw: value(klass(), *args, **kw) # why error?
      attrs[a] = _lead_element_function(klass, value)
    else: attrs[a] = value
  return type('Lead'+klass.__name__, klass.__bases__, attrs)

(__lt__, __le__, __eq__, __ne__, __gt__, __ge__, 
__getattr__, __call__, __getitem__, __iter__, 
__add__, __sub__, __mul__, __floordiv__, __div__, __truediv__, 
__mod__, __pow__, __lshift__, __rshift__, __and__, __xor__, __or__, 
__neg__, __pos__, __abs__, __invert__) = range(27)
names = (
'__lt__, __le__, __eq__, __ne__, __gt__, __ge__, '
'__getattr__, __call__, __getitem__, __iter__, '
'__add__, __sub__, __mul__, __floordiv__, __div__, __truediv__, '
'__mod__, __pow__, __lshift__, __rshift__, __and__, __xor__, __or__, '
'__neg__, __pos__, __abs__, __invert__'.split(', '))
class FormTraveller(object):
  def __init__(self, name, grammar):
    self.__form_name__ = name
    self.__form_grammar__ = preparse(grammar)
    self.__operator_data__ = []
  def __lt__(self, other): 
    self.__operator_data__.append((__lt__, other)); return self
  def __le__(self, other): 
    self.__operator_data__.append((__le__, other)); return self 
  def __eq__(self, other): 
    self.__operator_data__.append((__eq__, other)); return self 
  def __ne__(self, other): 
    self.__operator_data__.append((__ne__, other)); return self 
  def __gt__(self, other): 
    self.__operator_data__.append((__gt__, other)); return self 
  def __ge__(self, other): 
    self.__operator_data__.append((__ge__, other)); return self 
  def __getattr__(self, name):
##    print name
    self.__operator_data__.append((__getattr__, name)); return self 
  def __call__(self, *args, **kwargs): 
    self.__operator_data__.append((__call__, args, kwargs)); 
    return self 
  def __getitem__(self, key): 
    self.__operator_data__.append((__getitem__, key)); return self 
  def __add__(self, other): 
    self.__operator_data__.append((__add__, other)); return self 
  def __sub__(self, other): 
    self.__operator_data__.append((__sub__, other)); return self 
  def __mul__(self, other): 
    self.__operator_data__.append((__mul__, other)); return self 
  def __floordiv__(self, other): 
    self.__operator_data__.append((__floordiv__, other)); return self 
  def __div__(self, other): 
    self.__operator_data__.append((__div__, other)); 
    return self 
  def __truediv__(self, other): 
    self.__operator_data__.append((__lt__, other)); return self 
  def __mod__(self, other): 
    self.__operator_data__.append((__mod__, other)); 
##    print other
    return self 
  def __pow__(self, other): 
    self.__operator_data__.append((__pow__, other)); return self 
  def __lshift__(self, other): 
    self.__operator_data__.append((__lshift__, other)); return self 
  def __rshift__(self, other): 
    self.__operator_data__.append((__rshift__, other)); return self 
  def __and__(self, other): 
    self.__operator_data__.append((__and__, other)); return self 
  def __xor__(self, other): 
    self.__operator_data__.append((__xor__, other)); 
    return self 
  def __or__(self, other): 
    self.__operator_data__.append((__or__, other)); return self 
  def __iter__(self): 
    self.__operator_data__.append(__iter__); return self 
  def __neg__(self): 
    self.__operator_data__.append(__neg__); return self 
  def __pos__(self): 
    self.__operator_data__.append(__pos__); return self
  def __abs__(self): 
    self.__operator_data__.append(__abs__); return self 
  def __invert__(self): 
    self.__operator_data__.append(__invert__); return self 
  def ___parse___(self, parser):
    return eval(dao_parse(self.__form_grammar__, self.__operator_data__))
  def __nonzero__(self): return False
  
  # prevent __getattr__
  def closure(self, env): return self
  def deref(self, env): return self
  def getvalue(self, env): return self
  
  def __repr__(self): 
    if run_mode() is interactive:
      code = interactive_parser().parse(self)
      code = interactive_tagger().tag_loop_label(code)
      result = interactive_solver().eval(code)
      return repr(result) if result is not None else ''
    result = self.__form_name__
    for x in self.__operator_data__:
      if x== __neg__: result = '-%s'%result
      elif x== __pos__: result = '+%s'%result
      elif x== __abs__: result = 'abs(%s)'%result
      elif x== __invert__: result = '~%s'%result
      elif x[0]==__lt__: result += '<%s'%str(x[1])
      elif x[0]==__le__: result += '<=%s'%str(x[1])
      elif x[0]==__eq__: result += '==%s'%str(x[1])
      elif x[0]== __ne__: result += '!=%s'%str(x[1])
      elif x[0]== __gt__: result += '>%s'%str(x[1])
      elif x[0]== __ge__: result += '>=%s'%str(x[1])
      elif x[0]== __getattr__: result += '.%s'%str(x[1])
      elif x[0]== __call__: result += '(%s)'%str(x[1])
      elif x[0]== __getitem__: result += '[%s]'%str(x[1])
      elif x[0]==  __iter__: result = 'iter(%s)'%result
      elif x[0]== __add__: result += '+%s'%str(x[1])
      elif x[0]== __sub__: result += '-%s'%str(x[1])
      elif x[0]== __mul__: result += '*%s'%str(x[1])
      elif x[0]== __floordiv__: result += '//%s'%str(x[1])
      elif x[0]== __div__: result += '/%s'%str(x[1])
      elif x[0]== __truediv__: result += '/%s'%str(x[1])
      elif x[0]== __mod__: result += '%%%s'%str(x[1])
      elif x[0]== __pow__: result += '**%s'%str(x[1])
      elif x[0]== __lshift__: result += '<<%s'%str(x[1])
      elif x[0]== __rshift__: result += '>>%s'%str(x[1])
      elif x[0]==  __and__: result += '&%s'%str(x[1])
      elif x[0]== __xor__: result += '^%s'%str(x[1])
      elif x[0]== __or__: result += '|%s'%str(x[1])
    return result


def binary(attr):
  @matcher(names[attr])
  def func(solver, cont, argument=None): 
    argument = deref(argument, solver.env)
    syntax_result, pos = solver.stream
##    print  syntax_result, syntax_result[pos][1]
    if pos==len(syntax_result): return
    try: 
      if syntax_result[pos][0]!=attr: return
    except: return
    if argument is not None:
      for _ in unify(argument, syntax_result[pos][1], solver.env):
        solver.stream = syntax_result, pos+1
        yield cont,  True
    else: 
      solver.stream = syntax_result, pos+1
      yield cont, True
    solver.stream = syntax_result, pos
  return func

@matcher('__call__')
def call(solver, cont, args=None, kwargs=None): 
  args = deref(args, solver.env)
  kwargs = deref(kwargs, solver.env)
  syntax_result, pos = solver.stream
  if pos==len(syntax_result): return
  try: 
    if syntax_result[pos][0]!=__call__: return
  except: return
  if args is not None:
    for _ in unify(args, syntax_result[pos][1], solver.env):
      if kwargs is not None:
        for _ in unify(kwargs, syntax_result[pos][2], solver.env):
          solver.stream = syntax_result, pos+1
          yield cont,  True
      else: 
        solver.stream = syntax_result, pos+1
        yield cont, True
  else: 
    solver.stream = syntax_result, pos+1
    yield cont, True
  solver.stream = syntax_result, pos

def unary(attr):
  @matcher(names[attr])
  def func(solver, cont): 
    syntax_result, pos = solver.stream
    if pos==len(syntax_result): return
    if syntax_result[pos]!=attr: return
    solver.stream = syntax_result, pos+1
    yield cont,  True
    solver.stream = syntax_result, pos
  return func

'''
lambda	Lambda expression
or	Boolean OR
and	Boolean AND
not x	Boolean NOT
in, not in	Membership tests
is, is not	Identity tests
<, <=, >, >=, <>, !=, ==	Comparisons
|	Bitwise OR
^	Bitwise XOR
&	Bitwise AND
<<, >>	Shifts
+, -	Addition and subtraction
*, /, %	Multiplication, division, remainder
+x, -x	Positive, negative
~x	Bitwise not
**	Exponentiation
x.attribute	Attribute reference
x[index]	Subscription
x[index:index]	Slicing
f(arguments...)	Function call
(expressions...)	Binding or tuple display
[expressions...]	List display
{key:datum...}	Dictionary display
`expressions...`	String conversion
'''

lt = binary(__lt__) # <
le = binary(__le__) # <=
eq = binary(__eq__) # ==  
ne = binary(__ne__) # !=, <>
gt = binary(__gt__) # >
ge = binary(__ge__) # >=
bitor = binary(__or__)  # |
xor = binary(__xor__) # ^
bitand = binary(__and__) # &
lshift = binary(__lshift__) # <<
rshift = binary(__rshift__) # >>
add = binary(__add__) # +
sub = binary(__sub__) # -
mul = binary(__mul__) # *
div = binary(__div__) # /
floordiv = binary(__floordiv__) # //
mod = binary(__mod__) # %
pos = unary(__pos__)() # +x, negative 
neg = unary(__neg__)() # -x, Positive
invert = unary(__invert__)() # ~x	Bitwise not
abs = unary(__abs__)() # abs()
pow = binary(__pow__) # **	Exponentiation
getattr = binary(__getattr__) #  attribute access
getitem = binary(__getitem__) # object[index]
iterator = unary(__iter__)

def word(word): return getattr(word)
def words(text): return [getattr(w.strip()) for w in text.split(',')] 

def attr_item(name): return lambda arg: and_p(getattr(name),getitem(arg))

def attr_call(name): return lambda *args: and_p(getattr(name), call(*args))

def getitem_to_list(argument=None):
  if argument is not None:
    _x = DummyVar('_x')
    return and_p(getitem(_x), special.set(argument, to_list(_x)))
  else: return getitem()

if __name__ == "__main__":
  import doctest
  doctest.testmod()
