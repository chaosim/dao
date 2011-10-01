from oad import builtin
from oad.term import deref, unify
from oad.solve import eval
from oad.builtins.parser import parse as dao_parse

def lead_function(klass, function):
  return lambda self, *args, **kw: function(klass(), *args, **kw)

def lead_class(klass):
  attrs = {}
  for a, value in klass.__dict__.items():
    if not a.startswith('__init__') and isinstance(value, type(lead_class)): #type(lead): function type
##      attrs[a] = lambda self, *args, **kw: value(klass(), *args, **kw) # why error?
      attrs[a] = lead_function(klass, value)
    else: attrs[a] = value
  return type('Lead'+klass.__name__, klass.__bases__, attrs)

def lead(klass): return lead_class(klass)()

def lead_element_function(klass, function):
  return lambda self, *args, **kw: function(klass(self.__syntax_grammar__), *args, **kw)

def lead_element_class(klass):
  attrs = {}
  for a, value in klass.__dict__.items():
    if not a.startswith('__init__') and isinstance(value, type(lead_class)): #type(lead): function type
##      attrs[a] = lambda self, *args, **kw: value(klass(), *args, **kw) # why error?
      attrs[a] = lead_element_function(klass, value)
    else: attrs[a] = value
  return type('Lead'+klass.__name__, klass.__bases__, attrs)

def element(grammar): return lead_element_class(FormTraveller)(grammar)

def parse(form):
  try: 
    form_parse = form.__parse_syntax__
  except: 
    if isinstance(form, list):
      return [parse(x) for x in form]
    elif isinstance(form, tuple):
      return tuple(parse(x) for x in form)
    else: return form
  return form_parse()

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
  def __init__(self, grammar=None):
    self.__syntax_grammar__ = grammar
    self.__syntax_data__ = []
  def __lt__(self, other): 
    self.__syntax_data__.append((__lt__, other)); return self
  def __le__(self, other): 
    self.__syntax_data__.append((__le__, other)); return self 
  def __eq__(self, other): 
    self.__syntax_data__.append((__eq__, other)); return self 
  def __ne__(self, other): 
    self.__syntax_data__.append((__ne__, other)); return self 
  def __gt__(self, other): 
    self.__syntax_data__.append((__gt__, other)); return self 
  def __ge__(self, other): 
    self.__syntax_data__.append((__ge__, other)); return self 
  def __getattr__(self, name):
    self.__syntax_data__.append((__getattr__, name)); return self 
  def __call__(self, *args, **kw): 
    self.__syntax_data__.append((__call__, args, kw)); 
    return self 
  def __getitem__(self, key): 
    self.__syntax_data__.append((__getitem__, key)); return self 
  def __add__(self, other): 
    self.__syntax_data__.append((__add__, other)); return self 
  def __sub__(self, other): 
    self.__syntax_data__.append((__sub__, other)); return self 
  def __mul__(self, other): 
    self.__syntax_data__.append((__mul__, other)); return self 
  def __floordiv__(self, other): 
    self.__syntax_data__.append((__floordiv__, other)); return self 
  def __div__(self, other): 
    self.__syntax_data__.append((__div__, other)); return self 
  def __truediv__(self, other): 
    self.__syntax_data__.append((__lt__, other)); return self 
  def __mod__(self, other): 
    self.__syntax_data__.append((__mod__, other)); return self 
  def __pow__(self, other): 
    self.__syntax_data__.append((__pow__, other)); return self 
  def __lshift__(self, other): 
    self.__syntax_data__.append((__rshift__, other)); return self 
  def __rshift__(self, other): 
    self.__syntax_data__.append((__and__, other)); return self 
  def __and__(self, other): 
    self.__syntax_data__.append((__and__, other)); return self 
  def __xor__(self, other): 
    self.__syntax_data__.append((__xor__, other)); 
    return self 
  def __or__(self, other): 
    self.__syntax_data__.append((__or__, other)); return self 
  def __iter__(self): 
    self.__syntax_data__.append(__iter__); return self 
  def __neg__(self): 
    self.__syntax_data__.append(__neg__); return self 
  def __pos__(self): 
    self.__syntax_data__.append(__pos__); return self
  def __abs__(self): 
    self.__syntax_data__.append(__abs__); return self 
  def __invert__(self): 
    self.__syntax_data__.append(__invert__); return self 
  def __parse_syntax__(self):
    return eval(dao_parse(self.__syntax_grammar__, self.__syntax_data__))
  def __nonzero__(self): return False
  def __repr__(self): return self.__class__.__name__
  def __str__(self): return self.__class__.__name__

def binary(attr):
  @builtin.macro(names[attr])
  def func(solver, cont, argument=None): 
    argument = deref(argument, solver.env)
    syntax_result, pos = solver.stream
    if pos==len(syntax_result): return
    if syntax_result[pos][0]!=attr: return
    if argument is not None:
      for _ in unify(argument, syntax_result[pos][1], solver.env):
        solver.stream = syntax_result, pos+1
        yield cont,  True
    else: 
      solver.stream = syntax_result, pos+1
      yield cont, True
  return func

def unary(attr):
  @builtin.macro(names[attr])
  def func(solver, cont): 
    syntax_result, pos = solver.stream
    if pos==len(syntax_result): return
    if syntax_result[pos]!=attr: return
    solver.stream = syntax_result, pos+1
    yield cont,  True
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
negation = unary(__neg__) # +x, Positive
positive = unary(__pos__) # -x, negative 
invert = unary(__invert__) # ~x	Bitwise not
abs = unary(__abs__) # abs()
pow = binary(__pow__) # **	Exponentiation
getattr = binary(__getattr__) #  attribute access
getitem = binary(__getitem__) # object[index]
iterator = unary(__iter__)

@builtin.macro('__call__')
def call(solver, cont, args=None, kwargs=None): 
  args = deref(args, solver.env)
  kwargs = deref(kwargs, solver.env)
  syntax_result, pos = solver.stream
  if pos==len(syntax_result): return
  if syntax_result[pos][0]!=__call__: return
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
def word(word): return getattr(word)

def block(name): return lambda arg: getattr(name)+getitem(arg)

def funcall(name): return lambda args, kw: getattr(name)&call(args, kw)
 