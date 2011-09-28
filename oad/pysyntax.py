from oad import builtin

(__lt__, __le__, __eq__, __ne__, __gt__, __ge__, 
__getattr__, __call__, __getitem__, __iter__, 
__add__, __sub__, __mul__, __floordiv__, __div__, __truediv__, 
__mod__, __pow__, __lshift__, __rshift__, __and__, __xor__, __or__, 
__neg__, __pos__, __abs__, __invert__) = range(27)

class Traveller:
  def __init__(self):
    self.result = []
  def __lt__(self, other): self.result.append((__lt__, other)); return self
  def __le__(self, other): self.result.append((__le__, other)); return self 
  def __eq__(self, other): self.result.append((__eq__, other)); return self 
  def __ne__(self, other): self.result.append((__ne__, other)); return self 
  def __gt__(self, other): self.result.append((__gt__, other)); return self 
  def __ge__(self, other): self.result.append((__ge__, other)); return self 
  def __getattr__(self, name): self.result.append((__getattr__, other)); return self 
  def __call__(self, *args, **kw): self.result.append((__call__, args, kw)); return self 
  def __getitem__(self, key): self.result.append((__getitem__, key)); return self 
  def __iter__(self): self.result.append((__iter__, other)); return self 
  def __add__(self, other): self.result.append((__add__, other)); return self 
  def __sub__(self, other): self.result.append((__sub__, other)); return self 
  def __mul__(self, other): self.result.append((__mul__, other)); return self 
  def __floordiv__(self, other): self.result.append((__floordiv__, other)); return self 
  def __div__(self, other): self.result.append((__div__, other)); return self 
  def __truediv__(self, other): self.result.append((__lt__, other)); return self 
  def __mod__(self, other): self.result.append((__mod__, other)); return self 
  def __pow__(self, other): self.result.append((__pow__, other)); return self 
  def __lshift__(self, other): self.result.append((__rshift__, other)); return self 
  def __rshift__(self, other): self.result.append((__and__, other)); return self 
  def __and__(self, other): self.result.append((__and__, other)); return self 
  def __xor__(self, other): self.result.append((__xor__, other)); return self 
  def __or__(self, other): self.result.append((__or__, other)); return self 
  def __neg__(self): self.result.append(__neg__); return self 
  def __pos__(self): self.result.append(__pos__); return self
  def __abs__(self): self.result.append(__abs__); return self 
  def __invert__(self): self.result.append(__invert__); return self 

def binary(attr):
  @builtin.macro()
  def attr(solver, cont, argument): 
    argument = deref(argument, solver.env)
    syntax_result, pos = solver.stream
    if pos==len(syntax_result): return
    if syntax_result[pos][0]!=attr: return
    for _ in unify(argument, syntax_result[pos][1], solver.env):
      solver.stream = syntax_result, pos+1
      yield cont,  True
  return func

def unary(attr):
  @builtin.macro()
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
neq = binary(__ne__) # !=, <>
gt = binary(__gt__) # >
ge = binary(__ge__) # >=
bitor = binary(__or__)  # |
bitxor = binary(__xor__) # ^
bitand = binary(__and__) # &
bitlshift = binary(__lshift__) # <<
bitrshift = binary(__rshift__) # >>
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
pow = binary(__pow___) # **	Exponentiation
getattr = binary(__getattr__) #  attribute access
getitem = binary(__getitem__) # object[index]
iter = unary(__iter__)

@builtin.macro()
def call(solver, cont, args, kw): 
  args = deref(args, solver.env)
  kw = deref(kw, solver.env)
  syntax_result, pos = solver.stream
  if pos==len(syntax_result): return
  if syntax_result[pos][0]!=__call__: return
  for _ in unify(argument, syntax_result[pos][1], solver.env):
    solver.stream = syntax_result, pos+1
    yield cont,  True
  return func

def dotword(word): return getattr(word)

def funcall(fun_name):
  return getattr(fun_name)+call
