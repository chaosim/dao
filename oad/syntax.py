# -*- coding: utf-8 -*-

class PyMetaSyntaxError : pass

toself = 'to self'

class Object(object):
  def __getattr__(self, attr):
    try: return self.__dict__[attr]
    except: return self.getattr(attr)
  
class SyntaxObject(object):
  def __getattr__(self, attr):
    try: return self.__dict__[attr]
    except: 
      if attr in self.dotwords: return self.dotwords[attr]()
      return self.getattr(attr)

def lead_function(klass, function):
  def fun(self, data, *args, **kw):
    new  = klass()
    return function(new, data, *args, **kw)
  return fun

def lead(klass):
  if isinstance(klass, SyntaxKlass): klass = klass.get_class()
  attrs = {}
  for a, v in klass.__dict__.items():
    if not a.startswith('__init__') and isinstance(v, type(lead)):
      attrs[a] = lead_function(klass, v)
    else: attrs[a] = v
  return type('Lead'+klass.__name__, klass.__bases__, attrs)()

class SyntaxMetaClass(type):
  def __init__(cls, name, bases, attrs):
    super(SyntaxMetaClass, cls).__init__(name, bases, attrs)
    def __init__(self, *args):
      self.__data__ = {}
##      self.__init_syntax_instance__(*args)
    cls.__init__ = __init__
      
class Element:
  def __call__(self, *functions):
    result = self.copy()
    result.functions = result.functions+functions
    return result
    
class Init(Element):
  def __init__(self):
    self.functions = ()
    def __init__(self1):
      result = self1.__data__
      for fun in self.functions: result = fun(result)
      self1.__data__ = result
    self.init_method = __init__
  def copy(self): return Init()
  def __repr__(self): return 'init'
init  = Init()

class Attribute(Element):
  def __init__(self, name):
    self.functions = ()
    self.name = name
  def copy(self): 
    result = self.__class__(self.name)
    result.functions = self.functions[:]
    return result
  def make_method(attr_self):
    def method(syntax_object_self, *args, **kw):
      data = syntax_object_self.__data__
      for fun in attr_self.functions: 
        data = fun.run(data, *args, **kw)
      try: 
        result_class = attr_self.__result_class__
        if result_class is None: return data
        elif result_class is toself: syntax_result = syntax_object_self
        else: syntax_result = result_class()
        syntax_result.__data__ = data
      except:
        try: 
          result_class = syntax_object_self.__result_class__
          if result_class is None: return data
          elif result_class is toself: syntax_result = syntax_object_self
          else: syntax_result = result_class()
          syntax_result.__data__ = data
        except: return data
      return syntax_result
    return method
  def __rshift__(self, other):
    self.__result_class__ = other
    return self
  def __repr__(self): return self.name
  
class Binary(Attribute): pass
binary = Binary

class Unary(Attribute): pass
unary = Unary

class Fun(Attribute): pass
fun = Fun

class DotWord(Unary): pass
dotword = DotWord

def dotwords(text):
  return [dotword(x) for x in text.split(', ')]

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

__lt__ = binary('__lt__') # <
__le__ = binary('__le__') # <=
__eq__ = binary('__eq__') # ==  
__ne__ = binary('__ne__') # !=, <>
__gt__ = binary('__gt__') # >
__ge__ = binary('__ge__') # >=
__or__ = binary('__or__')  # |
__xor__ = binary('__xor__') # ^
__and__ = binary('__and__') # &
__lshift__ = binary('__lshift__') # <<
__rshift__ = binary('__rshift__') # >>
__add__ = binary('__add__') # +
__sub__ = binary('__sub__') # -
__mul__ = binary('__mul__') # *
__div__ = binary('__div__') # /
__floordiv__ = binary('__floordiv__') # //
__mod__ = binary('__mod__') # %
__neg__ = unary('__neg__') # +x, Positive
__pos__ = unary('__pos__') # -x, negative 
__invert__ = unary('__invert__') # ~x	Bitwise not
__abs__ = unary('__abs__') # abs()
__pow__ = binary('__pow___') # **	Exponentiation
__getattr__ = binary('getattr') #   def attr
__getitem__ = binary('__getitem__') # object[index]
__call__ = binary('__call__') # fun(*args)
__iter__ = unary('__iter__')

class SyntaxKlass:
  class_schemes = []
  class_dict = {}
  __result_class__ = None
  def __init__(self, name):
    self.name = name
    self.bases = (SyntaxObject,)
  def __getitem__(self, *attrs):
    if isinstance(attrs[0], tuple): self.attrs = attrs[0]
    else: self.attrs = attrs
    return self
  def __rshift__(self, other):
    self.__result_class__ = other
    return self
  def get_class(self):
    try: return self.class_dict[self.name]
    except KeyError: 
      attrs = {}
      dotwords = {}
      for item in self.attrs:
        if isinstance(item, Init): 
          attrs['__init_syntax_instance__'] = item.init_method
        else:
          if isinstance(item, DotWord): dotwords[item.name] = item.make_method()
          elif isinstance(item, Attribute): attrs[item.name] = item.make_method()
          elif isinstance(item, Fun): attrs[item.name] = item.make_method()
##        if '__init_syntax_instance__' not in attrs:
##          def f(*args): return
##          attrs['__init_syntax_instance__'] = f
      klass = SyntaxMetaClass(self.name, self.bases, attrs)
      klass.dotwords = dotwords
      klass.__result_class__ = self.__result_class__
      self.class_dict[self.name] = klass
      return self.class_dict[self.name]

  def __call__(self, *args):
    return self.get_class()(*args)
  
  @classmethod
  def make_all_classes(self):
    for klass in class_schemes:
      klass.make_class()
  
def syntax_klass(name): return SyntaxKlass(name)

def syntax_klasses(names): return [syntax_klass(name) for name in names.split(', ')]

__syntax__data__ = {} # global data which is used while parsing

def run(item, data, *args, **kw):
  try:
    return item.run(data, *args, **kw)
  except:
    if isinstance(item, list):
      return [run(i, data, *args, **kw) for i in item]
    elif isinstance(item, tuple):
      return tuple(run(i, data, *args, **kw) for i in item)
    else: return item

class OperatorFunction(Object):
  def __init__(self, *args): 
    self.args = args
  def run(self, data, *args, **kw):
    raise NotImplementedError
  def __repr__(self): return '<<%s>>'%self.__class__.__name__
  def __str__(self): return '<<%s>>'%self.__class__.__name__

class ErrorFunction(OperatorFunction):
  def run(self, data, *args, **kw):
    raise PyMetaSyntaxError
error = ErrorFunction()

# have.a, hava.a.b.c, get.a, append.a, add.a, args/1, args

see = lead(OperatorFunction)

test = lead(OperatorFunction)

class ArgFunction(OperatorFunction):
  def run(self, data, *args, **kw):
    return args[0]  
arg0 = arg = ArgFunction()

class ArgsFunction(OperatorFunction):
  def run(self, data, *args, **kw):
    return args 
args = ArgsFunction()

class DataFunction(OperatorFunction):
  def run(self, data, *args, **kw):
    return data 
data  = syntax_klass('data')
data[__getattr__(arg)]
data = lead(data)

class make(OperatorFunction):
  def __init__(self, klass, *args):
    self.klass, self.args = klass, args
  def run(self, data, *args, **kw):
    klass_args = [run(arg, data, *args, **kw) for arg in self.args]
    return self.klass(*klass_args)

class GetFunction(OperatorFunction):
  def __init__(self, var):
    self.var = var
  def run(self, data, *args, **kw):
    return __syntax__data__[self.key]  
get  = syntax_klass('get')
get[__getattr__(make(GetFunction, arg))]
get = lead(get)

class AssignFunction(OperatorFunction):
  def run(self, data, *args, **kw):
    __syntax__data__[self.var] = result = run(self.value, data, *args, **kw)
    return result
have,have_value  = syntax_klasses('have, have_value')
have[__getattr__([arg]),
     __getitem__([args])]>>have_value
have_value[__eq__(make(AssignFunction, data, arg))]
have = lead(have)

class AppendFunction(OperatorFunction):
  def run(self, data, *args, **kw):
    value = run(self.value, data, *args, **kw)
    old_list = [] if self.var not in __syntax__data__ else __syntax__data__[self.var]
    __syntax__data__[self.var] = result = old_list+[value]
    return result
  
append, append_value  = syntax_klasses('append, append_value')
append[__getattr__([arg])]>>append_value
append_value[__lshift__(make(AppendFunction, data, arg))]
append = lead(append)

class Iterator(OperatorFunction):
  def __call__(self, *args): 
    self.args = args
    return self
  def run(self, data, *args, **kw):
    return iter(run(self.args[0], data, *args, **kw) )
iterator = lead(Iterator)

class ApplyFunction(OperatorFunction):
  def run(self, data, *args, **kw):
    args = [run(arg, data, *args, **kw)]
    return self.args[0](*self.args[1])
apply, apply_value  = syntax_klasses('apply, apply_value')
apply[__getitem__(arg)]>>apply_value
apply_value[__call__(make(ApplyFunction, data, args))]
apply = lead(apply)

##class IifFunction(OperatorFunction):
##  def run(self, data, *args, **kw):
##    value = run(self.value, data, *args, **kw)
##    old_list = [] if self.var not in __syntax__data__ else __syntax__data__[self.var]
##    __syntax__data__[self.var] = result = old_list+[value]
##    return result

##(iif, _then, then_clause, _elsif, elsif_clause, 
##  _els, els_clause, iif_form) = syntax_klasses(
##  'iif, _then, then_clause, _elsif, elsif_clause, '
##  '_els, els_clause, iif_form')
##then, elsif, els = dotwords('then, elsif, els')
##iif[__call__]>>iif_then
##iif_then[then]>>then_clause
##then_clause[__getitem__]>>iif_form
##iif_form[
##  elsif>>elsif_test,
##  els>>els_clause,
##  unbox(unbox_iif)]
##elsif_test[call]>>elsif_then
##elsif_clause [__getitem__>>iif_form]
##els_clause[__getitem__>>iif_form]
##iff = lead(iff)

args_len = apply[len](args)

import operator
def args_len_eq(n):
  return apply(operator.eq, args_len, n)

