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
        data = run(fun, data, *args, **kw)
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

lt = binary('__lt__') # <
le = binary('__le__') # <=
eq = binary('__eq__') # ==  
ne = binary('__ne__') # !=, <>
gt = binary('__gt__') # >
ge = binary('__ge__') # >=
bitor = binary('__or__')  # |
bitxor = binary('__xor__') # ^
bitand = binary('__and__') # &
lshift = binary('__lshift__') # <<
rshift = binary('__rshift__') # >>
add = binary('__add__') # +
sub = binary('__sub__') # -
mul = binary('__mul__') # *
div = binary('__div__') # /
floordiv = binary('__floordiv__') # //
mod = binary('__mod__') # %
neg = unary('__neg__') # +x, Positive
pos = unary('__pos__') # -x, negative 
invert = unary('__invert__') # ~x	Bitwise not
abs = unary('__abs__') # abs()
pow = binary('__pow___') # **	Exponentiation
getattr = binary('getattr') #   def attr
getitem = binary('__getitem__') # object[index]
call = binary('__call__') # fun(*args)
iterable = unary('__iter__')
unbox = unary('__unbox__')

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
        if '__init_syntax_instance__' not in attrs:
          def f(*args): return
          attrs['__init_syntax_instance__'] = f
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
  except AttributeError:
    if isinstance(item, list):
      return [run(i, data, *args, **kw) for i in item]
    elif isinstance(item, tuple):
      return tuple(run(i, data, *args, **kw) for i in item)
    else: return item
    
def run_sequence(items, data, *args, **kw):
  for item in items:
    result = run(item, data, *args, **kw)
  return result

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
data  = DataFunction()

class make(OperatorFunction):
  def __init__(self, klass, *args):
    self.klass, self.args = klass, args
  def run(self, data, *args, **kw):
    klass_args = [run(arg, data, *args, **kw) for arg in self.args]
    return self.klass(*klass_args)

class SeeFunction(OperatorFunction):
  def __init__(self):
    self.vars = []
  def getattr(self, var):
    self.vars.append(var)
    return self
  def run(self, data, *args, **kw):
    for v in self.vars:
      if v not in __syntax__data__: return False
    return True
see = lead(SeeFunction)

class GetFunction(OperatorFunction):
  def __init__(self):
    self.vars = []
  def getattr(self, var):
    self.vars.append(var)
    return self
  def run(self, data, *args, **kw):
    if len(self.vars)==1: return __syntax__data__[self.vars[0]]
    return [__syntax__data__[v] for v in self.vars]
get = lead(GetFunction)

class HaveFunction(OperatorFunction):
  def __init__(self):
    self.vars = []
  def getattr(self, var):
    self.vars.append(var)
    return self
  def __eq__(self, other):
    self.value = other
    return self
  def run(self, data, *args, **kw):
    value = run(self.value, data, *args, **kw)
    if len(self.vars)==1:
      __syntax__data__[self.vars[0]] = value
      return
    elif len(self.vars)!=len(value): raise PyMetaSyntaxError
    for var, v in zip(self.vars, value): 
      __syntax__data__[var] = v
    return True
have = lead(HaveFunction)

class AppendFunction(OperatorFunction):
  def __init__(self, var, value):
    self.var, self.value = var, value
  def run(self, data, *args, **kw):
    value = run(self.value, data, *args, **kw)
    old_list = [] if self.var not in __syntax__data__ else __syntax__data__[self.var]
    __syntax__data__[self.var] = result = old_list+[value]
    return result
  
append, append_value  = syntax_klasses('append, append_value')
append[getattr(arg)]>>append_value
append_value[lshift(make(AppendFunction, data, arg))]
append = lead(append)

class Iterator(OperatorFunction):
  def __call__(self, sequence): 
    self.sequence = sequence
    return self
  def run(self, data, *args, **kw):
    return iter(run(self.sequence, data, *args, **kw) )
iterator = lead(Iterator)

class ApplyFunction(OperatorFunction):
  def run(self, data, *args, **kw):
    mm = self.args[1]
    apply_args = [run(x, data, *args, **kw) for x in mm]
    return self.args[0](*apply_args)
apply, apply_value  = syntax_klasses('apply, apply_value')
apply[getitem(arg)]>>apply_value
apply_value[call(make(ApplyFunction, data, args))]
apply = lead(apply)

args_len = apply[len](args)

import operator
def args_len_eq(n):
  return apply[operator.eq](args_len, n)

class IifFunction(OperatorFunction):
  def __init__(self):
    self.test_clauses = []
    self.els_clause = None
    self.state = 'test'
  def __call__(self, *args):
    if self.state=='test':
      if len(args)!=1: raise PyMetaSyntaxError
      self.test_clauses.append(args[0])
      self.state = 'then'
      return self
    else: raise PyMetaSyntaxError
  def getattr(self, attr):
    if self.state=='then' and attr=='then': 
      self.state = 'clause'
      return self
    elif self.state=='after_clause':
      if attr=='elsif': 
        self.state = 'test'
        return self
      elif attr=='els': 
        self.state = 'els_clause'
        return self
    raise PyMetaSyntaxError
  def __getitem__(self, args):
    if self.state=='clause': 
      self.test_clauses[-1] =  [self.test_clauses[-1], args]
      self.state = 'after_clause'
      return self
    elif self.state=='els_clause': 
      self.els_clause = args
      self.state = 'end'
      return self
    raise PyMetaSyntaxError
  def run(self, data, *args, **kw):
    if self.test_clauses is None: raise PyMetaSyntaxError
    elif self.state!='end' and self.state!='after_clause':
      raise PyMetaSyntaxError
    for test, clause in self.test_clauses:
      if run(test, data, *args, **kw):
        return run_sequence(clause, data, *args, **kw)
    else:
      if self.els_clause is not None:
        return run_sequence(self.els_clause, data, *args, **kw)
iif = lead(IifFunction)

##(iif, iif_then, then_clause, 
##  elsif_test, elsif_after_test, elsif_clause, 
##  els_clause, iif_form) = syntax_klasses(
##  'iif, iif_then, then_clause, '
##  'elsif_test, elsif_after_test, elsif_clause, '
##  'els_clause, iif_form')
##then, elsif, els = dotwords('then, elsif, els')
##iif[call]>>iif_then
##iif_then[then]>>then_clause
##then_clause[getitem]>>iif_form
##iif_form[
##  elsif>>elsif_test,
##  els>>els_clause,
##  unbox(unbox_iif)]
##elsif_test[call]>>elsif_test
##elsif_after_test[then]>>elsif_clause
##elsif_clause[getitem>>iif_form]
##els_clause[getitem>>iif_form]
##iff = lead(iif)

##iif = (call(have.test==args)+then+getitem(have.test_clauses==[(get.test, args)])
##+opt(any(elsif+call(have.test==args)
##         +then+getitem(append.test_clauses<<(get.test,args)))    
##     +opt(els+getitem(make[Iff](get.test_clauses, args)))))

