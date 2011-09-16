from oad.term import Var

_var_cache = {}
def varcache(name):
  try: return _var_cache[name]
  except: 
    _var_cache[name] = Var(name)
    return _var_cache[name]

class AmibiguityPathError(Exception):
  def __init__(self, item):
    self.item = item

class SyntaxBase: 
  def __init__(self, fsm, state):
    self.fsm = fsm
    self.state = state # set([self.fsm.start_state])
  def __getattr__(self, attr):
    if attr not in self.state.attrs: return
    
class ParserState(SyntaxBase):
  def __init__(self, fsm, states):
    self.fsm = fsm
    self.states = states
    for state in self.states:
      if item in self.fsm.dict[state]:
        if isinstance(item, Init):
          method = iteme.make_method()
          result = method(self)
          state1 = self.fsm.dict[state][item]
          states[state1] = result
    self.states = null_closure(states)
  def getattr(self, attr):
    states = {}
    for state in self.states:
      if item in self.fsm.dict[state]:
        if isinstance(item, Binary):
          if item.attr== 'getattr':
            method = iteme.make_method()
            result = method(self)
            state1 = self.fsm.dict[state][item]
            states[state1] = result
    return ParserState(fsm, null_closure(states))        
##  def __lt__(self, other): pass 
##  def __le__(self, other): pass 
##  def __eq__(self, other): pass 
##  def __ne__(self, other): pass 
##  def __gt__(self, other): pass 
##  def __ge__(self, other): pass 
##  def __getattr__(self, name): pass 
##  def __call__(self, *args): pass 
##  def __getitem__(self, key): pass 
##  def __iter__(self): pass 
##  def __add__(self, other): pass 
##  def __sub__(self, other): pass 
##  def __mul__(self, other): pass 
##  def __floordiv__(self, other): pass 
##  def __mod__(self, other): pass 
##  def __pow__(self, other): pass 
##  def __lshift__(self, other): pass 
##  def __rshift__(self, other): pass 
##  def __and__(self, other): pass 
##  def __xor__(self, other): pass 
##  def __or__(self, other): pass 
##  def __div__(self, other): pass 
##  def __truediv__(self, other): pass 
##  def __neg__(self): pass 
##  def __pos__(self): pass
##  def __abs__(self): pass 
##  def __invert__(self): pass 

class FSM:
  '''finite state machine, have no multiple edges'''
  class State:
    def __init__(self, number):
      self.number = number
    def __eq__(self, other):
      return self.number==other.number
    def __hash__(self): return hash(self.number)
    def __repr__(self): return 's'+str(self.number)
    
  class SyntaxMetaClass(type):
    def __init__(cls, name, bases, attrs):
      super(FSM.SyntaxMetaClass, cls).__init__(name, bases, attrs)
      def __init__(self, *args):
        self.__data__ = None
        self.syntax_do_init_instance(*args)
      cls.__init__ = __init__
      
  def __init__(self):
    self.state_no = 0
    self.dict = {}
    self.rev_dict = {}
    self.start_state = self.new_state()
  def add_edge(self, state, edge, state1=None):
    if state1 is None: state1 = self.new_state()
    self.dict[state].setdefault(edge, set()).add(state1)
    return state1
  def new_state(self):
    self.state_no += 1
    state = FSM.State(self.state_no)
    self.dict[state] = {}
    return state
  
  def make_class(self):
    state2class = {}
    bases = (SyntaxBase,)
    for state in self.dict:
      if self.dict[state]=={}: continue
      try: class_name = state.class_name
      except: class_name = 'Class'+str(state)
      attrs = {}
      dotwords = {}
      for item in self.dict[state]:
        if isinstance(item, Init): 
          attrs['syntax_do_init_instance'] = item.init_method
        else:
          state1 = self.dict[state][item]
          if isinstance(item, DotWord):
            dotwords[item.word] = item.make_method(state2class, state1)
          elif isinstance(item, Attribute):
            attrs[item.attr] = item.make_method(state2class, state1)
        if 'syntax_do_init_instance' not in attrs:
          def f(*args): return
          attrs['syntax_do_init_instance'] = f
        klass = FSM.SyntaxMetaClass(class_name, bases, attrs)
        state2class[state] = klass
    return state2class[self.start_state]
        
class SyntaxBase(object):
  def __getattr__(self, attr):
    try: return self.__dict__[attr]
    except: return self.getattr(attr)

class Element: 
  def toFSM(self, fsm, state0=None, state1=None):
    if state0 is None: state0 = fsm.start_state
    state1 = fsm.add_edge(state0, self, state1)
    try: state1.class_name = self.class_name
    except: pass
    return state1
  
  def makeFSM(self):
    fsm = FSM()
    self.toFSM(fsm, fsm.start_state)
    return fsm
  
  def __getitem__(self, class_name):
    self.class_name = class_name
    return self
  
  def __add__(self, other):  return Sequence(self, other)
  def __or__(self, other): return Or(self, other)

class Some(Element): 
  def __init__(self, item):
    self.item = item
  def toFSM(self, fsm, state0=None, state1=None):
    if state0 is None: state0 = fsm.start_state
    state1 = self.item.toFSM(fsm, state0, state1)
    state1 = self.item.toFSM(fsm, state1, state1)
    try: state1.class_name = self.class_name
    except: pass
    return state1
some = Some

class Any(Element): 
  def __init__(self, item):
    self.item = item
  def toFSM(self, fsm, state0=None, state1=None):
    if state0 is None: state0 = fsm.start_state
    if state1 is not None and state1!=state0: raise AmibiguityPathError(self)
    state0 = self.item.toFSM(fsm, state0, state0)
    try: state0.class_name = self.class_name
    except: pass
    return state0
any = Any

class Sequence(Element):
  def __init__(self, *items):
    self.items = items
  def toFSM(self, fsm, state0, state1=None):
    if state0 is None: state0 = fsm.start_state
    if len(self.items)==0: return state0
    for item in self.items[:-1]:
      state0 = item.toFSM(fsm, state0, None)
    state = self.items[-1].toFSM(fsm, state0, state1)
    try: state.class_name = self.class_name
    except: pass
    return state
seq = Sequence

class Or(Element):
  def __init__(self, *items):
    self.items = items
    
  def toFSM(self, fsm, state0, state1=None):
    if state0 is None: state0 = fsm.start_state
    state = self.items[0].toFSM(fsm, state0, state1)
    if len(self.items)==1: return state
    for item in self.items[1:]:
      state = item.toFSM(fsm, state0, state1)
    try: state1.class_name = self.class_name
    except: pass
    return state1
or_ = Or      

class CallableElement(Element):
  def __call__(self, *functions):
    result = self.copy()
    result.functions = result.functions+functions
    return result
    

class Attribute(CallableElement):
  def __init__(self, attr):
    self.functions = ()
    self.attr = attr
  def copy(self): 
    result = self.__class__(self.attr)
    result.functions = self.functions[:]
    return result
  def __div__(self, other):
    self.result_class = other
    return self

class Binary(Attribute):
  def make_method(self, state2class, state1):
    def attr_method(self1, operand2):
      result = self1.__data__
      for fun in self.functions: result = fun(result, operand2)
      try:  
        syntax_result = state2class[state1]()
        syntax_result.__data__ = result
        return syntax_result
      except: return result
    return attr_method
binary = Binary

class Unary(Attribute):
  def make_method(self, state2class, state1):
    def attr_method(self1):
      result = self1.__data__
      for fun in self.functions: result = fun(result)
      try:  
        syntax_result = state2class[state1]()
        syntax_result.__data__ = result
        return syntax_result
      except: return result
    return attr_method
unary = Unary

class Fun(CallableElement):
  def __init__(self, name):
    self.name = name
    self.functions = ()
  def copy(self): 
    result = self.__class__(self.name)
    result.functions = self.functions[:]
    return result
  def make_method(self, state2class, state1):
    def attr_method(self1, *args):
      result = self1.__data__
      for fun in self.functions: result = fun(result, *args)
      try:  
        syntax_result = state2class[state1]()
        syntax_result.__data__ = result
        return syntax_result
      except: return result
    return attr_method

fun = Fun
when_fun = fun('when')
until_fun = fun('until')

class DotWord(Unary):
  def __init__(self, word):
    self.word = word
    self.functions = ()

dotword = DotWord

of_word = dotword('of')

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
neq = binary('__ne__') # !=, <>
gt = binary('__gt__') # >
ge = binary('__ge__') # >=
bitor = binary('__or__')  # |
bitxor = binary('__xor__') # ^
bitand = binary('__and__') # &
bitlshift = binary('__lshift__') # <<
bitrshift = binary('__rshift__') # >>
add = binary('__add__') # +
sub = binary('__sub__') # -
mul = binary('__mul__') # *
div = binary('__div__') # /
floordiv = binary('__floordiv__') # //
mod = binary('__mod__') # %
negation = unary('__neg__') # +x, Positive
positive = unary('__pos__') # -x, negative 
invert = unary('__invert__') # ~x	Bitwise not
abs = unary('__abs__') # abs()
pow = binary('__pow___') # **	Exponentiation
getattr = binary('getattr') #   def attr
getitem = binary('__getitem__') # object[index]
call = binary('__call__') # fun(*args)

iter = unary('__iter__')

class Init(CallableElement):
  def __init__(self):
    self.functions = ()
    def __init__(self1):
      result = self1.__data__
      for fun in self.functions: result = fun(result)
      self1.__data__ = result
    self.init_method = __init__
  def copy(self): return Init()
init  = Init()

class Container(Element):
  def __init__(self):
    self.functions = ()
  def copy(self): return Container()

def lead(element): return element.makeFSM().make_class()()

##use_item = attr|div(var)|div(str)
##use = 'use'+some(use_item)+optional(use_block)|any(use_item)+use_block\
##          |some(use_item)+div+use_block

def append_var(result, attr): return result+(varcache(attr),)
v = lead(getattr['v_Var'](lambda attr, result: varcache(attr)))

def vars_iter(result):
  for v in result: yield v
var = lead(init(lambda x:())
           |some(getattr(append_var))['var_Vars']
           +iter(vars_iter))

my = lead(getattr['LocalVar'])
out = lead(getattr['OuterVar'])
globl = lead(getattr['GlobalVar'])

def check_form(result, forms): pass
block = getitem(check_form)
calls = any(getattr)|any(getattr+call)+getattr+(getattr('end')|call)
calls = some(getattr)+getattr('end')
calls = getattr+any(getattr+getattr)+getattr+(call|dotword('end'))
calls = getattr+(call|dotword('end'))
stmts = block|calls

def check_let_bindings(result, bindings): pass
let = lead(call(check_let_bindings)+dotword('do')+stmts)

##put = lead(getattr|getitem + eq(check_form))
##
##body = some(getattr|getattr+call) | block 
##do_condition = when_fun | until_fun | when_fun + until_fun
##
##do = lead(body+do_condition)
##
##on = lead(call+getattr('do')+stmts)
##case = lead(call+some(getattr('of')+call+some(stmts))|div(dict))
##iff = lead(call+stmts+any(attr('elsif')+stmts)+optional(getattr('els')+stmts))
##loop = lead(call(int)+stmts)
##
##fun = lead(getattr+(eq+rule_dict|(getattr('at')+rule_list)))
##
##decl = use|var|v|my|out|globl
##stmt = do|put|let|fun|macro|on|case|iff|loop|rule|rules|put
##form = decl|stmt