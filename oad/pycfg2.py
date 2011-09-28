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

class Object(object):
  def __getattr__(self, attr):
    try: return self.__dict__[attr]
    except: return self.getattr(attr)

class ParserState(SyntaxBase):
  def __init__(self, fsm, states):
    self.__internal__data__ = Container()
    self.__internal__data__.fsm = fsm
    self.__internal__data__.result_stack = [(None, {}, self.fsm.start_state)]
    self.__internal__data__.state = self.fsm.start_state
    self.__internal__data__.locals = {}
    self.locals = {}
  def __getattr__(self, attr):
    def result_generator():
      pass
      self.__internal__data__.result_stack.push(result_generator)
    self.__internal__data__.result_stack.push(result_generator)
    try: result = results.next()
    except: 
      self.__internal__data__.result_stack.pop()
    return self
      
class State:
  def __init__(self, number):
    self.number = number
    self.class_name = None
  def __eq__(self, other):
    return self.number==other.number
  def __hash__(self): return hash(self.number)
  def __repr__(self): return 's'+str(self.number)
  
class SyntaxMetaClass(type):
  def __init__(cls, name, bases, attrs):
    super(SyntaxMetaClass, cls).__init__(name, bases, attrs)
    def __init__(self, *args):
      self.__data__ = None
    cls.__init__ = __init__

class FSM:
  '''finite state machine'''      
  def __init__(self):
    self.state_no = 0
    self.dict = {}
    self.start_state = self.new_state()
    self.states2class = {}
  def xxxadd_edge(self, state, edge, state1=None):
    state1 = self.new_state() if state1 is None else state1
    self.dict[state].setdefault(edge, set()).add(state1)
    return state1
  def new_state(self):
    self.state_no += 1
    state = State(self.state_no)
    self.dict[state] = {}
    return state
  
  def get_class(self, states):
    states = tuple(states)
    if states in self.states2class:
      return self.state2class[states]
    self.states2class[states] = self.make_class(states)
    return self.states2class[states]
  def make_class(self, states):
    bases = (SyntaxBase,)
    attrs = {}
    dotwords = {}
    states = self.null_closure(states)
    edge2action_states = {}
    for s in states:
      for edge in self.dict[s]:
        for action_state in self.dict[s][edge]:
          edge2action_states.setdefault(edge, set()).add(action_state)
    for edge in edge2action_states:
      if  isinstance(edge, DotWord):
        dotwords[edge.name] = self.make_method(edge2action_states[edge])
      else:
        attrs[edge.name] = self.make_method(edge2action_states[edge])
    for s in states:
      if s.class_name is not None:  
        class_name = s.class_name
        break
    else: class_name = 'Class'+str(list(states)[0])
    klass = SyntaxMetaClass(class_name, bases, attrs)
    return klass
  def make_method(self, actions_states):
    def method(self1, *args, **kw):
      def result_generator():
        for (actions, state) in actions_states:
          result = run_sequence(actions, self1, *args, **kw)
          result_klass = self.get_class(state)
          result_object = result_klass() if result_klass is not None else None
          result_object.__data__ = self.__data__
          result_object.__data__['__previous__result__'] = result
          yield result_object
        for x in self1.__data__['__result_generator__']:
          yield x
      results = result_generator()
      result = results.next()
      result.__data__['__result_generator__'] = result_generator
      return result
    return method
  def null_closure(self, states):
    states = list(states)
    tested_states = set()
    while states:
      s = states.pop()
      tested_states.add(s)
      states1 = self.dict[s].get(None, set())
      for s1 in states1:
        if s1 not in tested_states:
          states.append(s1)
    return tested_states
  
class SyntaxBase(object):
  def __getattr__(self, attr):
    try: return self.__dict__[attr]
    except: return self.getattr(attr)

class Element(Object):
  def __init__(self):
    self.__class__name = None
  def toFSM(self, fsm, state0=None, state1=None):
    if state0 is None: state0 = fsm.start_state
    if state1 is None: state1 = fsm.new_state()
    state1.class_name = self.__class__name if self.__class__name is not None else None
    fsm.dict[state0].setdefault(self.edge, set()).add((self.actions, state1))
    return state1
  
  def makeFSM(self):
    fsm = FSM()
    self.toFSM(fsm, fsm.start_state)
    return fsm
  
  def getattr(self, class_name):
    self.__class__name = class_name
    return self
  
  def __add__(self, other):  return Sequence(self, other)
  def __or__(self, other): return Or(self, other)
  
class ActElement(Element):
  def __init__(self, edge, actions):
    self.edge, self.actions = edge, actions
  
class NullWord(Element): 
  def toFSM(self, fsm, state0=None, state1=None):
    if state0 is None: state0 = fsm.start_state
    if state1 is None: state1 = fsm.new_state()
    fsm.dict[state0].setdefault(None, set()).add(state1)
    return state1
  
nullword = NullWord()

class Some(Element): 
  def __init__(self, item):
    self.item = item
  def toFSM(self, fsm, state0=None, state1=None):
    if state0 is None: state0 = fsm.start_state
    new_state1 = self.item.toFSM(fsm, state0)
    state1 = any(self.item).toFSM(fsm, new_state1, state1)
    try: state1.class_name = self.class_name
    except: pass
    return state1
some = Some

class Any(Element): 
  def __init__(self, item):
    self.item = item
  def toFSM(self, fsm, state0=None, state1=None):
    if state0 is None: state0 = fsm.start_state
    new_state = nullword.toFSM(fsm, state0)
    self.item.toFSM(fsm, new_state, new_state)
    state1 = nullword.toFSM(fsm, new_state, state1)
    try: state1.class_name = self.class_name
    except: pass
    return state0
any = Any

class Optional(Element): 
  def __init__(self, item):
    self.item = item
  def toFSM(self, fsm, state0=None, state1=None):
    if state0 is None: state0 = fsm.start_state
    state1 = self.item.toFSM(fsm, state0, state1)
    nullword.toFSM(fsm, state0, state1)
    try: state0.class_name = self.class_name
    except: pass
    return state1
opt = optional = Optional

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

class Edge:
  def __call__(self, *actions):
    return ActElement(self, actions)
  
class Attribute(Edge):
  def __init__(self, name):
    self.name = name
  def __getitem__(self, test):
    return AttributeTest(self, test)
    
class Binary(Attribute): pass
binary = Binary

class Unary(Attribute): pass
unary = Unary

class FunCall(Binary): pass

class DotWord(Unary): pass

dotword = DotWord

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

iterator = unary('__iter__')

def lead_function(klass, function):
  def fun(self, data, *args, **kw):
    new  = klass()
    new.__data__['__result_generaotr__'] = ()
    return function(new, data, *args, **kw)
  return fun

def lead_class(klass):
  attrs = {}
  for a, v in klass.__dict__.items():
    if not a.startswith('__init__') and isinstance(v, type(lead)):
      attrs[a] = lead_function(klass, v)
    else: attrs[a] = v
  return type('Lead'+klass.__name__, klass.__bases__, attrs)()

def lead(element): 
  fsm = element.makeFSM()
  return lead_class(fsm.make_class(set([fsm.start_state])))

