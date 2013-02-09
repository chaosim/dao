''' experiment for functional logic programming'''

def solve(goal, cont, fc):
  return goal(cont)(None, fc, Bindings(), None)

def end(v, fc, bindings, parse_state):
  print 'end'

def done(v, fc, bindings, parse_state):
  print 'done'
  return v

class Bindings(dict):
  def __add__(self, other):
    result = Bindings(self)
    result.update(other)
    return result
  
  def add(self, key, value):
    self[key] = value

class Var:
  def __init__(self, name):
    self.name = name
  
  def __call__(self, cont):
    def var_fun2(v, fc, bindings, parse_state):
      return cont(self.deref(bindings), fc, bindings, parse_state)
    return var_fun2
  
  def deref(self, bindings):
    try:
      v = bindings[self]
    except:
      return self
    while not isinstance(v, Var):
      self = v
      try: 
        v = bindings[self]
      except:
        return v
    return v
  
  def getvalue(self, memo, bindings):
    try: return memo[self]
    except:
      binding = bindings[self]
      if binding is self: 
        memo[self] = binding
        return binding
      else:
        result = getvalue(binding, memo)
        return result
      
  def __eq__(x, y):
    return isinstance(y, Var) and x.name==y.name
  
  def __hash__(self): return hash(self.name)
  
  def __repr__(self): return self.name
    
class DummyVar(Var):
  def __init__(self, name='_'): 
    Var.__init__(self, name)
      
  def deref(self, bindings): 
    return self

  def getvalue(self, memo, bindings):
    try: return memo[self]
    except:
      binding = bindings[self]
      if binding is self: 
        memo[self] = binding
        return binding
      else:
        result = getvalue(binding, memo)
        return result

def deref(item, bindings):
  try:
    item_deref = item.deref
  except:
    return item
  return item_deref(bindings)
  
def integer(x):
  def integer_fun(cont):
    def integer_fun2(v, fc, bindings, parse_state):
      print x
      return cont(x, fc, bindings, parse_state)
    return integer_fun2
  return integer_fun

def if_(test, then, else_):
  def if_fun(cont):
    then_cont = then(cont)
    else_cont = else_(cont)
    def if_cont(v, fc, bindings, parse_state):
      if v:
        return then_cont(v, fc, bindings, parse_state)
      else:
        return else_cont(v, fc, bindings, parse_state)
    test_if_cont = test(if_cont)
    def if_fun2(v, fc, bindings, parse_state):
      return test_if_cont(v, fc, bindings, parse_state)
    return if_fun2
  return if_fun
      
def succeed(cont):
  def succeed_fun2(v, fc, bindings, parse_state):
    print 'succeed'
    return cont(True, fc, bindings, parse_state)
  return succeed_fun2

def fail(cont): 
  def fail_fun2(v, fc, bindings, parse_state):
    print 'fail'
    return fc(False, end, bindings, parse_state)
  return fail_fun2

def not_(goal):
  def not_fun(cont):
    def not_fun2(v, fc, bindings, parse_state):
      print 'not'
      return goal(fc)(v, cont, bindings, parse_state)
    return not_fun2
  return not_fun

def prin(x):
  def prin_fun(cont):
    def prin_fun2(v, fc, bindings, parse_state):
      print x
      return cont(v, fc, bindings, parse_state)
    return prin_fun2
  return prin_fun

def and_2(a, b):
  def and_fun(cont):
    b_cont = b(cont)
    def and_cont(v, fc1, bindings1, parse_state1):
      print 'and_cont'
      return b_cont(v, fc1, bindings1, parse_state1)
    a_and_cont = a(and_cont)
    
    def and_fun2(v, fc, bindings, parse_state):
      print 'and'
      return a_and_cont(v, fc, bindings, parse_state)
    return and_fun2
  return and_fun

def begin(*exps):
  if not exps: 
    raise
  elif len(exps)==1: 
    return exps[0]
  elif len(exps)==2: 
    return and_2(*exps)
  else: 
    return and_2(exps[0], begin(*exps[1:]))
and_ = begin

def or_2(a, b):
  def or_fun(cont):
    a_cont = a(cont)
    b_cont = b(cont)
    def or_fcont(v, fc1, bindings1, parse_state1):
      return b_cont(v, fc1, bindings1, parse_state1)    
    def or_fun2(v, fc, bindings, parse_state):
        print 'or'
        return a_cont(v, or_fcont, bindings, parse_state)
    return or_fun2
  return or_fun

def or_(*exps):
  if not exps: 
    raise
  elif len(exps)==1: 
    return exps[0]
  elif len(exps)==2: 
    return or_2(*exps)
  else: 
    return or_2(exps[0], or_(*exps[1:]))
        
def unify(x, y):
  def unify_fun(cont):
    def unify_fun2(v, fc, bindings, parse_state):
        x1 = deref(x, bindings)
        y1 = deref(y, bindings)
        if isinstance(x, Var):
          return cont(v, fc, bindings+{x1:y1}, parse_state)
        else:
          if isinstance(y, Var):
            return cont(v, fc, bindings+{y1:x1}, parse_state) 
          else:
            if x==y:
              return cont(v, fc, bindings, parse_state) 
            else:
              return fc(v, end, bindings, parse_state)
    return unify_fun2
  return unify_fun

def parse(text, matcher):
  def parse_fun(cont):
    def parse_fun2(v, fc, bindings, parse_state):
      return matcher(cont)(v, fc, bindings, (text, 0))
    return parse_fun2
  return parse_fun

def set_text(text):
  def set_text_fun(cont):
    def set_text_fun2(v, fc, bindings, parse_state):
      return cont(v, fc, bindings, (text, 0))
    return set_text_fun2
  return set_text_fun

def char(item):
  def char_fun(cont):
    def char_fun2(v, fc, bindings, parse_state):
      print 'char: ', 
      text, pos = parse_state
      if pos==len(text):
        print 'end of text'
        return fc(False, end, bindings, parse_state)
      else:
        print text[pos], pos 
        x = deref(item, bindings)
        if isinstance(x, str):
          if x==text[pos]:
            return cont(True, fc, bindings, (text, pos+1))
          else:
            return fc(False, end, bindings, parse_state)
        elif isinstance(x, Var):
          return cont(True, fc, bindings+{x:text[pos]}, (text, pos+1))
        else: raise TypeError(x)
    return char_fun2
  return char_fun

def eoi(cont):
  def eoi_fun2(v, fc, bindings, parse_state):
    text, pos = parse_state
    print 'eoi'
    if pos==len(text):
      return cont(v, fc, bindings, (text, pos))
    else:
      return fc(v, end, bindings, (text, pos))
  return eoi_fun2

def findall(goal):
  def findall_fun(cont):
    def findall_fun2(v, fc, bindings, parse_state):
      print 'findall'
      def findall_done(v, fc1, bindings1, parse_state1):
        print 'findall_done'
        fc
        return cont(v, fc, bindings1, parse_state1)
      def findall_next(v, fc1, bindings1, parse_state1):
        print 'findall_next'
        fc
        return fc1(v, findall_done, bindings1, parse_state1)
      return goal(findall_next)(v, findall_done, bindings, parse_state)
    return findall_fun2
  return findall_fun
  
def any(goal):
  def any_fun(cont):
    def any_fun2(v, fc, bindings, parse_state):
      print 'any'
      def any_fcont(v, fc1, bindings1, parse_state1):
        print 'any_fcont'
        return cont(v, fc, bindings, parse_state)    
      return goal(any_fun2)(v, any_fcont, bindings, parse_state)
    return any_fun2
  return any_fun

def lazy_any(goal):
  def lazy_any_fun(cont):
    def lazy_any_fun2(v, fc, bindings, parse_state):
      print 'lazy_any'
      def lazy_any_fcont(v, fc1, bindings1, parse_state1):
        print 'lazy_any_fcont'
        return goal(lazy_any_cont)(v, fc, bindings1, parse_state1)    
      def lazy_any_cont(v, fc1, bindings1, parse_state1):
        print 'lazy_any_cont'
        return cont(v, lazy_any_fcont, bindings1, parse_state1)    
      return lazy_any_cont(None, fc, bindings, parse_state)
    return lazy_any_fun2
  return lazy_any_fun

def greedy_any(goal):
  def greedy_any_fun(cont):
    def greedy_any_fun2(v, fc, bindings, parse_state):
      print 'greedy_any'
      def greedy_any_fcont(v, fc1, bindings1, parse_state1):
        print 'greedy_any_fcont'
        return cont(v, fc, bindings1, parse_state1)    
      def greedy_any_cont(v, fc1, bindings1, parse_state1):
        print 'greedy_any_cont'
        return goal(greedy_any_cont)(v, greedy_any_fcont, bindings1, parse_state1)    
      return greedy_any_cont(None, fc, bindings, parse_state)
    return greedy_any_fun2
  return greedy_any_fun

