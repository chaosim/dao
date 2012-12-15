''' experiment for functional logic programming'''

def solve(goal, cont, fc):
  return goal(fc)(cont)

def integer(x):
  def fun(fc):
    def fun2(cont):
      cont(x)
      return fc
    return fun2
  return fun

def succeed(fc):
  def fun(cont):
    print 'succeed'
    cont(True)
    return fc
  return fun

def fail(fc): 
  def fun(cont):
    print 'fail'
    return fc(False)
  return fun

def not_(goal):
  def fun(fc):
    def fun2(cont):
      return goal(cont)(fc)
    return fun2
  return fun

def prin(x):
  def fun(fc):
    def fun2(cont):
      print x
      cont(None)
      return fc
    return fun2
  return fun

def and_(a, b):
  def fun(fc):
    def fun2(cont):
      def new_cont(v):
        x = b(fc)(cont)
        #cont(v)
        return fc
      return a(fc)(new_cont)
    return fun2
  return fun

def or_(a, b):
  def fun(fc):
    def fun2(cont):
      def fcont(fc1):
        x= b(fc)(cont)
        #cont(x)
        return fc1
      return a(fcont)(cont)
    return fun2
  return fun

def findall(goal):
  def fun(fc):
    return goal(lambda fc1: fc(fc1))(fc)
  return fun
  
def unify(x, y):
  def logic_fun(fc):
    def fun(bindings):
        x = deref(x, bindings)
        y = deref(y, bindings)
        if isinstance(x, Var):
          bindings+{x:y}
        else:
          if isinstance(y, Var):
            bindings+{y:x}
          else:
            return None
    return fun
  return logic_fun

def parse(text, matcher):
  def fun(fc):
    def fun2(cont):
      parse_state = text, 0
      return matcher(fc)(cont)(parse_state)
    return fun2
  return fun
      
def char(x):
  def fun(fc):
    def fun2(cont):
      def fun3(parse_state):
        text, pos = parse_state
        print x
        if text[pos]==x:
          cont((text, pos+1))
          return fc
        else:
          return fc(parse_state)
      return fun3
    return fun2
  return fun


