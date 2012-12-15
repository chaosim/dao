''' experiment for functional logic programming'''

def solve(goal, fc):
  return goal(fc)

def succeed(fc): 
  print 'succeed'
  return fc

def prin(x):
  def fun(fc):
    print x
    return fc
  return fun

def fail(fc): 
  print 'fail'
  return fc(None)

def and_(a, b):
  def fun(fc):
    return b(a(fc))
  return fun

def or_(a, b):
  def fun(fc):
    return a(lambda fc1: b(fc))
  return fun

def not_(a):
  def fun(fc):
    return fc()
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
    parse_state = text, 0
    return matcher(fc)(parse_state)
  return fun
      
def char(x):
  def logic_fun(fc):
    def fun(parse_state):
      text, pos = parse_state
      if text[pos]==x:
        return text, pos+1
      else: 
        return fc(None)
    return fun
  return logic_fun
