''' experiment for functional logic programming'''

def end(v, fc):
  return None

def solve(goal, cont, fc):
  return goal(cont)(None, fc)

def integer(x):
  def integer_fun(cont):
    def integer_fun2(v, fc):
      print x
      return cont(x, fc)
    return integer_fun2
  return integer_fun

def succeed(cont):
  def succeed_fun(v, fc):
    print 'succeed'
    return cont(True, fc)
  return succeed_fun

def fail(cont): 
  def fail_fun(v, fc):
    print 'fail'
    return fc(False, end)
  return fail_fun

def not_(goal):
  def not_fun(cont):
    def not_fun2(v, fc):
      print 'not'
      return goal(fc)(v, cont)
    return not_fun2
  return not_fun

def prin(x):
  def prin_fun(cont):
    def prin_fun2(v, fc):
      print x
      return cont(v, fc)
    return prin_fun2
  return prin_fun

def and_(a, b):
  def and_fun(cont):
    def and_fun2(v, fc):
      print 'and'
      def and_cont(v, fc):
        return b(fc)(v, cont)
      return a(and_cont)(v, fc)
    return and_fun2
  return and_fun

def begin(*exps):
  if not exps: 
    raise
  elif len(exps)==1: 
    return exps[0]
  elif len(exps)==2: 
    return and_(*exps)
  else: 
    return and_(exps[0], begin(*exps[1:]))
  
def or_(a, b):
  def or_fun(cont):
    def or_fun2(v, fc):
      print 'or'
      def or_fcont(v, fc1):
        return b(cont)(v, fc1)
      return a(cont)(v, or_fcont)
    return or_fun2
  return or_fun

def findall(goal):
  def findall_fun(cont):
    def findall_fun2(v, fc):
      print 'findall'
      def findall_next(v, fc1):
        print 'findall_next'
        return fc1(v, findall_done)
      def findall_done(v, fc1):
        print 'findall_done'
        return cont(v, fc1)
      return goal(findall_next)(v, fc)
    return findall_fun2
  return findall_fun
  
def unify(x, y):
  def unify_fun(cont):
    def unify_fun2(bindings, fc):
        x = deref(x, bindings)
        y = deref(y, bindings)
        if isinstance(x, Var):
          return bindings+{x:y}, fc
        else:
          if isinstance(y, Var):
            bindings+{y:x}
          else:
            return bindings, fc
    return unify_fun2
  return unify_fun

def parse(text, matcher):
  def parse_fun(cont):
    def parse_fun2(v, fc):
      return matcher(cont)((text, 0), fc)
    return parse_fun2
  return parse_fun
      
def char(x):
  def char_fun(cont):
    def char_fun2(parse_state, fc):
        text, pos = parse_state
        print x
        if text[pos]==x:
          return cont((text, pos+1), fc)
        else:
          return fc(parse_state, end)
    return char_fun2
  return char_fun
