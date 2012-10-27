class Done:
  def __eq__(self, other):
    return isinstance(other, Done)
  def __repr__(self):
    return 'done'
done = Done()

class return_:
  def __init__(self, *exps):
    self.exps = exps
  def __eq__(self, other):
    return isinstance(other, return_) and self.exps==other.exps
  def __repr__(self):
    return 'return_(%s)'%repr(self.exps)
  
class set:
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
  def __eq__(self, other):
    return isinstance(other, set) and self.var==other.var and self.exp==other.exp
  def __repr__(self):
    return 'set(%s, %s)'%(self.var, self.exp)
  
class call:
  def __init__(self, function, args):
    self.function, self.args = function, args
  def __eq__(self, other):
    return isinstance(other, call) and self.function==other.function and self.args==other.args
  def __repr__(self):
    return 'call(%s, %s)'%(self.function, repr(self.args))


class unify:
  def __init__(self, x, y, cont):
    self.x, self.y, self.cont = x, y, cont
  def __eq__(self, other):
    return isinstance(other, unify) and self.x==other.x and self.y==other.y and self.cont==other.cont
  def __repr__(self):
    return 'unify(%s, %s, %s)'%(self.x, self.y, self.cont)
  '''target code:
    x1 = deref(x)
    y1 = deref(y)
    if isinstance(x1, LogicVar):
      save_fc(fc1)
      set_fcont(lambda(x2,), x1.binding = x1, return (False, fc1))
      x1.binding = y1
    elif isinstance(y1, LogicVar):
      save_fc(fc1)
      set_fcont(lambda(x2,), y1.binding = y1, return (False, fc1))
      y1.binding = x1
    elif x1!=y1:
      return (False, fc1)
    else:  
      return (True, cont)
  '''
class unify_list:
  def __init__(self, x, y, cont):
    self.x, self.y, self.cont = x, y, cont
  def __eq__(self, other):
    return isinstance(other, unify_list) and self.x==other.x and self.y==other.y and self.cont==other.cont
  def __repr__(self):
    return 'unify_list(%s, %s, %s)'%(repr(self.x), repr(self.y), self.cont)
  '''target code:
    lambda(x,): unify(x[0], y[0], lambda_(unify_list(x[1:],y[1:], cont) 
  '''
  