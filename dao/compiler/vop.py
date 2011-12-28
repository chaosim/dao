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
