##from oad.term import UEntity

class Environment:
  def extend(self, vars=(), values=()):
    bindings = {}
    for var, value in zip(vars, values):
      bindings[var] = value
    return ExtendEnvironment(bindings, self)
  def update(self, vars, values):
    for var, value in zip(vars, values):
      self.bindings[var] = value
    return self
  def __setitem__(self, var, value):
    self.bindings[var] = value
  def hasBindings(self): return True
  def __repr__(self): return "env"

class NullEnvironment(Environment):
  def __getitem__(self, var): return var
  def lookup(self, label, evaluator): 
    raise Exception('block %s does not exist.')%label
  def __setitem__(self, var, value):
    raise Exception('null env')
  def hasBindings(self): return False
  def __repr__(self): return "NullEnv"
  
class GlobalEnvironment(Environment): 
  def __init__(self, bindings=None):
    if bindings is None: bindings = {}
    self.bindings = bindings
  def __getitem__(self, var): 
    try: return self.bindings[var]
    except: return var
  def lookup(self, label, evaluator): 
    raise Exception('block %s does not exist.')%label
  def __repr__(self): return "GlobalENV%s"%self.bindings
    
class ExtendEnvironment(Environment):
  def __init__(self, bindings, outer):
    self.bindings, self.outer = bindings, outer 
  def __getitem__(self, var):
    if var in self.bindings: return self.bindings[var]
    return self.outer[var]
  def __setitem__(self, var, value):
    self.bindings[var] = value
  def lookup(self, label, cont, evaluator):
    return self.outer.lookup(label,cont, evaluator)
  def __repr__(self): return "%s"%(self.bindings)+repr(self.outer)
