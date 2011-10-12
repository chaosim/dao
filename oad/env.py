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
  def __repr__(self): return "env"

class NullEnvironment(Environment):
  def __getitem__(self, var): return var
  def lookup(self, label, solver): 
    raise Exception('block %s does not exist.')%label
  def __setitem__(self, var, value):
    raise Exception('null env')
  def __repr__(self): return "NullEnv"
  
class GlobalEnvironment(Environment): 
  def __init__(self, bindings=None):
    if bindings is None: bindings = {}
    self.bindings = bindings
    self.outer = None
  def __getitem__(self, var): 
    try: return self.bindings[var]
    except: return var
  def lookup(self, label, cont, solver): 
    raise Exception('block %s does not exist.'%label)
  def __repr__(self): return "GlobalENV%s"%self.bindings
    
class ExtendEnvironment(Environment):
  def __init__(self, bindings, outer):
    self.bindings, self.outer = bindings, outer 
  def __getitem__(self, var):
    if var in self.bindings: return self.bindings[var]
    return self.outer[var]
  def __setitem__(self, var, value):
    self.bindings[var] = value
  def lookup(self, label, cont, solver):
    return self.outer.lookup(label,cont, solver)
  def __repr__(self): return "%s"%(self.bindings)+repr(self.outer)

def unwind(cont, tag, stop_cont, solver):
  try: return cont.unwind(cont, tag, stop_cont, solver)
  except AttributeError: 
    if cont is stop_cont: return cont
    else: return unwind(cont.cont, tag, stop_cont, solver)

class BlockEnvironment(ExtendEnvironment):
  def __init__(self, label, outer, cont):
    self.label, self.outer, self.cont = label, outer, cont
    self.bindings = {}
  def lookup(self, label, cont, solver):
    if label==self.label: 
      return unwind(cont, label, self.cont, solver)
    return self.outer.lookup(label, cont, solver)
  def __repr__(self): return '[%s: %s]'%(self.label, self.bindings)

class ModuleEnvironment(ExtendEnvironment): pass
  