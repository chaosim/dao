##from dao.term import UEntity

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
  def lookup_exit_cont(self, label, cont, form_value, solver): 
    raise Exception('block %s does not exist.'%label)
  def lookup_next_cont(self, label, cont, form_value, solver): 
    raise Exception('block %s does not exist.'%label)
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
  def lookup_exit_cont(self, label, cont, form_value, solver): 
    raise Exception('block %s does not exist.'%label)
  def lookup_next_cont(self, label, cont, form_value, solver): 
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
  def lookup_exit_cont(self, label, cont, form_value, solver):
    return self.outer.lookup_exit_cont(label,cont, form_value, solver)
  def lookup_next_cont(self, label, cont, solver):
    return self.outer.lookup_next_cont(label, cont, solver)
  def __repr__(self): return "%s"%(self.bindings)+repr(self.outer)

def unwind(cont, form_value, tag, stop_cont, solver, next_cont=None):
  try: cont_unwind = cont.unwind
  except AttributeError:
    if cont is solver.stop_cont:
      solver.solved = True
    if cont is stop_cont: 
      return cont if next_cont is None else next_cont
    else: return unwind(cont.cont, form_value, tag, stop_cont, solver, next_cont)
  return cont_unwind(cont, form_value, tag, stop_cont, solver, next_cont)

class BlockEnvironment(ExtendEnvironment):
  def __init__(self, label, outer, exit_cont, next_cont):
    self.bindings = {}
    self.label, self.outer = label, outer
    self.exit_cont, self.next_cont = exit_cont, next_cont
  def lookup_exit_cont(self, label, cont, form_value, solver):
    if label==self.label: 
      return unwind(cont, form_value, label, self.exit_cont, solver)
    return self.outer.lookup_exit_cont(label, cont, form_value, solver)
  def lookup_next_cont(self, label, cont, solver):
    if label==self.label: 
      return unwind(cont, None, label, self.exit_cont, solver, self.next_cont)
    return self.outer.lookup_next_cont(label, cont, solver)
  def __repr__(self): return '[%s: %s]'%(self.label, self.bindings)

class ModuleEnvironment(ExtendEnvironment): pass
  