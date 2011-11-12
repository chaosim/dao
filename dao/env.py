##from dao.term import UEntity

class Environment:
  def extend(self):
    return ExtendEnvironment({}, self)
  
  def __repr__(self): 
    result = '' 
    while self is not None:
      result += self._repr()+' '
      self = self.outer
    return result

class GlobalEnvironment(Environment): 
  def __init__(self, bindings):
    self.bindings = bindings
    self.outer = None
  def __getitem__(self, var): 
    try: return self.bindings[var]
    except: return var
  def __setitem__(self, var, value):
    self.bindings[var] = value
  def lookup_exit_cont(self, label, cont, form_value, solver): 
    raise Exception('block %s does not exist.'%label)
  def lookup_next_cont(self, label, cont, form_value, solver): 
    raise Exception('block %s does not exist.'%label)
  def _repr(self): return 'GENV%s'%self.bindings
    
class ExtendEnvironment(Environment):
  def __init__(self, bindings, outer):
    self.bindings, self.outer = bindings, outer 
  def __getitem__(self, var):
    if var in self.bindings: return self.bindings[var]
    if self.outer is None: return var
    return self.outer[var]
  def __setitem__(self, var, value):
    self.bindings[var] = value
  def lookup_exit_cont(self, label, cont, form_value, solver):
    return self.outer.lookup_exit_cont(label,cont, form_value, solver)
  def lookup_next_cont(self, label, cont, solver):
    return self.outer.lookup_next_cont(label, cont, solver)
  def _repr(self): 
    return "ENV%s"%(self.bindings)

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
  def _repr(self): return 'BENV %s:%s'%(self.label, self.bindings)

class NotExistVariable(Exception):
  def __init__(self, var): 
    self.var = var 
  def __repr__(self):
    return '%s'%self.var
  __str__ = __repr__
  
class ModuleEnvironment(ExtendEnvironment): 
  def __init__(self, bindings, outer, name):
    self.bindings = bindings
    self.outer = outer
    self.name = name
    
  def lookup(self, var):
    try: return self.bindings[var]
    except: raise NotExistVariable(var)
    
  def _repr(self): return 'MEnv(%s)'%self.name
  