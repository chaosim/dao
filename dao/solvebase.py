class BaseCommand: pass

class CutException(Exception): pass
class DaoStopIteration(Exception): pass

class DaoUncaughtThrow(Exception):
  def __init__(self, tag): 
    self.tag = tag

class  DaoSyntaxError(Exception):
  pass

class DaoError(Exception):
  def __init__(self, message): 
    self.message = message
  def __repr__(self): 
    return self.message
  __str__ = __repr__

class NoSolutionFound:
  cont_order = 0 # just use it accidently
  def __init__(self, exp): 
    self.exp = exp
  def __repr__(self): return 'exit!'
  
class NoMoreSolution(Exception): pass

def tag_lookup(fun):
  def lookup_tagger(tagged_fun):
    tagged_fun.lookup = fun
    return tagged_fun
  return lookup_tagger

def done_lookup(cont, tag, stop_cont, solver): 
  raise DaoUncaughtThrow(tag)

from dao.env import unwind
def done_lookup(cont, tag, stop_cont, solver): 
  @mycont(cont)
  def loopkup_fail_cont(value, solver):
    raise  DaoUncaughtThrow(tag)
  return unwind(stop_cont, None, tag, loopkup_fail_cont, solver)

def tag_unwind(fun):
  def unwind_tagger(tagged_fun):
    tagged_fun.unwind = fun
    return tagged_fun
  return unwind_tagger
 
def done_unwind(cont, value, tag, stop_cont_cont, solver, next_cont=None):
  if cont is stop_cont_cont: 
    return cont if next_cont is None else next_cont
  raise DaoUncaughtThrow(tag)

def mycont(cont):
  def mycont_tagger(fun):
    fun.succ = cont
    try:
      fun.cont_order = cont.cont_order+1
    except:
      cont(1, None)
    return fun
  return mycont_tagger
 
@tag_lookup(done_lookup)
@tag_unwind(done_unwind)
@mycont(NoSolutionFound)
def done(value, solver): return value

@tag_lookup(done_lookup)
@tag_unwind(done_unwind)
@mycont(NoSolutionFound)
def fail_done(value, solver): 
  solver.failed = True
  return value

def value_cont(exp, cont):
  @mycont(cont)
  def value_cont(value, solver):
    solver.scont = cont
    return exp
  return value_cont

def to_sexpression(exp):
  try: exp_to_sexpression = exp.to_sexpression
  except: 
    if isinstance(exp, tuple):
      return tuple(to_sexpression(x) for x in exp)
    else: return exp
  if isinstance(exp, type):
    return exp
  return exp_to_sexpression()

#------------------------------------------
# for preprocess before Solver.solve

class Parser: 
  def parse(self, exp):
    try: exp_parse = exp.___parse___
    except: 
      if isinstance(exp, list):
        return [self.parse(e) for e in exp]
      elif isinstance(exp, tuple):
        return tuple(self.parse(e) for e in exp)
      else: return exp
    try: return exp_parse(self)
    except TypeError: return exp

def preparse(exp): 
  return Parser().parse(exp)

class LoopExitNextTagger:
  ''' use tagger to preprocess before solve expression'''
  surfix = '$'
  def __init__(self): 
    self.new_label_id = 1
    self.labels = {}
  def make_label(self, label):
    if label is None: 
      label = '$'+str(self.new_label_id)
      self.new_label_id += 1
    return label
  def push_label(self, control_struct_type, label):
    self.labels.setdefault(control_struct_type, []).append(label)
    self.labels.setdefault(None,[]).append(label)
  def pop_label(self, control_struct_type):
    self.labels[control_struct_type].pop()
    self.labels[None].pop()

  def tag_loop_label(self, exp):
    try: exp_tag_loop_label = exp.tag_loop_label
    except: 
      if isinstance(exp, list):
        return [self.tag_loop_label(e) for e in exp]
      elif isinstance(exp, tuple):
        return tuple(self.tag_loop_label(e) for e in exp)
      else: return exp
    try: return exp_tag_loop_label(self)
    except TypeError: return exp

def tag_loop_label(exp): 
  return LoopExitNextTagger().tag_loop_label(exp)

def dao_repr(exp):
    try: exp_____repr____ = exp.____repr____
    except: 
      if isinstance(exp, list) or isinstance(exp, tuple):
        return ','.join([dao_repr(e) for e in exp])
      else: return repr(exp)
    try: return exp_____repr____()
    except TypeError: return repr(exp)

