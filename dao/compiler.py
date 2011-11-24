pyeval = eval

from dao.solve import to_sexpression, dao_repr, BaseCommand
from dao.base import is_subclass

from dao.solve import set_run_mode, noninteractive
set_run_mode(noninteractive)

def compile(exp): 
  sexp = to_sexpression(exp)
  return Compiler().compile(sexp)

stub = '''from dao.builtins.compiled.parser import *
from dao.builtins.compiled.terminal import *
from dao.base import apply_generator_fun_list
'''

def compile_exec(exp, globls):
  pycode = compile(exp)
  exec stub+pycode in globls

def compile2file(exp, file_name):
  pycode = compile(exp)
  with open(r'E:\dao\dao\builtins\tests\compiled\\' + file_name, 'w') as f:
    f.write(stub+pycode)
  
_current_parse_state = None

def get_parse_state():
  return _current_parse_state
  
def set_parse_state(parse_state):
  global _current_parse_state
  _current_parse_state = parse_state

def code(exp):
  if isinstance(exp, tuple):
    if len(exp)==1:
      result = '(%s,)'%code(exp[0])
    else:
      return '%s(%s)'%(code(exp[0]), ', '.join(code(x) for x in exp[1:]))
  else:
    try: exp_code = exp.code
    except: return dao_repr(exp)
    return exp_code()
    
class ValueCont:
  def __init__(self, exp, cont):
    self.exp, self.cont = exp, cont
  def code(self):
    return code(self.exp)
class DoneCont:
  def __init__(self):
    pass

class Compiler:
  
  def __init__(self):
    pass
    #self.global_env = global_env
    #self.env = env
    self.cont = self.stop_cont = DoneCont()
    
    #self.parse_state = parse_state
    #self.solved = False
    # used for chart parsing, from bottom to up parsing
    # left recursive is permmited
    #self.sign_state2cont = {}
    #self.sign_state2results = {}
    #self.call_path = []

  def compile_to_cont(self, exp, cont):
    if isinstance(exp, tuple): 
      if is_subclass(exp[0], BaseCommand): # SpecialForm
        form = exp[0](*exp[1:])
        return form.compile_to_cont(cont, self)
      else:
        return self.compile_to_cont(exp[0], exp[1:])
    else:
      if is_subclass(exp, object):
        return ValueCont(exp, cont)
      try: exp_compile_to_cont = exp.compile_to_cont
      except: return ValueCont(exp, cont)
      return exp_compile_to_cont(cont, self)
    
  def compile(self, exp):
    cont = self.compile_to_cont(exp, self.stop_cont)
    return cont.code()
  
  