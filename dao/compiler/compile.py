pyeval = eval

from dao.solve import to_sexpression, dao_repr, BaseCommand
from dao.base import is_subclass
from dao.compiler.cont import *

from dao.solve import set_run_mode, noninteractive
set_run_mode(noninteractive)

def compile_to_cont(exp):
  sexp = to_sexpression(exp)
  compiler = Compiler()
  return compiler.cont(sexp, DoneCont())
  
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
    
class Compiler:
  
  def __init__(self):
    pass
    #self.global_env = global_env
    #self.env = env
    self.scont = self.stop_cont = done
    self.cont_set = set()
    
    #self.parse_state = parse_state
    #self.solved = False
    # used for chart parsing, from bottom to up parsing
    # left recursive is permmited
    #self.sign_state2cont = {}
    #self.sign_state2results = {}
    #self.call_path = []

  def cont(self, exp, cont):
    if isinstance(exp, tuple): 
      if is_subclass(exp[0], BaseCommand): # SpecialForm
        form = exp[0](*exp[1:])
        return form.compile_to_cont(cont, self)
      else:
        self.scont = cont
        if isinstance(exp[0], tuple):
          cont0 = self.cont(exp[0], cont)
          
          #(result_type, result) = self.try_compile_and_evaluate(exp[0])
          #if result_type is 0: 
            ## can not solved in the compile phase, 
            ## if no error in front of the program, it should be solvable, 
            ## otherwise program can not be interpreted too. 
            ## but the result may be not static, or the type of result may be dyanmic.
            ## so type inference is necessary.
            ## lisp 1 or lisp2 ?
            #function_args_cont = self.compile_function_arguments(exp[1:])
            #macro_args_cont = self.compile_macro_arguments(exp[1:])
            #SelectCont(, )
          #elif result_type==1: #function
            #return ValueCont(result, result.compile_cont(exp[1:]))
        return ValueCont(exp[0], exp[0].compile_cont(self, exp[1:]))
    else:
      if is_subclass(exp, object):
        return ValueCont(exp, cont)
      try: exp_compile_to_cont = exp.compile_to_cont
      except: 
        vc = ValueCont(exp, cont)
        self.add_cont(vc)
        return vc
      return exp_compile_to_cont(cont, self)
    
  def exps_cont(self, exps, cont):
      if len(exps)==0: 
        return ValueCont(None, cont)
      elif len(exps)==1: 
        return self.cont(exps[0], cont)
      else:
        return self.cont(exps[0], self.exps_cont(exps[1:], cont))
  def parse_compile_to_cont(self, exp):
    sexp = to_sexpression(exp)
    return self.cont(sexp, self.stop_cont)
  def compile(self, exp):
    cont = self.cont(exp, self.stop_cont)
    return cont.code()
  def add_cont(self, cont):
    self.scont = cont
    self.cont_set.add(cont)
  
def make_evaluate_user_macro_cont(exps, cont):
  return rule_cont(rules_cont(eval_macro_result_cont(cont)))

#argument_cont(gather_cont(apply_cont()))