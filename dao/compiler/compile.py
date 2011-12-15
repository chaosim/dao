pyeval = eval
pytype = type

from dao.solve import to_sexpression, dao_repr, BaseCommand
from dao.base import is_subclass, is_var
from dao.solvebase import DaoError
from dao.compiler.env import GlobalEnvironment
from dao.compiler.cont import *
from dao.command import compile_function_cont, compile_macro_cont
from dao.command import Function, Macro
from dao.builtin import BuiltinFunction

from dao.compiler import typenv
from dao.compiler import type

from dao.solve import set_run_mode, noninteractive
set_run_mode(noninteractive)

def get_type(exp, typer):
  try: return exp.type
  except: return type.atom
  
def eval_type(exp):
  compiler = make_compiler()
  from dao.solvebase import to_sexpression
  #sexp = to_sexpression(exp)
  return compiler.get_type(sexp)

def make_compiler():
  global_env = GlobalEnvironment({})
  env = global_env.extend({})
  tenv = typenv.GlobalEnvironment({})
  return Compiler(global_env, env, tenv)

def compile_to_cont(exp):
  #sexp = to_sexpression(exp)
  compiler = make_compiler()
  return compiler.cont(exp, DoneCont())
  
def compile(exp): 
  sexp = to_sexpression(exp)
  compiler = make_compiler()
  return compiler.compile(sexp)

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

class Compiler:
  
  def __init__(self, global_env, env, typenv):
    self.global_env = global_env
    self.env = env
    self.typenv = typenv
    self.scont = self.stop_cont = done
    self.fcont = self.fail_stop = fail_done
    self.cont_set = set([self.stop_cont, fail_done])
    self.root_set = set([self.stop_cont, fail_done])
    
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
        cont0, type0 = self.get_cont_type(exp[0], cont)
        if isinstance(type0, type.BuiltinFunction):
          arguments_cont = compile_function_cont(cont0, self, exp[1:], BuiltinFunction)
        elif isinstance(type0, type.Function):
          arguments_cont = compile_function_cont(cont0, self, exp[1:], Function)
        elif isinstance(type0, type.Function):
          arguments_cont = compile_macro_cont(cont0, self, exp[1:])
        else:
          raise DaoError('can not find the type of caller')
        self.add_cont(arguments_cont)
        return arguments_cont
    else:
      if is_subclass(exp, object):
        return ValueCont(exp, cont)
      try: exp_compile_to_cont = exp.compile_to_cont
      except: 
        return cont(exp)
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
    self.cont(exp, self.stop_cont)
    result = ''
    for cont in self.root_set:
      result += cont.code()
    return result
  
  def add_cont(self, cont):
    self.scont = cont
    self.cont_set.add(cont)
    return cont
  
  def get_type(self, exp): 
    if isinstance(exp, tuple): 
      if is_subclass(exp[0], BaseCommand): # SpecialForm
        form = exp[0](*exp[1:])
        return form.get_type(self)
      else:
        if isinstance(exp[0], tuple):
          type0 = self.get_type(exp[0])
          return type0.apply([self.get_type(e) for e in exp[1:]])
        if is_var(exp[0]):
          var_type = self.env[exp[0]]
          if var_type==type.dummy:
            self.env[exp[0]] = type.recursive_user_function
            for e in exp[1:]: self.get_type(e)
            return type.recursive_user_function.result_type
          else: return var_type.result_type
        return exp[0].evaluate_type(self, exp[1:])
    else:
      if is_subclass(exp, BaseCommand):
        return get_type(exp, self)
      try: exp_get_type = exp.get_type
      except: return get_type(exp, self)
      return exp_get_type(self)
    
  def get_type_exps(self, exps):
    if len(exps)==0: return type.NoneType
    if len(exps)==1: return self.get_type(exps[0])
    self.get_type_exps(exps[:-1])
    return self.get_type(exps[-1])
  
  def get_cont_type(self, exp, cont):
    if isinstance(exp, tuple): 
      if is_subclass(exp[0], BaseCommand): # SpecialForm
        form = exp[0](*exp[1:])
        return form.get_cont_type(cont, self)
      else:
        self.scont = cont
        type0, cont0 = self.get_cont_type(exp[0], cont)
        if isinstance(type0, type.BuiltinFunction):
          arguments_cont = compile_function_cont(cont0, self, exp[1:], BuiltinFunction)
        elif isinstance(type0, type.Function):
          arguments_cont = compile_function_cont(cont0, self, exp[1:], Function)
        elif isinstance(type0, type.Function):
          arguments_cont = compile_macro_cont(cont0, self, exp[1:])
        else:
          raise DaoError('can not find the type of caller')
        self.add_cont(arguments_cont)
        return arguments_cont, type0.result_type
    else:
      if is_subclass(exp, object):
        vc = ValueCont(exp, cont)
        self.add_cont(vc)
        return vc, type.atom
      try: exp_get_cont_type = exp.get_cont_type
      except: 
        vc = ValueCont(exp, cont)
        self.add_cont(vc)
        return vc, type.atom
      return exp_get_cont_type(cont, self)
    
def make_evaluate_user_macro_cont(exps, cont):
  return rule_cont(rules_cont(eval_macro_result_cont(cont)))

#argument_cont(gather_cont(apply_cont()))