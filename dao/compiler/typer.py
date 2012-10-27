pytype = type

from dao.compiler.typenv import GlobalEnvironment

from dao.base import is_subclass, is_var
from dao.solvebase import BaseCommand

from dao.compiler import type

def get_type(exp, typer):
  try: return exp.type
  except: return type.atom
def eval(exp):
  typer = make_typer()
  from dao.solvebase import to_sexpression
  sexp = to_sexpression(exp)
  return typer.solve(sexp)

def make_typer():
  env = GlobalEnvironment({})
  return Typer(env)

class Typer:  
  def __init__(self, env):
    self.env = env
  
  def solve(self, exp): 
    if isinstance(exp, tuple): 
      if is_subclass(exp[0], BaseCommand): # SpecialForm
        form = exp[0](*exp[1:])
        return form.get_type(self)
      else:
        if isinstance(exp[0], tuple):
          type0 = self.solve(exp[0])
          return type0.apply([self.solve(e) for e in exp[1:]])
        if is_var(exp[0]):
          var_type = self.env[exp[0]]
          if var_type==type.dummy:
            self.env[exp[0]] = type.recursive_user_function
            for e in exp[1:]: self.solve(e)
            return type.recursive_user_function.result_type
          else: return var_type.result_type
        return exp[0].evaluate_type(self, exp[1:])
    else:
      if is_subclass(exp, BaseCommand):
        return get_type(exp, self)
      try: exp_get_type = exp.get_type
      except: return get_type(exp, self)
      return exp_get_type(self)
    
  def solve_exps(self, exps):
    if len(exps)==0: return type.NoneType
    if len(exps)==1: return self.solve(exps[0])
    self.solve_exps(exps[:-1])
    return self.solve(exps[-1])
  
  def annotate(self, exp):
    if isinstance(exp, tuple): 
      if is_subclass(exp[0], BaseCommand): # SpecialForm
        form = exp[0](*exp[1:])
        t = form.get_type(self)
        return Anotation(exp, t)
      else:
        if isinstance(exp[0], tuple):
          type0 = self.solve(exp[0])
          return type0.apply([self.solve(e) for e in exp[1:]])
        if is_var(exp[0]):
          var_type = self.env[exp[0]]
          if var_type==type.dummy:
            self.env[exp[0]] = type.recursive_user_function
            for e in exp[1:]: self.solve(e)
            return type.recursive_user_function.result_type
          else: return var_type.result_type
        return exp[0].evaluate_type(self, exp[1:])
    else:
      if is_subclass(exp, BaseCommand):
        return get_type(exp, self)
      try: exp_get_type = exp.get_type
      except: return get_type(exp, self)
      return exp_get_type(self)
  