# -*- coding: utf-8 -*-

import os

from dao.solvebase import Solutions
from dao.compile import compile_to_pyfile, cps_to_pyfile

def solve(exp, env):
  try: 
    os.remove(r'f:\dao_all\dao\dao\tests\compiled.pyc')
  except: pass
  compile_to_pyfile(exp, env)
  from dao.tests import compiled
  reload(compiled)
  return Solutions(exp, compiled.compiled_dao_function())

def eval(exp, env=None):
  return solve(exp, env).next()
  
def solve_cps(exp, compiler):
  try: 
    os.remove(r'f:\dao_all\dao\dao\tests\compiled.pyc')
  except: pass
  cps_to_pyfile(exp, compiler)
  from dao.tests import compiled
  reload(compiled)
  return Solutions(exp, compiled.compiled_dao_function())

def eval_cps(exp, compiler):
  return solve_cps(exp, compiler).next()
