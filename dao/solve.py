# -*- coding: utf-8 -*-

import os

from dao.solvebase import Solutions
from dao.compile import compile_to_pyfile

def solve(exp):
  try: 
    os.remove(r'f:\dao_all\dao\dao\tests\compiled.pyc')
  except: pass
  compile_to_pyfile(exp)
  from dao.tests import compiled
  reload(compiled)
  return Solutions(exp, compiled.compiled_dao_function())

def eval(exp):
  return solve(exp).next()
