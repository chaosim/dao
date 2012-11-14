# -*- coding: utf-8 -*-

from dao.base import is_subclass

from dao.solvebase import Solutions
from dao.compile import compile_to_pyfile

def solve(exp): 
  compile_to_pyfile(exp)
  from dao.tests import compiled
  return Solutions(exp, compiled.compiled_dao_function(None))

def eval(exp):
  return solve(exp).next()
