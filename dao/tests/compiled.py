# -*- coding: utf-8 -*-
# generated file after havingcompiled dao expression.

from dao.interlang import LogicVar
from dao.solvebase import Solver, deref

solver = Solver()

def compiled_dao_function():
  old_parse_state = solver.parse_state
  solver.parse_state = ('abcde', 0)
  fc11 = solver.fail_cont
  def function(v1):
    solver.parse_state = old_parse_state
    return fc11(False)
  solver.fail_cont = function
  text, pos = solver.parse_state
  if False: 
    solver.fail_cont(True)
  
  if ('a') == ((text)[pos]): 
    fc1 = solver.fail_cont
    def function1(v):
      solver.parse_state = (text, pos)
      return fc1(False)
    solver.fail_cont = function1
    solver.parse_state = (text, pos+1)
    yield (text)[pos]
  else:
    yield solver.fail_cont(True)
compiled_dao_function