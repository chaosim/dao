# -*- coding: utf-8 -*-
# generated file after havingcompiled dao expression.

from dao.interlang import LogicVar
from dao.solvebase import Solver, deref

solver = Solver()

def compiled_dao_function():
  old_parse_state = solver.parse_state
  solver.parse_state = ('aaa', 0)
  fc12 = solver.fail_cont
  def function(v2):
    solver.parse_state = old_parse_state
    return fc12(False)
  solver.fail_cont = function
  fc1 = solver.fail_cont
  solver.fail_cont = lambda v: ((lambda v: True if ((solver.parse_state)[1]) == (len((solver.parse_state)[0])) else solver.fail_cont(False))(True), fc1(False))
  text, pos = solver.parse_state
  if (pos) >= (len(text)): 
    solver.fail_cont(True)
  
  if ('a') == ((text)[pos]): 
    fc11 = solver.fail_cont
    def function1(v1):
      solver.parse_state = (text, pos)
      return fc11(False)
    solver.fail_cont = function1
    solver.parse_state = (text, pos+1)
    yield any_cont((text)[pos])
  else:
    yield solver.fail_cont(True)
  
compiled_dao_function