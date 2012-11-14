# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.compile import VariableNotBound, alpha_convert
from dao.compilebase import Environment
from dao.command import lamda, begin, LogicVar

from dao import interlang as il

def alpha(exp):
  env = Environment()
  return alpha_convert(exp, env)

class TestAlphaConvert:
  def test_var(self):
    x = il.Var('x')
    eq_(alpha(x), LogicVar('x'))

  def test_lamda(self):
    x, x1, y, y1, k = il.Var('x'), il.Var('x1'), il.Var('y'), il.Var('y1'), il.Var('k')
    eq_(alpha(lamda((x,y), 1)), il.Lamda((x,y), 1))
    
  def test_lamda2(self):
    x, x1, y, y1, k = il.Var('x'), il.Var('x1'), il.Var('y'), il.Var('y1'), il.Var('k')
    eq_(alpha(lamda((x,y), lamda((x,), x, y), x)), lamda((x,y), lamda((x1,), x1, y), x))
    
  def test_lamda3(self):
    x, x1, y, y1, k = il.Var('x'), il.Var('x1'), il.Var('y'), il.Var('y1'), il.Var('k')
    eq_(alpha(lamda((x,), x, y)), lamda((x,), x, LogicVar('y')))
    
  def test_begin(self):
    eq_(alpha(begin(1,2)), begin(1,2))