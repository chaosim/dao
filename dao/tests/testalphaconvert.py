# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.compile import VariableNotBound, alpha_convert
from dao.compilebase import AlphaConvertEnvironment
from dao.command import lamda

from dao import interlang as il

class TestAlphaConvert:
  def test_var(self):
    x = il.Var('x')
    env = AlphaConvertEnvironment()
    assert_raises(VariableNotBound, alpha_convert, x, env)

  def test_lamda(self):
    x, x1, y, y1, k = il.Var('x'), il.Var('x1'), il.Var('y'), il.Var('y1'), il.Var('k')
    env = AlphaConvertEnvironment()
    eq_(alpha_convert(lamda((x,y), 1), env), il.Lamda((x,y), 1))
    
  def test_lamda2(self):
    x, x1, y, y1, k = il.Var('x'), il.Var('x1'), il.Var('y'), il.Var('y1'), il.Var('k')
    env = AlphaConvertEnvironment()
    eq_(alpha_convert(lamda((x,y), lamda((x,), x, y), x), env), lamda((x,y), lamda((x1,), x1, y), x))