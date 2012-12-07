# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.compile import VariableNotBound
from dao.command import Var, LogicVar
from dao.compilebase import Environment, Compiler
from dao.builtins import lamda, begin

from dao import interlang as il

def alpha(exp):
  env = Environment()
  compiler = Compiler()
  return exp.alpha_convert(env, compiler)

class TestAlphaConvert:
  def test_var(self):
    x = Var('x')
    assert_raises(VariableNotBound, alpha, x)

  def test_lamda(self):
    x, x1, y, y1, k = Var('x'), Var('x1'), Var('y'), Var('y1'), Var('k')
    eq_(alpha(lamda((x,y), 1)), lamda((x, y), il.Integer(1)))
    
  def test_lamda2(self):
    x, x1, y, y1, k = Var('x'), Var('x1'), Var('y'), Var('y1'), Var('k')
    eq_(alpha(lamda((x,y), lamda((x,), x, y), x)), lamda((x,y), lamda((x1,), x1, y), x))
    
  def test_lamda3(self):
    x, x1, y, y1, k = Var('x'), Var('x1'), Var('y'), Var('y1'), Var('k')
    assert_raises(VariableNotBound, alpha, lamda((x,), x, y))
    
  def test_begin(self):
    eq_(alpha(begin(1,2)), begin(1,2))