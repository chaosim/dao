# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.compiler.interlang import *

from dao.compiler.compile import Compiler, AlphaConvertEnvironment, VariableNotBound
from dao.compiler.command import begin, quote, assign, if_, LogicVar
from dao.compiler.command import add
from dao.compiler.command import fail, succeed, or_, unify
from dao.compiler.command import lamda

from dao.compiler import interlang as il

v, fc = il.Var('v'), il.Var('fc')
a0, a1, a2, a3, a4 = tuple(il.Var('a'+repr(i)) for i in range(5))

class Done(Clamda):
  def __repr__(self): return 'done()'
  
def done():
  return Done(v, fc, il.Return(v, fc))

def cps_convert(exp):
  return Compiler().cps_convert(exp, done(), None)

class TestCPSConvert:
  def test_integer(self):
    result = cps_convert(1)
    expect = done()(1, None)
    eq_(result, expect)
    
  def test_quote(self):
    result = cps_convert(quote(1))
    expect = done()(1, None)
    eq_(result, expect)
    
  def test_begin(self):
    result = cps_convert(begin(1, 2))
    expect = Clamda(v, fc, done()(2, None))(1, None)
    eq_(result, expect)
  
  def test_assign(self):
    x = il.Var('x')
    result = cps_convert(assign(x, 2))
    expect = Clamda(v, fc, il.Assign(x, v), il.Return(v, fc))(2, None)
    eq_(result, expect)

  def test_if(self):
    result = cps_convert(if_(0, 1, 2))
    expect = Clamda(v, fc, il.If(v, done()(1, None), done()(2, None)))(0, None)
    eq_(result, expect)
  
  def test_fail(self):
    result = cps_convert(fail)
    expect = None
    eq_(result, expect)

  def test_succeed(self):
    result = cps_convert(succeed)
    expect = done()
    eq_(result, expect)

  def test_or(self):
    result = cps_convert(or_(1, 2))
    expect = Clamda(v, fc, done()(1, Clamda(v, fc, done()(2, None))))
    eq_(result, expect)
    
  def test_unify(self):
    eq_(cps_convert(unify(1, 2)), None)    
    eq_(cps_convert(unify(1, 1)), done()) 
    
  def test_unify2(self):
    x = LogicVar('x')
    result = cps_convert(unify(x, 2))
    expect = il.Unify(x, 2, done(), None)
    eq_(result, expect)
    
  def test_add(self):
    result = cps_convert(add(1, 2))
    expect = Clamda(a0, fc, Clamda(a1, fc, Return(done()(il.add((a0, a1)), fc)))(2, None))(1, None)
    eq_(result, expect)

  def test_lambda(self):
    x, y, k = il.Var('x'), il.Var('y'), il.Var('k')
    result = cps_convert(lamda((x,y), 1))
    expect = done()(lamda((x, y, k), k(1, None)), None)
    eq_(result, expect)

class TestAlphaConvert:
  def test_var(self):
    x = il.Var('x')
    env = AlphaConvertEnvironment()
    assert_raises(VariableNotBound, env.alpha_convert, x)

  def test_lamda(self):
    x, x1, y, y1, k = il.Var('x'), il.Var('x1'), il.Var('y'), il.Var('y1'), il.Var('k')
    env = AlphaConvertEnvironment()
    eq_(env.alpha_convert(lamda((x,y), 1)), lamda((x,y), 1))
    
  def test_lamda2(self):
    x, x1, y, y1, k = il.Var('x'), il.Var('x1'), il.Var('y'), il.Var('y1'), il.Var('k')
    env = AlphaConvertEnvironment()
    eq_(env.alpha_convert(lamda((x,y), lamda((x,), x, y), x)), lamda((x,y), lamda((x1,), x1, y), x))
