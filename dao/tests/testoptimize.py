# -*- coding: utf-8 -*-
'''optimization'''

from nose.tools import eq_, ok_, assert_raises

from dao.compile import Compiler, AlphaConvertEnvironment
from dao.optimize import optimize, OptimizationData, analyse_before_optimize
from dao.command import begin, quote, assign, if_, LogicVar
from dao.command import add
from dao.command import fail, succeed, or_, unify

from dao import interlang as il

#from dao.compile import Optimizer

v, fc = il.Var('v'), il.Var('fc')
v1, fc1 = il.Var('v1'), il.Var('fc1')
a0, a1, a2, a3, a4 = tuple(il.Var('a'+repr(i)) for i in range(5))

class Done(il.Clamda):
  def __repr__(self): return 'done()'
  
def done():
  return Done(v, fc, il.Return(v))

def compile_optimize(exp):
  exp = Compiler().cps(exp, done(), None)
  exp = AlphaConvertEnvironment().alpha_convert(exp)
  optimize_data = OptimizationData()
  analyse_before_optimize(exp, optimize_data)
  return optimize(exp, optimize_data)

class TestSimple:
  def test_integer(self):
    result = compile_optimize(1)
    expect = il.Return(1, None)
    eq_(result, expect)
    
  def test_quote(self):
    result = compile_optimize(quote(1))
    expect = il.Return(1, None)
    eq_(result, expect)
    
  def test_begin(self):
    result = compile_optimize(begin(1, 2))
    expect = il.Return(2, None)
    eq_(result, expect)
  
  def test_assign(self):
    x = il.Var('x')
    result = compile_optimize(assign(x, 2))
    expect = il.Begin((il.Assign(x, 2), il.Return(2, None)))
    eq_(result, expect)

  def test_if(self):
    result = compile_optimize(if_(0, 1, 2))
    expect = il.If(0, il.Return(1, None), il.Return(2, None))
    eq_(result, expect)
  
  def test_fail(self):
    result = compile_optimize(fail)
    expect = None
    eq_(result, expect)

  def test_succeed(self):
    result = compile_optimize(succeed)
    expect = done()
    eq_(result, expect)

  def test_or(self):
    result = compile_optimize(or_(1, 2))
    expect = il.Return(1, il.Lamda((v1, fc1), il.Return(2, None)))
    eq_(result, expect)
    
  def test_unify(self):
    eq_(compile_optimize(unify(1, 2)), None)    
    eq_(compile_optimize(unify(1, 1)), done()) 
    
  def test_unify2(self):
    x = LogicVar('x')
    result = compile_optimize(unify(x, 2))
    expect = Clamda(v, ret(il.unify(x, 2, done(), None)))
    eq_(result, expect)
    
  def test_add(self):
    result = compile_optimize(add(1, 2))
    expect = il.Return(il.add((1, 2)), None)
    eq_(result, expect)

def test_optimize(exp):
  data = OptimizationData()
  analyse_before_optimize(exp, data)
  return optimize(exp, data)

class TestOptimize:
  def test_if(self):
    result = test_optimize(il.If(1, il.If(1, 2, 3), 4))
    expect = il.If(1, 2, 4)
    eq_(result, expect)
    
  def test_if2(self):
    result = test_optimize(il.If(1, 2, il.If(1, 3, 4)))
    expect = il.If(1, 2, 4)
    eq_(result, expect)
    
  def test_if3(self):
    result = test_optimize(il.If(1, il.If(1, 2, 3), il.If(1, 4, 5)))
    expect = il.If(1, 2, 5)
    eq_(result, expect)
    
  def test_lambda_apply(self):
    result = test_optimize(il.Clamda(v, il.Return(1, None))(v, fc))
    expect = il.Return(1, None)
    eq_(result, expect)
  
  def test_lambda_apply2(self):
    v1, fc1 = il.Var('v1'), il.Var('fc1')
    result = test_optimize(il.Clamda(v1, fc1, il.Return(v1, fc1))(v, fc))
    expect = il.Return(v)
    eq_(result, expect)
    