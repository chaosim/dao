# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.compiler.compile import Compiler, AlphaConvertEnvironment
from dao.compiler.optimize import optimize, OptimizationData, analyse_before_optimize
from dao.compiler.command import begin, quote, assign, if_, LogicVar
from dao.compiler.command import add
from dao.compiler.command import fail, succeed, or_, unify

from dao.compiler import interlang as il

#from dao.compiler.compile import Optimizer

v, fc = il.Var('v'), il.Var('fc')
v1, fc1 = il.Var('v1'), il.Var('fc1')
a0, a1, a2, a3, a4 = tuple(il.Var('a'+repr(i)) for i in range(5))

class Done(il.Clamda):
  def __repr__(self): return 'done()'
  
def done():
  return Done(v, fc, il.Return(v, fc))

def compile_optimize(exp):
  exp = Compiler().cps(exp, done(), None)
  exp = AlphaConvertEnvironment().alpha_convert(exp)
  optimize_data = OptimizationData()
  analyse_before_optimize(exp, optimize_data)
  return optimize(exp, optimize_data)

class TestSimple:
  def test_integer(self):
    result = compile_optimize(1)
    expect = (il.Return((1, None)),)
    eq_(result, expect)
    
  def test_quote(self):
    result = compile_optimize(quote(1))
    expect = (il.Return((1, None)),)
    eq_(result, expect)
    
  def test_begin(self):
    result = compile_optimize(begin(1, 2))
    expect = ((il.Return((2, None)),),)
    eq_(result, expect)
  
  def test_assign(self):
    x = il.Var('x')
    result = compile_optimize(assign(x, 2))
    expect = clamda(v, fc, il.assign(x, v), ret(v, fc))(2, None)
    eq_(result, expect)

  def test_if(self):
    result = compile_optimize(if_(0, 1, 2))
    expect = (il.If(0, (il.Return((1, None)),), (il.Return((2, None)),)),)
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
    expect = (il.Return((1, il.Lamda((v1, fc1), (il.Return((2, None)),)))),)
    eq_(result, expect)
    
  def test_unify(self):
    eq_(compile_optimize(unify(1, 2)), None)    
    eq_(compile_optimize(unify(1, 1)), done()) 
    
  def test_unify2(self):
    x = LogicVar('x')
    result = compile_optimize(unify(x, 2))
    expect = clamda(v, fc, ret(il.unify(x, 2, done(), None)))
    eq_(result, expect)
    
    
  def test_add(self):
    result = compile_optimize(add(1, 2))
    expect = ((il.Return(((il.Return((None, None)),),)),),)
    eq_(result, expect)
