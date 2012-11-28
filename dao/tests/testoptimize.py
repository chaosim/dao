# -*- coding: utf-8 -*-
'''optimization'''

from nose.tools import eq_, ok_, assert_raises

from dao.compilebase import Compiler, Environment, OptimizationData
from dao.compile import optimize
from dao.builtins import begin, quote, assign, if_
from dao.builtins import add
from dao.builtins import fail, succeed, or_, unify, LogicVar

from dao import interlang as il

v, fc = il.Var('v'), il.Var('fc')
v1, fc1 = il.Var('v1'), il.Var('fc1')
a0, a1, a2, a3, a4 = tuple(il.Var('a'+repr(i)) for i in range(5))

class Done(il.Clamda):
  def __repr__(self): return 'done()'
  
def done():
  return Done(v, v)

def compile_optimize(exp):
  exp = exp.cps_convert(Compiler(), done())
  exp = exp.alpha_convert(Environment(), Compiler())
  optimize_data = OptimizationData()
  exp.optimization_analisys(optimize_data)
  return optimize(exp, optimize_data)

class TestSimple:
  def test_integer(self):
    result = compile_optimize(1)
    expect = 1
    eq_(result, expect)
    
  def test_quote(self):
    result = compile_optimize(quote(1))
    expect = 1
    eq_(result, expect)
    
  def test_begin(self):
    result = compile_optimize(begin(1, 2))
    expect = 2
    eq_(result, expect)
  
  def test_assign(self):
    x = il.Var('x')
    result = compile_optimize(assign(x, 2))
    expect = (il.Assign(x, 2), 2)
    eq_(result, expect)

  def test_if(self):
    result = compile_optimize(if_(0, 1, 2))
    expect = il.If(0, 1, 2)
    eq_(result, expect)
  
  def test_fail(self):
    result = compile_optimize(fail)
    expect = il.failcont(True)
    eq_(result, expect)

  def test_succeed(self):
    result = compile_optimize(succeed)
    expect = True
    eq_(result, expect)

  def test_or(self):
    cut_or_cont = il.Var('cut_or_cont')
    v1 = il.Var('v1')
    result = compile_optimize(or_(1, 2))
    expect = (il.Assign(cut_or_cont, il.failcont), 
              il.SetCutOrCont(il.failcont), 
              il.AppendFailCont(
                il.Clamda(v, 
                          il.SetCutOrCont(cut_or_cont), 
                          Done(v1, v1)(v))
                (2)), 
              (il.SetCutOrCont(cut_or_cont), 1))
    eq_(result, expect)
    
  def test_unify(self):
    eq_(compile_optimize(unify(1, 2)), il.failcont(True))    
    eq_(compile_optimize(unify(1, 1)), True) 
    
  def test_unify2(self):
    x = LogicVar('x')
    result = compile_optimize(unify(x, 2))
    expect = (il.SetBinding(x, 2),
              il.AppendFailCont(il.DelBinding(x)), 
              True)
    eq_(result, expect)
    
  def test_add(self):
    result = compile_optimize(add(1, 2))
    expect = il.add((1, 2))
    eq_(result, expect)

def optimize_it(exp):
  data = OptimizationData()
  exp.optimization_analisys(data)
  return optimize(exp, data)

class TestOptimize:
  def test_if(self):
    result = optimize_it(il.if_(1, il.if_(1, 2, 3), 4))
    expect = il.if_(1, 2, 4)
    eq_(result, expect)
    
  def test_if_2(self):
    result = optimize_it(il.if_(1, 2, il.if_(1, 3, 4)))
    expect = il.if_(1, 2, 4)
    eq_(result, expect)
    
  def test_if_3(self):
    result = optimize_it(il.if_(1, il.if_(1, 2, 3), il.if_(1, 4, 5)))
    expect = il.if_(1, 2, 5)
    eq_(result, expect)
    
  def test_lambda_apply(self):
    result = optimize_it(il.clamda(v, 1)(v))
    expect = 1
    eq_(result, expect)
  
  def test_lambda_apply2(self):
    v1 = il.Var('v1')
    result = optimize_it(il.Clamda(v1, v1)(v))
    expect = v
    eq_(result, expect)
    