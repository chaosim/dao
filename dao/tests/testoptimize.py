# -*- coding: utf-8 -*-
'''optimization'''

from nose.tools import eq_, ok_, assert_raises

from dao.compilebase import Compiler, Environment
from dao.builtins import begin, quote, assign, if_
from dao.builtins import add
from dao.builtins import fail, succeed, or_, unify
from dao.command import Var, LogicVar

from dao import interlang as il

v, fc = il.Var('v'), il.Var('fc')
v1, fc1 = il.Var('v1'), il.Var('fc1')
a0, a1, a2, a3, a4 = tuple(il.Var('a'+repr(i)) for i in range(5))

class Done(il.Clamda):
  def __repr__(self): return 'done()'
  
def done():
  return Done(v, v)

def compile_optimize(exp):
  compiler = Compiler()
  exp = il.element(exp).alpha_convert(Environment(), compiler)
  exp = exp.cps_convert(compiler, done())
  exp.analyse(compiler)
  return exp.optimize(compiler)

class TestSimple:
  def test_integer(self):
    result = compile_optimize(1)
    expect = 1
    eq_(result, expect)
    
  def test_quote(self):
    result = compile_optimize(quote(1))
    expect = il.ExpressionWithCode(il.Integer(1), il.Lamda((), il.Integer(1)))
    eq_(result, expect)
    
  def test_begin(self):
    result = compile_optimize(begin(1, 2))
    expect = 2
    eq_(result, expect)
  
  def test_assign(self):
    x = Var('x')
    result = compile_optimize(assign(x, 2))
    expect = il.Integer(2)
    eq_(result, expect)

  def test_if(self):
    result = compile_optimize(if_(0, 1, 2))
    expect = il.Integer(2)
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
    expect = 1
    eq_(result, expect)
    
  def test_unify(self):
    eq_(compile_optimize(unify(1, 2)), il.failcont(True))    
    eq_(compile_optimize(unify(1, 1)), True) 
    
  def test_unify2(self):
    x = LogicVar('x')
    result = compile_optimize(unify(x, 2))
    x1 = il.LogicVar('x')
    x2 = il.LocalVar('x')
    expect = il.begin(il.Assign(x2, il.Deref(LogicVar(x1))), 
                      il.If(il.IsLogicVar(x2), il.TRUE, il.If(il.Eq(x2, il.Integer(2)), il.TRUE, il.failcont(il.TRUE))))
    eq_(result, expect)
    
  def test_add(self):
    result = compile_optimize(add(1, 2))
    expect = il.Integer(3)
    eq_(result, expect)

def optimize(exp):
  compiler = Compiler()
  exp.analyse(compiler)
  return exp.optimize(compiler)

class TestOptimize:
  def test_if(self):
    x = il.Var('x')
    result = optimize(il.if_(x, il.if_(x, 2, 3), 4))
    expect = il.if_(x, 2, 4)
    eq_(result, expect)
    
  def test_if_2(self):
    x = il.Var('x')
    result = optimize(il.if_(x, 2, il.if_(x, 3, 4)))
    expect = il.if_(x, 2, 4)
    eq_(result, expect)
    
  def test_if_3(self):
    x = il.Var('x')
    result = optimize(il.if_(x, il.if_(x, 2, 3), il.if_(x, 4, 5)))
    expect = il.if_(x, 2, 5)
    eq_(result, expect)
    
  def test_if_4(self):
    eq_(optimize(il.if_(1, 2, 3)), il.Integer(2))
    eq_(optimize(il.if_(0, 2, 3)), il.Integer(3))
    
  def test_lambda_apply(self):
    result = optimize(il.clamda(v, 1)(v))
    expect = 1
    eq_(result, expect)
  
  def test_lambda_apply2(self):
    v1 = il.Var('v1')
    result = optimize(il.Clamda(v1, v1)(v))
    expect = v
    eq_(result, expect)
    
  def test_function(self):
    x = il.Var('x')
    f = il.Var('f')
    result = optimize(il.Apply(
      il.Function(f, (x, ), 
                  il.If(il.Eq(x, il.Integer(1)), 
                        il.Integer(1),
                        f(il.sub(x, il.Integer(1))))), (il.Integer(3),)))
    expect =f(il.Integer(2))
    eq_(result, expect)
    
  def test_function2(self):
    x = il.Var('x')
    f = il.Var('f')
    result = optimize(il.Begin((
      il.Assign(f, il.Lamda((x,), f(il.Integer(1)))), 
      il.Apply(f, (il.Integer(3),)))))
    expect = f(il.Integer(3))
    eq_(result, expect)
