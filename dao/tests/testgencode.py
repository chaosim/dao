# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.compilebase import Compiler, Compiler, Environment
from dao.builtins import begin, quote, assign, if_ 
from dao.builtins import add
from dao.builtins import fail, succeed, or_, unify, LogicVar, Var

from dao import interlang as il

v, fc = il.Var('v'), il.Var('fc')
a0, a1, a2, a3, a4 = tuple(il.Var('a'+repr(i)) for i in range(5))

class Done(il.Clamda):
  def __repr__(self): return 'done()'
  
def done():
  return Done(v, v)

def generate_code(exp):
  exps, has_statement = il.element(exp).pythonize(Environment(), Compiler())
  compiler = Compiler()
  return begin(*exps).to_code(compiler)

def compile(exp):
  compiler = Compiler()
  exp = il.element(exp).cps_convert(compiler, done())
  exp = exp.optimize(compiler)
  exps, has_statement = exp.pythonize(Environment(), compiler)
  compiler = Compiler()
  return begin(*exps).to_code(compiler)

class TestGenerateCode:
  def test_simple(self):
    eq_(generate_code(1), '1')
    eq_(generate_code(il.Var('a')), 'a')
    eq_(generate_code('a'), "'a'")
    eq_(generate_code(il.Assign(v, il.Integer(1))), "v = 1")
    eq_(generate_code(il.add(v, il.Integer(1))), "(v)+(1)")
    
  def test_done(self):
    eq_(generate_code(done()), 'lambda v: v')
    eq_(generate_code(done()(1)), '(lambda v: v)(1)')
    
    
class TestCompileGenerateCode:
  def test_integer(self):
    result = compile(1)
    expect = "1"
    eq_(result, expect)
    
  def test_quote(self):
    result = compile(quote(1))
    expect = "ExpressionWithCode((1), (lambda : 1))"
    eq_(result, expect)
    
  def test_begin(self):
    result = compile(begin(1, 2))
    expect = '2'
    eq_(result, expect)
  
  def test_assign(self):
    x = Var('x')
    result = compile(assign(x, 2))
    expect = '2'
    eq_(result, expect)
    #assert 0, 'need pythonization with return statement'

  def test_if(self):
    result = compile(if_(0, 1, 2))
    expect = '2'
    eq_(result, expect)
  
  def test_fail(self):
    result = compile(fail)
    expect = 'solver.fail_cont(True)'
    eq_(result, expect)

  def test_succeed(self):
    result = compile(succeed)
    expect = 'True'
    eq_(result, expect)

  def test_or(self):
    result = compile(or_(1, 2))
    expect = '1'
    eq_(result, expect)
    
  def test_unify(self):
    eq_(compile(unify(1, 2)), 'solver.fail_cont(True)')    
    eq_(compile(unify(1, 1)), 'True') 
    
  def test_unify2(self):
    x = il.LogicVar('x')
    result = compile(unify(x, 2))
    expect = 'solver.fail_cont(True)'
    eq_(result, expect)
    
  def test_add(self):
    result = compile(add(1, 2))
    #expect ='(lambda a0: (lambda a1: (lambda v: v)(a0+a1))(2))(1)'
    expect = '3'
    eq_(result, expect)