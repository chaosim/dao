# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.compilebase import Compiler, CodeGenerator
from dao.compilebase import generate_code
from dao.builtins import begin, quote, assign, if_ 
from dao.builtins import add
from dao.builtins import fail, succeed, or_, unify, LogicVar

from dao import interlang as il

v, fc = il.Var('v'), il.Var('fc')
a0, a1, a2, a3, a4 = tuple(il.Var('a'+repr(i)) for i in range(5))

class Done(il.Clamda):
  def __repr__(self): return 'done()'
  
def done():
  return Done(v, v)

def compile(exp):
  return generate_code(exp.cps_convert(Compiler(), done()))


class TestGenerateCode:
  def test_simple(self):
    eq_(generate_code(1), '1')
    eq_(generate_code(il.Var('a')), 'a')
    eq_(generate_code('a'), "'a'")
    eq_(generate_code(il.Assign(v, 1)), "v = 1")
    eq_(generate_code(il.add((v, 1))), "v+1")
    
  def test_done(self):
    eq_(generate_code(done()), 'lambda v: v')
    eq_(generate_code(done()(1)), '(lambda v: v)(1)')
    
    
class TestCompileGenerateCode:
  def test_integer(self):
    result = compile(1)
    expect = "(lambda v: v)(1)"
    eq_(result, expect)
    
  def test_quote(self):
    result = compile(quote(1))
    expect = "(lambda v: v)(1)"
    eq_(result, expect)
    
  def test_begin(self):
    result = compile(begin(1, 2))
    expect = '''(lambda v: (lambda v: v)(2))(1)'''
    eq_(result, expect)
  
  def test_assign(self):
    x = il.Var('x')
    result = compile(assign(x, 2))
    expect = '''def function(v):
  x = v
  v
function(2)'''
    eq_(result, expect)
    assert 0, 'need pythonization with return statement'

  def test_if(self):
    result = compile(if_(0, 1, 2))
    expect = '(lambda v: (lambda v: v)(1) if v else (lambda v: v)(2))(0)'
    eq_(result, expect)
  
  def test_fail(self):
    result = compile(fail)
    expect = 'solver.fail_cont(True)'
    eq_(result, expect)

  def test_succeed(self):
    result = compile(succeed)
    expect = '(lambda v: v)(True)'
    eq_(result, expect)

  def test_or(self):
    result = compile(or_(1, 2))
    expect = '''cut_or_cont = solver.cut_or_cont
solver.cut_or_cont = solver.fail_cont
x1 = solver.fail_cont
def x2(v):
  solver.fail_cont = x1
  (lambda v: (solver.cut_or_cont = cut_or_cont, (lambda v: v)(v)))(2)
solver.fail_cont = x2
(lambda v: (solver.cut_or_cont = cut_or_cont, (lambda v: v)(v)))(1)'''
    eq_(result, expect)
    assert 0, 'refine it again'
    
  def test_unify(self):
    eq_(compile(unify(1, 2)), 'solver.fail_cont(True)')    
    eq_(compile(unify(1, 1)), '(lambda v: v)(True)') 
    
  def test_unify2(self):
    x = il.LogicVar('x')
    result = compile(unify(x, 2))
    expect = '''solver.bindings[LogicVar('x')] = 2
x1 = solver.fail_cont
def x2(v):
  solver.fail_cont = x1
  del solver.bindings[LogicVar('x')]
solver.fail_cont = x2
(lambda v: v)(True)'''
    eq_(result, expect)
    
  def test_add(self):
    result = compile(add(1, 2))
    expect ='(lambda a0: (lambda a1: (lambda v: v)(a0+a1))(2))(1)'
    eq_(result, expect)