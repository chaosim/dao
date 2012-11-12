# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.compilebase import Compiler, CodeGenerator
from dao.compile import to_code, cps
from dao.command import begin, quote, assign, if_, LogicVar
from dao.command import add
from dao.command import fail, succeed, or_, unify

from dao import interlang as il

v, fc = il.Var('v'), il.Var('fc')
a0, a1, a2, a3, a4 = tuple(il.Var('a'+repr(i)) for i in range(5))

class Done(il.Clamda):
  def __repr__(self): return 'done()'
  
def done():
  return Done(v, v)

def compile(exp):
  return to_code(cps(Compiler(), exp, done()))


class TestGenerateCode:
  def test_simple(self):
    eq_(to_code(1), '1')
    eq_(to_code(il.Var('a')), 'a')
    eq_(to_code('a'), "'a'")
    eq_(to_code(il.Assign(v, 1)), "v = 1")
    eq_(to_code(il.add((v, 1))), "v+1")
    
  def test_done(self):
    eq_(to_code(done()), 'lambda v: v')
    eq_(to_code(done()(1, None)), '(lambda v: v)(1)')
    
    
class TestCompileGenerateCode:
  def test_integer(self):
    result = compile(1)
    expect = "(lambda v, fc: (v, fc))(1, None)"
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
  return v
function(2)'''
    eq_(result, expect)

  def test_if(self):
    result = compile(if_(0, 1, 2))
    expect = '''(lambda v, fc: ((lambda v, fc: (v, fc))(1, None) if v else (lambda v, fc: (v, fc))(2, None)))(0, None)'''
    eq_(result, expect)
  
  def test_fail(self):
    result = compile(fail)
    expect = 'None'
    eq_(result, expect)

  def test_succeed(self):
    result = compile(succeed)
    expect = 'lambda v: v'
    eq_(result, expect)

  def test_or(self):
    result = compile(or_(1, 2))
    expect = '''(lambda v: v)(1, (lambda v: v)(2))'''
    eq_(result, expect)
    
  def test_unify(self):
    eq_(compile(unify(1, 2)), 'None')    
    eq_(compile(unify(1, 1)), 'lambda v: v') 
    
  def test_unify2(self):
    x = il.LogicVar('x')
    result = compile(unify(x, 2))
    expect = '''unify(LogicVar('x'), 2, lambda v: v)'''
    eq_(result, expect)
    
    
  def test_add(self):
    result = compile(add(1, 2))
    expect ='''(lambda a0: ((lambda a1: ((lambda v: v)(a0+a1)))(2)))(1)'''
    eq_(result, expect)
    
