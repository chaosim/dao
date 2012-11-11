# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.compile import Compiler
from dao.gencode import CodeGenerator, to_code
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
  return to_code(Compiler().cps(exp, done(), None))


class TestGenerateCode:
  def test_simple(self):
    eq_(to_code(1), '1')
    eq_(to_code(il.Var('a')), 'a')
    eq_(to_code('a'), "'a'")
    eq_(to_code(il.Assign(v, 1)), "v = 1")
    eq_(to_code(il.add((v, 1))), "v+1")
    
  def test_done(self):
    eq_(to_code(done()), 'lambda v, fc: (v, fc)')
    eq_(to_code(done()(1, None)), '(lambda v, fc: (v, fc))(1, None)')
    
    
class TestCompileGenerateCode:
  def test_integer(self):
    result = compile(1)
    expect = "(lambda v, fc: (v, fc))(1, None)"
    eq_(result, expect)
    
  def test_quote(self):
    result = compile(quote(1))
    expect = "(lambda v, fc: (v, fc))(1, None)"
    eq_(result, expect)
    
  def test_begin(self):
    result = compile(begin(1, 2))
    expect = '''(lambda v, fc: ((lambda v, fc: (v, fc))(2, None)))(1, None)'''
    eq_(result, expect)
  
  def test_assign(self):
    x = il.Var('x')
    result = compile(assign(x, 2))
    expect = '''def function(v, fc):
  x = v
  return v, fc
function(2, None)'''
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
    expect = 'lambda v, fc: (v, fc)'
    eq_(result, expect)

  def test_or(self):
    result = compile(or_(1, 2))
    expect = '''(lambda v, fc: (v, fc))(1, (lambda v, fc: (v, fc))(2, None))'''
    eq_(result, expect)
    
  def test_unify(self):
    eq_(compile(unify(1, 2)), 'None')    
    eq_(compile(unify(1, 1)), 'lambda v, fc: (v, fc)') 
    
  def test_unify2(self):
    x = il.LogicVar('x')
    result = compile(unify(x, 2))
    expect = '''unify(LogicVar('x'), 2, lambda v, fc: (v, fc), None)'''
    eq_(result, expect)
    
    
  def test_add(self):
    result = compile(add(1, 2))
    expect ='''(lambda a0, fc: ((lambda a1, fc: ((lambda v, fc: (v, fc))(a0+a1, fc)))(2, None)))(1, None)'''
    eq_(result, expect)
    
