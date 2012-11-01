# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.compiler.compile import Compiler, CodeGenerator
from dao.compiler.command import begin, quote, assign, if_, LogicVar
from dao.compiler.command import add
from dao.compiler.command import fail, succeed, or_, unify

from dao.compiler import interlang as il

def to_code(exp):
  coder = CodeGenerator()
  return coder.to_code(exp)

v, fc = il.Var('v'), il.Var('fc')
a0, a1, a2, a3, a4 = tuple(il.Var('a'+repr(i)) for i in range(5))

class Done(il.Clamda):
  def __repr__(self): return 'done()'
  
def done():
  return Done(v, fc, (il.Return(v, fc),))

def compile(exp):
  return to_code(Compiler().cps_convert(exp, done(), None))


class TestGenerateCode:
  def test_simple(self):
    eq_(to_code(1), '1')
    eq_(to_code(il.Var('a')), 'a')
    eq_(to_code(il.il.Return('a')), "return 'a'")
    eq_(to_code(il.assign(v, 1)), "v = 1")
    eq_(to_code(il.add((v, 1))), "v+1")
    
  def test_done(self):
    eq_(to_code(done()), 'def function1(v, fc):\n  return v, fc')
    #eq_(to_code(done()(1, None)), 'def function1(v, fc):\n  return v, fc\nfunction1(1, None)')
    
    
class TestCompileGenerateCode:
  def test_integer(self):
    result = compile(1)
    expect = "def function1(v, fc):\n  return v, fc\nfunction1(1, None)"
    eq_(result, expect)
    
  def test_quote(self):
    result = compile(quote(1))
    expect = "def function1(v, fc):\n  return v, fc\nfunction1(1, None)"
    eq_(result, expect)
    
  def test_begin(self):
    result = compile(begin(1, 2))
    expect = 'def function1(v, fc):\n  def function2(v, fc):\n    return v, fc\n  function2(2, None)\nfunction1(1, None)'
    eq_(result, expect)
  
  def test_assign(self):
    x = il.Var('x')
    result = compile(assign(x, 2))
    expect = 'def function1(v, fc):\n  x = v\n  return v, fc\nfunction1(2, None)'
    eq_(result, expect)

  def test_if(self):
    result = compile(if_(0, 1, 2))
    expect = '''def function1(v, fc):
  def function2(v, fc):
    return v, fc
  if v: 
    function2(1, None)
  else:
    function2(2, None)
function1(0, None)'''
    eq_(result, expect)
  
  def test_fail(self):
    result = compile(fail)
    expect = 'None'
    eq_(result, expect)

  def test_succeed(self):
    result = compile(succeed)
    expect = 'def function1(v, fc):\n  return v, fc'
    eq_(result, expect)

  def test_or(self):
    result = compile(or_(1, 2))
    expect = il.Clamda(v, fc, done()(1, Clamda(v, fc, done()(2, None))))
#def function1(v, fc):
  #def function2(v, fc):
    #return v, fc
  #function2(1, def function3(v, fc):
    #function2(2, None))    
    eq_(result, expect)
    
  def test_unify(self):
    eq_(compile(unify(1, 2)), 'None')    
    eq_(compile(unify(1, 1)), 'def function1(v, fc):\n  return v, fc') 
    
  def test_unify2(self):
    x = il.LogicVar('x')
    result = compile(unify(logicvar('x'), 2))
    expect = Clamda(v, fc, il.Return(il.unify(x, 2, done(), None)))
    eq_(result, expect)
    
    
  def test_add(self):
    result = compile(add(1, 2))
    expect = il.Clamda(a0, fc, il.Clamda(a1, fc, il.Return(done()(il.add((a0, a1)), fc)))(2, None))(1, None)
    eq_(result, expect)
