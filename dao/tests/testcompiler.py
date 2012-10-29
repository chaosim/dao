# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.compiler.interlang import *

from dao.compiler.compile import Compiler
from dao.compiler.command import begin, quote, assign, if_, LogicVar
from dao.compiler.command import add
from dao.compiler.command import fail, succeed, or_, unify

from dao.compiler import interlang as il

v, fc = il.Var('v'), il.Var('fc')
a0, a1, a2, a3, a4 = tuple(il.Var('a'+repr(i)) for i in range(5))

class Done(Clamda):
  def __repr__(self): return 'done'
  
done = Done(v, fc, ret(1, fc))

def compile(exp):
  return Compiler().compile(exp, done, None)

def compile_args(args):
  return Compiler().compile_args(args, done, None)

class TestCompileArguments:
  def test_empty(self):
    result = compile_args(())
    expect = clamda(v, fc, 
                    done(v, fc))
    eq_(result, expect)

  def test1(self):
    result = compile_args((1,))
    expect = clamda(v, fc, 
                    done((1,), fc))
    eq_(result, expect)

class TestSimple:
  def test_integer(self):
    result = compile(1)
    expect = done(1, None)
    eq_(result, expect)
    
  def test_quote(self):
    result = compile(quote(1))
    expect = done(1, None)
    eq_(result, expect)
    
  def test_begin(self):
    result = compile(begin(1, 2))
    expect = clamda(v, fc, done(2, None))(1, None)
    eq_(result, expect)
  
  def test_assign(self):
    x = il.Var('x')
    result = compile(assign(x, 2))
    expect = clamda(v, fc, il.assign(x, v), ret(v, fc))(2, None)
    eq_(result, expect)

  def test_if(self):
    result = compile(if_(0, 1, 2))
    expect = clamda(v, fc, il.if_(v, done(1, None), done(2, None)))(0, None)
    eq_(result, expect)
  
  def test_fail(self):
    result = compile(fail)
    expect = None
    eq_(result, expect)

  def test_succeed(self):
    result = compile(succeed)
    expect = done
    eq_(result, expect)

  def test_or(self):
    result = compile(or_(1, 2))
    expect = clamda(v, fc, done(1, clamda(v, fc, done(2, None))))
    eq_(result, expect)
    
  def test_unify(self):
    eq_(compile(unify(1, 2)), None)    
    eq_(compile(unify(1, 1)), done) 
    
  def test_unify2(self):
    x = LogicVar('x')
    result = compile(unify(x, 2))
    expect = clamda(v, fc, ret(il.unify(x, 2, done, None)))
    eq_(result, expect)
    
    
  def test_add(self):
    result = compile(add(1, 2))
    expect = clamda(a0, fc, clamda(a1, fc, ret(done(il.add((a0, a1)), fc)))(2, None))(1, None)
    eq_(result, expect)
