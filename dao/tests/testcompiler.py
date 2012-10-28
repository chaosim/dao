# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.interlang import *

from dao.compile import Compiler, LogicVar
from dao.compile import begin, if_, fail, succeed, or_, unify

from dao import interlang as il

v, fc = il.Var('v'), il.Var('fc')

class Done(Clamda):
  def __repr__(self): return 'done'
  
done = Done(v, fc, ret(1, fc))

def compile(exp):
  return Compiler().compile(exp, done, None)

class TestSimple:
  def test_integer(self):
    result = compile(1)
    expect = clamda(v, fc, 
                    done(1, None))
    eq_(result, expect)
    
  def test_begin(self):
    result = compile(begin(1, 2))
    expect = clamda(v, fc, 
                    clamda(v, fc, 
                           done(2, None))(1, None))
    eq_(result, expect)
  
  def test_if(self):
    result = compile(if_(0, 1, 2))
    expect = clamda(v, fc, 
                    clamda(v, fc, 
                           il.if_(v, clamda(v, fc, done(1, None)), 
                                  clamda(v, fc, done(2, None))))(0, None))
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
    expect = clamda(v, fc, clamda(v, fc, 
                                  done(1, clamda(v, clamda(v, fc, done(2, None)), ))))
    eq_(result, expect)
    
  def test_unify(self):
    eq_(compile(unify(1, 2)), None)    
    eq_(compile(unify(1, 1)), done) 
    
  def test_unify2(self):
    x = LogicVar('x')
    result = compile(unify(x, 2))
    expect = clamda(v, fc, ret(il.unify(x, 2, done, None)))
    eq_(result, expect)