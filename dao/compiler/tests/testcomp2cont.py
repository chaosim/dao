# -*- coding: utf-8 -*-

pyset = set

from nose.tools import eq_, ok_, assert_raises

from dao.term import cons
from dao.special import *

from dao.compiler.compile import compile_to_cont, make_compiler
from dao.compiler import vop
from dao.compiler.vop import done, return_
from dao.compiler.term import Var as CompileVar, vars as compile_vars

from dao.util import *

class TestSimple:
  def testInteger(self):
    value = CompileVar('value')
    eq_(compile_to_cont(1), return_(1, done))    
  def testVar(self):
    value, x1 = CompileVar('value'), CompileVar('x')
    eq_(compile_to_cont(x), vop.return_(x1, done)) 
  def testset(self):
    value, value_1, a1 = CompileVar('value'), CompileVar('value_1'), CompileVar('a')
    result = compile_to_cont(set(a, 2))
    expect = return_(2, clambda(value, begin(vop.set(a1, value), return_(value, done))))
    eq_(result, expect)    
    
class TestControl:
  def testBegin(self):
    value, = compile_vars('value')
    expect = return_(1, clambda(value, return_(2, done)))
    result = compile_to_cont(begin(1, 2))
    eq_(result,expect)
  def testif_(self):
    value,  = compile_vars('value')
    expect = return_(0, clambda(value, if_(value, return_(1,done), return_(2, done))))
    result = compile_to_cont(if_(0, 1, 2))
    eq_(result,expect)
  
class TestLambda:
  def testLambda1(self):
    k, x1 = compile_vars('k, x')
    expect = return_(lambda_((k, x1), return_(1, k)), done)
    result = compile_to_cont(lambda_([x], 1))
    eq_(result,expect)
    
  def testLambda2(self):
    k, x1, function, arg = compile_vars('k, x, function, arg')
    expect = return_(lambda_((k, x1), return_(1, k)), 
                clambda(function, return_(2, clambda(arg, return_(vop.call(function, (done, arg)),)))))
    result = compile_to_cont(lambda_([x], 1)(2))
    eq_(result,expect)
        
class TestLet:
  def testlet(self):
    x1, value, value_1, value_2 = compile_vars('x, value, value_1, value_2')
    expect = return_(1, clambda(value_1, 
                      begin(vop.set(x1, value_1),return_(value_1, clambda(value,return_(x1, done))))))
    result = compile_to_cont(let([(x,1)], x))
    eq_(result,expect)

  def testlet2(self):
    x1, y1, value, value_1, value_2, value_3, value_4, value_5 = compile_vars(
      'x, y, value, value_1, value_2, value_3, value_4, value_5')
    expect = return_(1, lambda_((value_5,),
              begin(vop.set(x1, value_5), return_(value_5, clambda(value, return_(2, lambda_((value_3,), 
              begin(vop.set(y1, value_3), return_(value_3, clambda(value_1, 
                    return_(x1, clambda(value_2, return_(y1, done)))))))))))))
    result = compile_to_cont(let([(x,1),(y,2)], x, y))
    eq_(result,expect)

  def testlet3(self):
    x1, x1_1, value, value_1, value_2, value_3, value_4, value_5 = compile_vars(
      'x, x_1, value, value_1, value_2, value_3, value_4, value_5')
    expect = return_(1, clambda(value_5,
            begin(vop.set(x1, value_5),return_(value_5, clambda(value, return_(2, lambda_((value_3,),
            begin(vop.set(x1_1, value_3),return_(value_3, 
                      clambda(value_2, return_(x1_1, clambda(value_1, return_(x1, done)))))))))))))
    result = compile_to_cont(let([(x, 1)], let([(x,2)], x), x))
    eq_(result,expect)

class TestLetr:
  def testletr(self):
    f1, k, value, value_1, function, function_1 = compile_vars('f, k, value, value_1, function, function_1')
    expect = return_(clambda(k, return_(f1, clambda(function_1, return_(vop.call(function_1, k),)))),
             clambda(value_1, begin(vop.set(f1, value_1), return_(value_1, clambda(value, 
             return_(f1, clambda(function, return_(vop.call(function, done)))))))))
    result = compile_to_cont(letr([(f, lambda_((), f()))], f()))
    eq_(result,expect)
