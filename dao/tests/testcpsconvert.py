# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

import dao
from dao.compilebase import Compiler, Environment, VariableNotBound
from dao.builtins import begin, quote, assign, if_, let, letrec
from dao.builtins import add
from dao.builtins import fail, succeed, or_, unify, repeat, any, nongreedy, LogicVar
from dao.builtins import lamda, add
from dao.command import Var

from dao import interlang as il

v, fc = il.Var('v'), il.Var('fc')
a0, a1, a2, a3, a4 = tuple(il.Var('a'+repr(i)) for i in range(5))

class Done(il.Clamda):
  def __repr__(self): return 'done()'
  
def done():
  return Done(v, v)
  
def cps(exp):
  return il.element(exp).cps(Compiler(), done())

class TestCPSConvert:
  def test_integer(self):
    result = cps(1)
    expect = done()(1)
    eq_(result, expect)
    
  def test_quote(self):
    result = cps(quote(1))
    expect = done()(1)
    eq_(result, expect)
    
  def test_begin(self):
    result = cps(begin(1, 2))
    expect = il.Clamda(v, done()(2))(1)
    eq_(result, expect)
  
  def test_assign(self):
    x = Var('x')
    result = cps(assign(x, 2))
    expect = 1
    eq_(result, expect)

  def test_if(self):
    result = cps(if_(0, 1, 2))
    expect = il.Clamda(v, il.If(v, done()(1), done()(2)))(0)
    eq_(result, expect)
  
  def test_fail(self):
    result = cps(fail)
    expect = il.failcont(True)
    eq_(result, expect)

  def test_succeed(self):
    result = cps(succeed)
    expect = done()(True)
    eq_(result, expect)
    
  def test_repeat(self):
    function = il.Var('function')
    result = cps(repeat)
    expect = il.begin(
       il.SetFailCont(function), 
       il.CFunction(function, v, done()(v)))
    eq_(result, expect)

  def test_or(self):
    cut_or_cont = il.Var('cut_or_cont')
    result = cps(or_(1, 2))
    expect = 1
    eq_(result, expect)
    
  def test_unify(self):
    eq_(cps(unify(1, 2)), il.failcont(True))    
    eq_(cps(unify(1, 1)), done()(True))
    
  def test_unify2(self):
    x = LogicVar('x')
    result = cps(unify(x, 2))
    expect = 1
    eq_(result, expect)
    
  def test_unify3(self):
    x = il.Var('x')
    result = cps(unify(x, 2))
    expect = 1
    eq_(result, expect)
    
  def test_add(self):
    result = cps(add(1, 2))
    expect = il.Clamda(a0, il.Clamda(a1, done()(il.add((a0, a1))))(2))(1)
    eq_(result, expect)

  def test_lambda(self):
    x, y, k = Var('x'), Var('y'), il.Var('k')
    result = cps(lamda((x,y), 1))
    expect = done()(lamda((x, y, k), k(1)))
    eq_(result, expect)
    
  def test_let(self):
    x, y, k = il.Var('x'), il.Var('y'), il.Var('k')
    result = cps(let(((x,1),), x))
    expect = il.Clamda(x, done()(x))(1)
    eq_(result, expect)
    
  def test_letrec(self):
    f, k, function = il.Var('f'), il.Var('k'), il.Var('function')
    result = cps(letrec([(f, lamda((), f()))], f()))
    expect = il.Clamda(v, 
                       il.Assign(f, v), 
                       v)(
                         il.Lamda((k,), il.Clamda(function, function(k))(f)))
    eq_(result, expect)
    
from dao.builtins import eoi, char, findall

class TestBuiltin:
  def test_eoi(self):
    x = il.Var('x')
    result = cps(eoi)
    expect = 1
    eq_(result, expect)

  def test_char(self):
    text, pos = il.Var('text'), il.Var('pos')
    result = cps(char('a'))
    expect = 1
    eq_(result, expect)

  def test_char2(self):
    x = Var('x')
    text, pos = il.Var('text'), il.Var('pos')
    result = cps(char(x))
    expect = 2
    eq_(result, expect)

  def test_findall(self):
    cut_or_cont = il.Var('cut_or_cont')
    result = cps(findall(or_(1, 2)))
    expect = 1
    eq_(result, expect)
    
  def test_findall2(self):
    cut_or_cont = il.Var('cut_or_cont')
    x, y = Var('x'), Var('y')
    findall_result = il.Var('findall_result')
    result = cps(findall(or_(1, 2), x, y))
    expect = 1
    eq_(result, expect)
    
  def test_any(self):
    any_cont = il.Var('any_cont')
    result = cps(any(1, nongreedy))
    expect = 1
    eq_(result, expect)
      
  def test_any2(self):
    text, pos = il.Var('text'), il.Var('pos')
    any_cont = il.Var('any_cont')
    result = cps(any(char('1')))
    expect = 1
    eq_(result, expect)
  