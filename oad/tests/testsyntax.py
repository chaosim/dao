# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad import *
from oad.term import Var

a, b = var.a.b
c = v.c

class TestVardeclare:
  def test1(self):
    ok_(isinstance(a, Var))    
    ok_(isinstance(b, Var))    
    ok_(isinstance(c, Var))    

class TestDo:
  def test1(self):
    do1 =  do.write(1).write(2)
    eq_(do1.body, [write(1), write(2)])
