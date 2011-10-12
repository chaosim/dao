# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad import *
from oad.term import Var
from oad.dexpr import *
from oad.dinpy import *
from oad import special

a, b, c = var.a.b.c
a, b, c = [a, b, c]
i, j = [v.i, v.j]
n = v.n
x, y = [v.x, v.y]

class TestVarDeclare:
  def test1(self):
    ok_(isinstance(a, VarSymbol))    
    ok_(isinstance(b, VarSymbol))    
    ok_(isinstance(c, VarSymbol))    
    ok_(isinstance(i, VarSymbol)) 
    
class Test_v_var:
  def test_v(self):
    x = v.a
    eq_(x.__class__, VarSymbol)
    eq_(parse(x), varcache('a'))

  def test_var(self):
    x = var.a.b.c
    eq_(parse(list(x)), 
        [varcache('a'),varcache('b'),varcache('c')])

