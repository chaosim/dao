# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad import *
from oad.term import Var
from oad.dinpy.dexpr import *
from oad.dinpy.dexpr import _VarSymbol
from oad.dinpy.dinpy import *
from oad import special

a, b, c = var.a.b.c
a, b, c = [a, b, c]
i, j = [v.i, v.j]
n = v.n
x, y = [v.x, v.y]

class TestVarDeclare:
  def test1(self):
    ok_(isinstance(a, _VarSymbol))    
    ok_(isinstance(b, _VarSymbol))    
    ok_(isinstance(c, _VarSymbol))    
    ok_(isinstance(i, _VarSymbol)) 
    
class Test_v_var:
  def test_v(self):
    x = v.a
    eq_(x.__class__, _VarSymbol)
    eq_(preparse(x), varcache('a'))

  def test_var(self):
    x = var.a.b.c
    eq_(preparse(list(x)), 
        [varcache('a'),varcache('b'),varcache('c')])

class TestDecInc:
  def test_inc(self):
    eq_(preparse(++i),preparse(special.set(i, arith.add(i,1))))
    
  def test_dec(self):
    eq_(preparse(--i), preparse(special.set(i, arith.sub(i,1))))
    
class TestAssign:
  def test_single_assign(self):
    eq_(preparse(i<<1), preparse(special.set(i, 1)))
  def test_assign_chain(self):
    eq_(preparse(i<<j<<1), preparse(special.begin(special.set(j, 1),
                                            special.set(i, j))))
