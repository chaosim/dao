# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad import *
from oad.builtins.format import write
from oad.term import Var

a, b, c = var.a.b.c
i, j = v.i, v.j
n = v.n

class TestVardeclare:
  def test1(self):
    ok_(isinstance(a, Var))    
    ok_(isinstance(b, Var))    
    ok_(isinstance(c, Var))    
    ok_(isinstance(i, Var))    

class TestDo:
  def test_do_until(self):
    do1 =  do.write(1).until(1)
    eq_(do1.forms,[write(1)])
    eq_(do1.until_conditions,[1])
    
class TestLoop:
  def test_LoopTimes1(self):
    loop1 = loop(100)[write(2)]
    eq_(loop1.forms, [write(2)])    
    eq_(loop1.times, 100)
  def test_LoopTimes1(self):
    loop1 = loop(100).write(2)
    eq_(loop1.forms, [write(2)])    
    eq_(loop1.times, 100)
  def test_Loop_forever1(self):
    loop1 =  loop[write(1)]
    eq_(loop1.forms, [write(1)])    

class TestAssignVariable:
  def test_assign1(self):
    put1 = put.i==100
    eq_(put1, SingleAssign('any_scope', i, 100))
  def test_assign2(self):
    put1 = put[i, my.j]==(100, 200)
    eq_(put1, MultipleAssign([('any_scope', i), ('local',j)], (100, 200)))
    
class TestLetForm:
  def test_let(self):
    let1 = let({a:1}).do[write(1)]
    eq_(let1, LetForm({a:1}, [write(1)]))
