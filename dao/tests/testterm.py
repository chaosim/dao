# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.term import cons, signature

from dao.util import *

from dao.solve import set_run_mode, noninteractive
set_run_mode(noninteractive)



#(a,1)   (1,1)
#(a,2)
#(1,a)
#(2,a)
#(1,1)
#(1,2)
#(2,1)
#(2,2)
#(1,3)
#(2,3)

class TestSimple:
  def testAtom(self):
    eq_(signature(1), 1)
  def testVar(self):
    eq_(signature(a), Var)
    
  def testMutual(self):
    ok_(signature(1)!=signature(2))
    eq_(signature(a), signature(b))
    
class TestMatchRuleHead:
  def testMutual(self):
    rules = {(True, None):[1,2]}
    eq_(signature((1,)), signature((1,)))
    eq_(signature((a,)), signature((b,)))
