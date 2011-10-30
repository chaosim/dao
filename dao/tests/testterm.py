# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.term import cons

from dao.util import *

from dao.solve import set_run_mode, noninteractive
set_run_mode(noninteractive)

class TestSimple:
  def testAtom(self):
    eq_(signature((1,)), ((False,1),))
    eq_(signature((a,)), ((True, None),))
    
class TestSimple:
  def testAtom(self):
    eq_(signature((1,)), signature((1,)))
    eq_(signature((a,)), signature((b,)))
    
  