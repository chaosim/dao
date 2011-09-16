# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad.syntax import *

class Test_Simple:
  def test_arg(self):
    ok_(run(arg, data, 1), 1)    

class Test_Complex:
  def test_iif1(self):
    ok_(run(iif(5>1).then[1].els[2]), 1)    
