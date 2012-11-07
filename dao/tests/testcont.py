# -*- coding: utf-8 -*-

pyset = set

from nose.tools import eq_, ok_, assert_raises

from dao.term import cons
from dao.solve import to_sexpression
from dao.builtins.arith import add, sub
from dao.builtins.term import define
from dao.special import *

# compiler
from dao.compile import compile_to_cont, Compiler
from dao import vop
from dao.cont import *

class Test_Cont_Dependency:
  def testValueCont(self):
    c1 = V(1, done)
    c2 = V(1, c1)
    eq_(done.depend_on(c1), True)    
    eq_(c1.depend_on(c2), False)    
    eq_(done.depend_on(c2), False)    
