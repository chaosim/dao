# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad.solve import preparse, tag_loop_label

from oad.term import cons
from oad.solve import eval
from oad.solve import set_run_mode, noninteractive
set_run_mode(noninteractive)
from oad.special import quote, set, begin, if_, lambda_, let, letrec, eval_
from oad.special import function, macro, block, exit_block, continue_block
from oad.special import catch, throw
from oad.special import unwind_protect, module, from_
from oad.special import LoopForm, LoopTimesForm, exit, next

from oad.builtins.control import and_p, cut, callcc
from oad.builtins.io import write
from oad.builtins.arith import eq, sub, mul, add, div
from oad.builtins.term import define

from oad.util import *

class TestSimple:
  def testInteger(self):
    eq_(preparse(1), (1))    
  def testquote(self):
    eq_(preparse(quote(x)), quote(x))
  def testset(self):
    eq_(preparse(set(a,2)), set(a,2))
  def testbegin(self):
    eq_(preparse(begin(1,2)), begin(1,2))

class TestTagLoopForm:
  def test_loop1(self):
    eq_(tag_loop_label(LoopForm((1, 2, exit()))), 
        block('$1', 1, 2,exit_block('$1'), continue_block('$1')))    
  def test_loop2(self):
    eq_(tag_loop_label(LoopForm((1, next(), 2))), 
        block('$1', 1, continue_block('$1'), 2, continue_block('$1')))
    
class TestTagLoopTimesForm:
  def test_loop1(self):
    print tag_loop_label(LoopTimesForm(3, (write(i), exit())))
  def test_loop2(self):
    print tag_loop_label(LoopTimesForm(3, (1, next(), 2)))
