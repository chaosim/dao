# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad.solve import parse

from oad.term import cons
from oad.solve import eval
from oad.special import quote, set, begin, if_, lambda_, let, letrec, eval_
from oad.special import function, macro, block, return_from, catch, throw
from oad.special import unwind_protect, module, loop
from oad.special import LoopForm, LoopTimesForm, exit, next

from oad.builtins.control import and_, cut
from oad.builtins.module import from_
from oad.builtins.format import write
from oad.builtins.arith import eq, sub, mul, add, div
from oad.builtins.arithpred import define
from oad.builtins.callcc import callcc

from oad.testutil import *

class TestSimple:
  def testInteger(self):
    eq_(parse(1), (1))    
  def testquote(self):
    eq_(parse(quote(x)), quote(x))
  def testset(self):
    eq_(parse(set(a,2)), set(a,2))
  def testbegin(self):
    eq_(parse(begin(1,2)), begin(1,2))

class TestLoopForm:
  def test_loop1(self):
    eq_(parse(LoopForm((1, 2, exit()))), 
        block('exit_label1', 
          loop(block('next_label2', 1,2, return_from('exit_label1')))))    
  def test_loop2(self):
    eq_(parse(LoopForm((1, next(), 2))), 
        block('exit_label1', 
          loop(block('next_label2', 1, return_from('next_label2'),2)))) 
    
class TestLoopTimesForm:
  def test_loop1(self):
    print parse(LoopTimesForm(3, (write(i), exit())))
##    eq_(parse(LoopTimesForm(3, (write(i), exit()))), 
##        block('exit_label1',set(i, 3), 
##          loop(block('next_label2', 
##                     if_(eq(i,1), return_from('exit_label1')), set(i, i-1),
##                     write(i), return_from('exit_label1')))))    
  def test_loop2(self):
    print parse(LoopTimesForm(3, (1, next(), 2)))
##    eq_(parse(LoopTimesForm(3, (1, next(), 2))), 
##        block('exit_label1',set(i, 3), 
##          loop(block('next_label2', 
##                     if_(eq(i,1), return_from('exit_label1')), set(i, i-1),
##                     write(i), return_from('next_label2')))))    