# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.pysyntax import *
from dao.pysyntax import __getattr__, __call__, __getitem__
from dao.pysyntax import FormTraveller, lead_class
from dao.builtins.terminal import eoi
from dao.util import x, y

from dao.solve import set_run_mode, noninteractive
set_run_mode(noninteractive)

class Test_Traveller:
  def test1(self):
    do = lead_class(FormTraveller)('do', None)
    do1 = do.write(1)
    eq_(do1.__operator_data__, [(__getattr__, 'write'), (__call__, (1,), {})])
  def test2(self):
    do = lead_class(FormTraveller)('do', None)
    do1 = do.write[1]
    eq_(do1.__operator_data__, [(__getattr__, 'write'), (__getitem__, 1)])
  def test3(self):
    g = element('g', getattr('a')&eoi)
    g1 = g.a.b[1]
    eq_(g1.__operator_data__, [(__getattr__, 'a'), (__getattr__, 'b'), (__getitem__, 1)])

class TestParse:
  def test1(self):
    g = element('g', getattr('a'))
    g1 = g.a.b[1]
    eq_(preparse(g1), True)
  def test2(self):
    g = element('g',getattr('a')&eoi)
    g1 = g.a.b
    assert_raises(DinpySyntaxError, preparse, g1)
  def test3(self):
    g = element('g',attr_call('a')(x, y)&eoi)
    g1 = g.a(1)
    eq_(preparse(g1), True)
  def test4(self):
    g = element('g',attr_call('a')(x, y)&eoi&x)
    eq_(preparse(g.a(1)), (1,))
