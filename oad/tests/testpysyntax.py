# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad.pysyntax import *
from oad.pysyntax import __getattr__, __call__, __getitem__
from oad.pysyntax import FormTraveller, lead_class
from oad.builtins.terminal import eof
from oad.testutil import x, y

class Test_Traveller:
  def test1(self):
    do = lead_class(FormTraveller)()
    do1 = do.write(1)
    eq_(do1.__syntax_data__, [(__getattr__, 'write'), (__call__, (1,), {})])
  def test2(self):
    do = lead_class(FormTraveller)()
    do1 = do.write[1]
    eq_(do1.__syntax_data__, [(__getattr__, 'write'), (__getitem__, 1)])
  def test3(self):
    g = element(getattr('a')&eof)
    g1 = g.a.b[1]
    eq_(g1.__syntax_data__, [(__getattr__, 'a'), (__getattr__, 'b'), (__getitem__, 1)])

class TestParse:
  def test1(self):
    g = element(getattr('a'))
    g1 = g.a.b[1]
    eq_(g1.__parse_syntax__(), True)
  def test2(self):
    g = element(getattr('a')&eof)
    g1 = g.a.b
    eq_(g1.__parse_syntax__(), None)
  def test3(self):
    g = element(attr_call('a')(x, y)&eof)
    g1 = g.a(1)
    eq_(g1.__parse_syntax__(), True)
    eq_(parse(g.a(1)), True)
  def test3(self):
    g = element(attr_call('a')(x, y)&eof&x)
    eq_(parse(g.a(1)), (1,))
