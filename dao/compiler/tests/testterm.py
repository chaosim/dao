# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.util import *

# demonstrate the idea to compile var and unify
class TestUnify:
  def test1(self):    
    compiler = make_compiler()
    compiler.compile(unify(x,1), "x = Var('x'); x.binding = 1")
  def test2(self):    
    compiler = make_compiler()
    compiler.compile(unify(x,y), "x = Var('x'); y = Var('y'), x.binding = y.binding if y.binding is not None else y")
