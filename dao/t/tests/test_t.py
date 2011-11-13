from nose.tools import eq_, assert_raises

from dao.term import var
from dao.t.teval import teval

class Test_teval:
  def test1(self):
    eq_(teval('-1'), -1)
    eq_(teval('-123.5e-6'), -0.0001235)
    eq_(teval('"-1"'), "-1")
    eq_(teval('a'), var('a'))
  def test2(self):
    eq_(teval('+1 ; -1'), -1)
    eq_(teval('-123.5e-6; +1; -1'), -1)
    eq_(teval('"-1" ;  1'), 1)
    #eq_(teval('a; a;'), var('a'))
  def test3(self):
    eq_(teval('a = 1;'), 1)
  def test6(self):
    eq_(teval('a--'), 'a')
