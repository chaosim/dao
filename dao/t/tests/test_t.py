from nose.tools import eq_, assert_raises

from dao.term import var
from dao.t.teval import teval

class Test_simple:
  def test1(self):
    eq_(teval('-1'), -1)
    eq_(teval('-123.5e-6'), -0.0001235)
    eq_(teval('"-1"'), "-1")
  def test1_2(self):
    eq_(teval('a'), var('a'))
  def test2_0(self):
    eq_(teval('+1 ; -1'), -1)
    eq_(teval('-123.5e-6; +1; -1'), -1)
    eq_(teval('"-1" ;  1'), 1)
  def test2(self):
    eq_(teval('a; a;'), var('a'))
  def test3(self):
    eq_(teval('a = 1;'), 1)
    eq_(teval('a = b = 1;'), 1)
  def test4(self):
    eq_(teval('1+2;'), 3)
  def test5(self):
    eq_(teval('1+2*3;'), 7)
  def test6(self):
    eq_(teval('a = 1; a--'), 0)
class Test_statement1:
  def test1(self):
    eq_(teval('print 1'), None)
  def test2(self):
    eq_(teval('let a = 1, b = 2 do: print a+b'), None)
  def test3(self):
    eq_(teval('if 1: print 1; else: print "other"'), None)
  def test4(self):
    eq_(teval('case 1: of 1: print 1; of 2: print 2; else: print "other"'), None)
  def test5(self):
    eq_(teval('{1; print "a";}'), None)
class Test_loop:
  def test1(self):
    eq_(teval('loop: print "loop" by 3 times'), None)
  def test2(self):
    eq_(teval('i = 0; loop: print "loop";  i++ until i==3'), None)
  def test3(self):
    eq_(teval('i = 0; loop: print "loop";  i++ when i<3'), None)
  def test4(self):
    eq_(teval('i = 0; while i<3: print "loop";  i++'), None)
  def test5(self):
    eq_(teval('i = 0; while i<3: pass;  i++ '), None)
  def test6(self):
    eq_(teval('i = 0; loop: i++; print i; if i==3: break'), None)
  def test7(self):
    eq_(teval('i = 0; block: i++; print i; if i<3: redo'), None)
    
class Test_Function:
  def test1(self):
    eq_(teval('fun a: \n   (): print 1; \n   (x): print x \na()'), None)
  def test2(self):
    eq_(teval('fun a = (): print 1\n a()'), None)
  def test3(self):
    eq_(teval('fun a + (): print 1 a()'), None)
  def test4(self):
    eq_(teval('fun a % (): print 1 a()'), None)
  def test5(self):
    eq_(teval('fun - a'), None)
  def test4(self):
    eq_(teval('fun - a/3'), None)
  def test5(self):
    eq_(teval('fun - a:()'), None)
