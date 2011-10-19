# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.syntax import *

def syntax_data(dict):
  from dao.syntax import __syntax__data__
  __syntax__data__.clear()
  __syntax__data__.update(dict)
  
class Test_Simple:
  def test_arg(self):
    eq_(run(arg, data, 1), 1)    
    eq_(run(arg, data, 2), 2)    
  def test_data(self):
    eq_(run(data, 1, 2), 1)    
    eq_(run(data, 2, 2), 2)    
  def test_see_var(self):
    syntax_data({'a':1, 'b':2})
    eq_(run(see.a, 1, 2), True)    
    eq_(run(see.c, 2, 2), False)    
    eq_(run(see.a.b, 2, 2), True)    
    eq_(run(see.a.c, 2, 2), False)    
  def test_getvar(self):
    syntax_data({'a':1, 'b':2})
    eq_(run(get.a, 1, 2), 1)    
    eq_(run(get.b, 2, 2), 2)    
    eq_(run(get.a.b, 2, 2), [1,2])    
  def test_have_get(self):
    syntax_data({'a':1, 'b':2})
    run(have.a==3, 2, 2)
    eq_(run(get.a, 2, 2), 3)    
    run(have.c.d==(4,5), 2, 2)
    eq_(run(get.d, 2, 2), 5)    
  def test_append(self):
    syntax_data({'a':[1], 'b':2})
    run(append.a<<2, 2, 2)
    eq_(run(get.a, 2, 2), [1,2]) 
  def test_apply(self):
    result = run(apply[operator.eq](data, arg), 2, 2)
    eq_(result, True) 
  def test_args_len(self):
    eq_(run(args_len, 'data', 1,2,3), 3) 
    eq_(run(args_len_eq(2), 'data', 1, 2), True) 
  def test_iterator(self):
    iter1 = run(iterator([1,2]), 'data', 1,2,3)
    eq_(iter1.next(), 1) 
    eq_(iter1.next(), 2) 

class Test_Complex:
  def test_iif1(self):
    assert_raises(PyMetaSyntaxError, run, iif, 'data', 1)    
    assert_raises(PyMetaSyntaxError, run, iif(1), 'data', 1)    
    assert_raises(PyMetaSyntaxError, run, iif(1).then, 'data', 1)    
  def test_iif2(self):
    assert_raises(PyMetaSyntaxError, iif(1).getattr, 'els')    
    assert_raises(PyMetaSyntaxError, iif(1).then[1], 2)    
  def test_iif3(self):
    eq_(run(iif(0).then[1].elsif(0).then[2].els[4, 5], 'data', 1,2,3), 5)    
