from nose.tools import eq_, ok_, assert_raises

from logicfun import *

def end(fc):
  print 'end'
  
class TestParse:
  def test_parse1(self):
    solve(parse('a', char('b')), lambda fc: fc)
  
class TestLogic:
  def test_succeed(self):
    solve(succeed, end)
    
  def test_fail(self):
    solve(fail, end)
    
  def test_and1(self):
    solve(and_(succeed, succeed), end)
    
  def test_and2(self):
    solve(and_(succeed, fail), end)
    
  def test_or1(self):
    solve(or_(fail, succeed), end)
  
  def test_or2(self):
    solve(or_(succeed, succeed), end)
  
  def test_or3(self):
    solve(or_(succeed, fail), end)
  
  def test_or4(self):
    solve(or_(prin(1), prin(2)), end)
  
  def test_findall(self):
    solve(findall(or_(prin(1), prin(2))), end)
  
  def test_unify(self):
    solve(unify(1,1), bindings, end)
    #solve(unify(x, 1), bindings, end)

