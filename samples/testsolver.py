from nose.tools import eq_, ok_, assert_raises

from solve import *
from solve import solve as _solve

def end(v):
  print 'end'

def done(v):
  print 'done'
  return v

def solve(goal):
  _solve(goal, done, end)
  
class TestParse:
  def test_parse1(self):
    solve(parse('a', char('b')))
    
  def test_parse2(self):
    solve(parse('a', char('a')))
  
  def test_parse3(self):
    solve(parse('a', or_(char('b'), char('a'))))
  
  def test_parse4(self):
    solve(parse('ab', and_(char('a'), char('b'))))
  
class TestLogic:
  def test_succeed(self):
    solve(succeed)
    
  def test_fail(self):
    solve(fail)
    
  def test_and1(self):
    solve(and_(succeed, succeed))
    
  def test_and2(self):
    solve(and_(succeed, fail))
    
  def test_not1(self):    
    solve(not_(fail))
    
  def test_not2(self):
    solve(not_(succeed))
    
  def test_or1(self):
    solve(or_(fail, succeed))
  
  def test_or2(self):
    solve(or_(succeed, succeed))
  
  def test_or3(self):
    solve(or_(succeed, fail))
  
  def test_or4(self):
    solve(or_(prin(1), prin(2)))
  
  def test_findall(self):
    solve(findall(or_(prin(1), prin(2))))
  
  def test_unify(self):
    solve(unify(1,1), bindings)
    #solve(unify(x, 1), bindings)

