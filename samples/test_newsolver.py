from nose.tools import eq_, ok_, assert_raises

from newsolver import *
from newsolver import solve as _solve

x = Var('x')
_ = DummyVar('_')

def end(v, fc, bindings, parse_state):
  print 'end'

def done(v, fc, bindings, parse_state):
  print 'done'
  return v

def solve(goal):
  return _solve(goal, done, end)
  
class TestParse:
  def test_parse1(self):
    solve(parse('a', char('b')))
    
  def test_parse2(self):
    solve(parse('a', char('a')))
  
  def test_parse3(self):
    solve(parse('a', or_(char('b'), char('a'))))
  
  def test_parse4(self):
    solve(parse('ab', and_(char('a'), char('b'))))
    
  def test_parse5(self):
    solve(parse('ab', begin(char('a'), prin(1), char('b'))))
  
  def test_parse6(self):
    solve(parse('ab', begin(char('a'), integer(1), char('b'))))
  
class TestLisp:
  def test_if(self):
    solve(if_(integer(1), integer(2), integer(3)))
  def test_if2(self):
    solve(if_(integer(0), integer(2), integer(3)))
    
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
  
  def test_begin(self):
    solve(begin(prin(1), prin(2), prin(3)))
  
  def test_findall(self):
    solve(findall(or_(prin(1), prin(2))))
  
  def test_unify(self):
    solve(unify(1,1))
    
  def test_unify2(self):
    x = Var('x')
    solve(unify(x, 1))

  def test_unify3(self):
    x = Var('x')
    solve(begin(unify(x, 1), unify(x,2)))


class TestAny:
  def test0(self):
    solve(begin(set_text('ab'), any(char(_))))
    
  def test1(self):
    solve(begin(set_text('ab'), any(char(_)), eoi))
    
  def test2(self):
    solve(begin(set_text('abc'), findall(any(char(_)))))
    
  def test3(self):
    solve(begin(set_text('ab'), any(char(_)), char(_)))
    
  def test4(self):
    solve(begin(set_text('abc'), any(char(_)), char(_), char(_)))
    
  def test5(self):
    solve(begin(set_text('abc'), any(char( _)), char(_), char(_), eoi))
  
  def test6(self):
    solve(begin(set_text('abc'), findall(begin(any(char(_)), char(_), char(_)))))
    
  def test7(self):
    solve(begin(set_text('abcd'), findall(begin(any(char(_)), char(_), char(_), eoi))))
  
class TestLazyAny:
  def test0(self):
    solve(begin(set_text('ab'), lazy_any(char(_))))
    
  def test1(self):
    solve(begin(set_text('ab'), lazy_any(char(_)), eoi))
    
  def test2(self):
    solve(begin(set_text('abc'), findall(lazy_any(char(_)))))
    
  def test3(self):
    solve(begin(set_text('ab'), lazy_any(char(_)), char(_)))
    
  def test4(self):
    solve(begin(set_text('abc'), lazy_any(char(_)), char(_), char(_)))
    
  def test5(self):
    solve(begin(set_text('abc'), lazy_any(char( _)), char(_), char(_), eoi))
  
  def test6(self):
    solve(begin(set_text('abc'), findall(begin(lazy_any(char(_)), char(_), char(_)))))
    
  def test7(self):
    solve(begin(set_text('abcd'), findall(begin(lazy_any(char(_)), char(_), char(_), eoi))))

class TestGreedyAny:
  def test0(self):
    solve(begin(set_text('ab'), greedy_any(char(_))))
    
  def test1(self):
    solve(begin(set_text('ab'), greedy_any(char(_)), eoi))
    
  def test2(self):
    solve(begin(set_text('abc'), findall(greedy_any(char(_)))))
    
  def test3(self):
    solve(begin(set_text('ab'), greedy_any(char(_)), char(_)))
    
  def test4(self):
    solve(begin(set_text('abc'), greedy_any(char(_)), char(_), char(_)))
    
  def test5(self):
    solve(begin(set_text('abc'), greedy_any(char( _)), char(_), char(_), eoi))
  
  def test6(self):
    solve(begin(set_text('abc'), findall(begin(greedy_any(char(_)), char(_), char(_)))))
    
  def test7(self):
    solve(begin(set_text('abcd'), findall(begin(greedy_any(char(_)), char(_), char(_), eoi))))
