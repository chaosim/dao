from nose.tools import eq_, assert_raises

from dao.util import *
from dao.special import *

from dao.term import Var, conslist as L
from dao.solve import eval, NoSolutionFound
from dao.builtins.arith import eq, sub, mul, add, div
from dao.builtins.control import succeed, fail, or_p, and_p, not_p, repeat
from dao.builtins.control import findall, call, once
from dao.builtins.parser import set_text
from dao.builtins.terminal import char
from dao.builtins.term import unify, notunify
from dao.builtins.io import write
from dao.builtins.term import ground
from dao.builtins.term import isvar, nonvar, is_

from dao.solve import set_run_mode, noninteractive
set_run_mode(noninteractive)

class TestControl:
  def test_fail(self):
    eq_(eval(let([(f,function([[1], fail], [[x], succeed]))], f(x))), True)

  def test_succeed(self):
    eq_(eval(let([(f,function([[1], succeed]))], f(x))), True)

  def test_Or(self):
    eq_(eval(or_p(fail, succeed)), True)
    
  def test_and(self):
    eq_(eval(succeed&succeed), True)
    assert_raises(NoSolutionFound, eval, succeed&fail)
    
  def test_not_p(self):
    eq_(eval(not_p(fail)), True)
    assert_raises(NoSolutionFound, eval, not_p(succeed))
    
  def test_repeat(self):
    return
    # the code below loops for ever, after modifie the behaviour of solver.parse_state and terminals.
    eq_(eval(and_p(set_text('123'), repeat, char(x), unify(x, '3'))), True)
  def test_repeat2(self):
    return
    # the code below loops for ever.
    eq_(eval(and_p(set_text('123'), repeat, char(x), unify(x, '4'))), True) 
    
  def test_if(self):
    from dao.builtins.control import if_p
    eq_(eval(if_p(succeed, succeed)), True)
    assert_raises(NoSolutionFound, eval, if_p(succeed, fail))
    # This below unusual semantics is part of the ISO and all de-facto Prolog standards.
    # see SWI-Prolog help.
    assert_raises(NoSolutionFound, eval, if_p(fail, succeed))
    assert_raises(NoSolutionFound, eval, if_p(fail, fail))

class TestArithpred:
  def test_is(self):
    eq_(eval(is_(x, 1)), True)
  def test_eq_le_ne(self):
    from dao.builtins.arith import eq, le, ne
    eq_(eval(le(1, 1)&ne(1, 2)), True)
    eq_(eval(eq(1, 1)), True)
    eq_(eval(le(1, 1)), True)
  def test_between(self):
    from dao.builtins.arith import between
    eq_(eval(between(1, 3, 2)), True)
    eq_(eval(between(1, 3, x)), True)

class TestTypePredicate:
  def test_ground(self):
    eq_(eval(ground(1)), True)
    eq(eval(ground(Var(''))), None)
  def test_var(self):
    eq_(eval(nonvar(1)), True)
    eq_(eval(nonvar(L(1))), True)
    eq_(eval(isvar(x)), True)
    assert_raises(NoSolutionFound, eval, isvar(1))
    assert_raises(NoSolutionFound, eval, isvar(L(1)))
    assert_raises(NoSolutionFound, eval, nonvar(x))
    
class Testunify:
  def test1(self):
    eq_(eval(unify(x, 1)), True)
  def test2(self):
    eq_(eval(unify(L(1), L(1))), True)
  def test3(self):
    eq_(eval(notunify(2, L(1))), True)
    
class TestMetacall:
  def testcall(self):
    eq_(eval(call(unify(x, 1))), True)
  def testonce(self):
    eq_(eval(once(unify(x, 1))), True)
    
class Testfindall:
  def test_findall(self):
    x, y, z = Var('x'), Var('y'), Var('z')
    eq_(eval(let([(f, function(((), 2), ((), 3)))], 
               findall(is_(x, f()), x, y), y)), [2, 3])
    
class TestRule:
  def test_abolish(self):
    from dao.builtins.rule import abolish
    eq_(eval(let([(f,function([[1], 1]))], abolish(f, 1))), {})
  def test_assert(self):
    from dao.builtins.rule import assert_
    eq_(eval(let([(f,function(([1], 1)))], 
               assert_(f, [2], [2]), f(2))), 2)
  def test_asserta(self):
    from dao.builtins.rule import asserta
    eq_(eval(let([(f,function(([1], 1)))], 
               asserta(f, [2], [2]), f(2))), 2)
  def test_replace(self):
    from dao.builtins.rule import replace
    eq_(eval(let([(f,function(([1], 1), ([2], 2)))], 
               replace(f, [2], 3), f(2))), 3)
    
        
from dao.builtins.string import length, concat, contain_char, substring
class TestStringConstruct:
  def test_string_length(self):
    eq_(eval(length("abc", x)), True)
  def test_string_length2(self):
    eq_(eval(begin(length("abc", x), x)), 3)
  def test_char_in(self):
    eq_(eval(begin(contain_char('abc', x), x)), 'a')
  def test_string_concat(self):
    eq_(eval(concat("abc", "def", "abcdef")), True)
    eq_(eval(begin(concat("abc", "def", x), x)), "abcdef")
    eq_(eval(begin(concat(y, "def", "abcdef"), y)), "abc")
  def test_string_concat2(self):
    eq_(eval(begin(concat(x, y, "abcdef"), y)), "bcdef")    
  def test_sub_string(self):
    eq_(eval(begin(findall(substring('ab', 0, y, 2, "ab"), y, z), z)), 
             [2])
  def test_sub_string2(self):
    eq_(eval(begin(findall(substring('ab', x, y, z, k), k, z), z)), 
             ['a', 'ab', 'b'])
  def test_findall_string_concat(self):
    eq_(eval(begin(findall(concat(x, y, "ab"), L(x, y), z), z)), 
             [L("a", "b")])
  def test_findall_string_concat2(self):
    eq_(eval(begin(findall(concat(x, y, "abc"), L(x, y), z), z)), 
             [L("a", "bc"), L("ab", "c")])
    

from dao.builtins.term import copy_term
class TestTermConstruct:
  def test_copy_term(self):
    eq_(eval(begin(copy_term(L("abc", 1), x), x)), L("abc", 1))
    
