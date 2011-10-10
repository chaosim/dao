from nose.tools import eq_, assert_raises

from oad.util import *
from oad.special import *

from oad.term import Var, conslist as L
from oad.solve import eval
from oad.builtins.arith import eq, sub, mul, add, div
from oad.builtins.control import succeed, fail, or_, and_, not_p, repeat
from oad.builtins.control import findall, call, once
from oad.builtins.parser import settext
from oad.builtins.terminal import char
from oad.builtins.term import unify, notunify
from oad.builtins.format import write
from oad.builtins.term import ground
from oad.builtins.term import isvar, nonvar, is_

class TestControl:
  def test_fail(self):
    eq_(eval(let({f: function([[1], fail], [[x], succeed])}, f(x))), True)

  def test_succeed(self):
    eq_(eval(let({f: function([[1], succeed])}, f(x))), True)

  def test_Or(self):
    eq_(eval(or_(fail, succeed)), True)
    
  def test_and(self):
    eq_(eval(succeed&succeed), True)
    eq_(eval(succeed&fail), None)
    
  def test_not_p(self):
    eq_(eval(not_p(fail)), True)
    eq_(eval(not_p(succeed)), None)
    
  def test_repeat(self):
    return
    # the code below loops for ever, after modifie the behaviour of solver.stream and terminals.
    eq_(eval(and_(settext('123'), repeat, char(x), unify(x, '3'))), True)
  def test_repeat2(self):
    return
    # the code below loops for ever.
    eq_(eval(and_(settext('123'), repeat, char(x), unify(x, '4'))), True) 
    
  def test_if(self):
    from oad.builtins.control import if_p
    eq_(eval(if_p(succeed, succeed)), True)
    eq_(eval(if_p(succeed, fail)), None)
    # This below unusual semantics is part of the ISO and all de-facto Prolog standards.
    # see SWI-Prolog help.
    eq_(eval(if_p(fail, succeed)), None)
    eq_(eval(if_p(fail, fail)), None)

class TestArithpred:
  def test_is(self):
    eq_(eval(is_(x, 1)), True)
  def test_eq_le_ne(self):
    from oad.builtins.arith import eq, le, ne
    eq_(eval(le(1, 1)&ne(1, 2)), True)
    eq_(eval(eq(1, 1)), True)
    eq_(eval(le(1, 1)), True)
  def test_between(self):
    from oad.builtins.arith import between
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
    eq_(eval(isvar(1)), None)
    eq_(eval(isvar(L(1))), None)
    eq_(eval(nonvar(x)), None)
    
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
    eq_(eval(let({f: function(((), 2), ((), 3))}, 
               findall(is_(x, f()), x, y), y)), [2, 3])
    
class TestRule:
  def test_abolish(self):
    from oad.builtins.rule import abolish
    eq_(eval(let({f:function([[1], 1])}, abolish(f, 1))), {})
  def test_assert(self):
    from oad.builtins.rule import assert_
    eq_(eval(let({f:function(([1], 1))}, 
               assert_(f, [2], [2]), f(2))), 2)
  def test_asserta(self):
    from oad.builtins.rule import asserta
    eq_(eval(let({f:function(([1], 1))}, 
               asserta(f, [2], [2]), f(2))), 2)
  def test_replace(self):
    from oad.builtins.rule import replace
    eq_(eval(let({f:function(([1], 1), ([2], 2))}, 
               replace(f, [2], [3]), f(2))), 3)
    
        
from oad.builtins.string import length, concat, char_in, substring
class TestStringConstruct:
  def test_string_length(self):
    eq_(eval(length("abc", x)), True)
  def test_string_length2(self):
    eq_(eval(begin(length("abc", x), x)), 3)
  def test_char_in(self):
    eq_(eval(begin(char_in(x, "abc"), x)), 'a')
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
    

from oad.builtins.term import copy_term
class TestTermConstruct:
  def test_copy_term(self):
    eq_(eval(begin(copy_term(L("abc", 1), x), x)), L("abc", 1))
    
