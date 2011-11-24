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
from dao.builtins.io import prin #write, 
from dao.builtins.term import ground_p
from dao.builtins.term import isvar, nonvar, is_, define, nonvar_p, isvar_p

from dao.solve import set_run_mode, noninteractive
set_run_mode(noninteractive)

from dao.compiler import compile

class TestControl:
  def test_fail(self):
    eq_(eval(let([(f,function([[1], fail], [[x], succeed]))], f(x))), True)

  def test_succeed(self):
    eq_(eval(let([(f,function([[1], succeed]))], f(x))), True)

  def test_Or(self):
    eq_(compile(or_p(fail, succeed)), '''
    def or_p(call1, call2):
      for x in call1:
        yield x
      for x in call2:
        yield x
    def fail():
      if 0: yield True
    def succeed():
      yield True
    for x in or_p(fail, succeed):
      yield x
    ''')
    
  def test_Or(self):
    eq_(compile(or_p(fail, succeed)), 
        '''for x in or_p(fail, succeed): yield x''')
    
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
    
  def test_if_p(self):
    from dao.builtins.control import if_p
    eq_(eval(if_p(succeed, succeed)), True)
    assert_raises(NoSolutionFound, eval, if_p(succeed, fail))
    # This below unusual semantics is part of the ISO and all de-facto Prolog standards.
    # see SWI-Prolog help.
    assert_raises(NoSolutionFound, eval, if_p(fail, succeed))
    assert_raises(NoSolutionFound, eval, if_p(fail, fail))

class xTestArithpred:
  def test_is(self):
    eq_(eval(is_(x, 1)), True)
  def test_define_recursive(self):
    eq_(eval(begin(define(f, function(((2,), 2), ((x,), f(x-1)))), f(4))), 2)
  def test_eq_le_ne(self):
    from dao.builtins.arith import eq, le, ne
    eq_(eval(le(1, 1)&ne(1, 2)), True)
    eq_(eval(eq(1, 1)), True)
    eq_(eval(le(1, 1)), True)
  def test_between(self):
    from dao.builtins.arith import between
    eq_(eval(between(1, 3, 2)), True)
    eq_(eval(between(1, 3, x)), True)

class xTestTypePredicate:
  def test_ground(self):
    eq_(eval(ground_p(1)), True)
    assert_raises(NoSolutionFound, eval, ground_p(Var('')))
  def test_var(self):
    eq_(eval(isvar(1)), False)
    eq_(eval(isvar(L(1))), False)
    eq_(eval(nonvar(1)), True)
    eq_(eval(nonvar(L(1))), True)
    eq_(eval(isvar(x)), True)
    eq_(eval(nonvar_p(1)), True)
    eq_(eval(nonvar_p(L(1))), True)
    eq_(eval(isvar_p(x)), True)
    assert_raises(NoSolutionFound, eval, isvar_p(1))
    assert_raises(NoSolutionFound, eval, isvar_p(L(1)))
    assert_raises(NoSolutionFound, eval, nonvar_p(x))
    
class xTestunify:
  def test1(self):
    eq_(eval(unify(x, 1)), True)
  def test2(self):
    eq_(eval(unify(L(1), L(1))), True)
  def test3(self):
    eq_(eval(notunify(2, L(1))), True)
    
class xTestMetacall:
  def testcall(self):
    eq_(eval(call(unify(x, 1))), True)
    eq_(eval(is_(x, quote(prin(1)))&call(x)), None)
  def testonce(self):
    eq_(eval(findall(once(prin('1, ')|prin('2, ')))), True)
    
class xTestfindall:
  def test_findall(self):
    x, y, z = Var('x'), Var('y'), Var('z')
    eq_(eval(let([(f, function(((), 2), ((), 3)))], 
               findall(is_(x, f()), x, y), y)), [2, 3])
    
class xTestRule:
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
               replace(f, [2], [3]), f(2))), 3)
    
        
from dao.builtins.container import concat, subsequence
from dao.builtins.container import length, contain

class xTestStringConstruct:
  def test_string_length(self):
    eq_(eval(length("abc", x)), True)
  def test_string_length2(self):
    eq_(eval(begin(length("abc", x), x)), 3)
  def test_char_in(self):
    eq_(eval(begin(contain('abc', x), x)), 'a')
  def test_string_concat(self):
    eq_(eval(concat("abc", "def", "abcdef")), True)
    eq_(eval(begin(concat("abc", "def", x), x)), "abcdef")
    eq_(eval(begin(concat(y, "def", "abcdef"), y)), "abc")
  def test_string_concat2(self):
    eq_(eval(begin(concat(x, y, "abcdef"), y)), "bcdef")    
  def test_sub_string(self):
    eq_(eval(begin(findall(subsequence('ab', 0, y, 2, "ab"), y, z), z)), 
             [2])
  def test_sub_string2(self):
    eq_(eval(begin(findall(subsequence('ab', x, y, z, k), k, z), z)), 
             ['a', 'ab', 'b'])
  def test_findall_string_concat(self):
    eq_(eval(begin(findall(concat(x, y, "ab"), L(x, y), z), z)), 
             [L("a", "b")])
  def test_findall_string_concat2(self):
    eq_(eval(begin(findall(concat(x, y, "abc"), L(x, y), z), z)), 
             [L("a", "bc"), L("ab", "c")])
    

from dao.builtins.term import copy_term
class xTestTermConstruct:
  def test_copy_term(self):
    eq_(eval(begin(copy_term(L("abc", 1), x), x)), L("abc", 1))
    
from dao.builtins.quasiquote import quasiquote, unquote, unquote_splice
class xTestQuasiquote:
  def test_simple1(self):
    eq_(eval(quasiquote(1)), 1)
  def test_unquote1(self):
    eq_(eval(quasiquote(unquote(add(1,1)))), 2)
  def test_unquote_slice1(self):
    assert_raises(DaoSyntaxError, eval, quasiquote(unquote_splice(add(1,1))))
  def test_tuple1(self):
    eq_(eval(quasiquote((1,))), (1,))
    eq_(eval(quasiquote((1,2))), (1,2))
    eq_(eval(quasiquote((add(1,1),2))), ((add,1,1),2))
  def test_unquote_add(self):
    eq_(eval(quasiquote((unquote(add(1,1)),2))), (2,2))
  def test_unquote_slice(self):
    eq_(eval(quasiquote((unquote(add(1,1)),unquote_splice(quote((3,4)))))), (2,3,4))
  def test_too_many_unquote(self):
   assert_raises(DaoSyntaxError, eval, quasiquote((unquote(unquote(add(1,1))),2)))
  