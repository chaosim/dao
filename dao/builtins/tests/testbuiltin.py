from nose.tools import eq_, assert_raises

#from dao.term import Var, conslist as L
from dao.solve import eval
from dao.solvebase import NoSolution

from dao.builtins.special import *
from dao.builtins.arith import eq, sub, mul, add, div
from dao.builtins.control import succeed, fail, or_, and_, not_p, cut_or#, repeat
from dao.builtins.control import findall#, call, once
from dao.builtins.parser import set_text
from dao.builtins.terminal import char
from dao.builtins.term import unify#, notunify
from dao.builtins.io import prin #write, 
#from dao.builtins.term import ground_p
#from dao.builtins.term import isvar, nonvar, is_, define, nonvar_p, isvar_p

from dao.tests.util import *

class TestControl:
  def xtest_fail(self):
    eq_(eval(let([(f,function([[1], fail], [[x], succeed]))], f(x))), True)

  def xtest_succeed(self):
    eq_(eval(let([(f,function([[1], succeed]))], f(x))), True)

  def test_or(self):
    eq_(eval(or_(fail, succeed)), True)
    
  def test_cut_or(self):
    eq_(eval(or_(begin(prin(1), fail), prin(2))), None)
    #assert_raises(NoSolution, eval, or_(begin(prin(1), cut_or, fail), prin(2)))
    
  def test_and(self):
    eq_(eval(and_(succeed, succeed)), True)
    
  def test_and2(self):
    assert_raises(NoSolution, eval, and_(succeed, fail))
    
  def test_not_p(self):
    eq_(eval(not_p(fail)), True)
    assert_raises(NoSolution, eval, not_p(succeed))
    
  def xtest_repeat(self):
    return
    # the code below loops for ever, after modifie the behaviour of solver.parse_state and terminals.
    eq_(eval(and_p(set_text('123'), repeat, char(x), unify(x, '3'))), True)
    
  def xtest_repeat2(self):
    return
    # the code below loops for ever.
    eq_(eval(and_p(set_text('123'), repeat, char(x), unify(x, '4'))), True) 
    
  def xtest_if_p(self):
    from dao.builtins.control import if_p
    eq_(eval(if_p(succeed, succeed)), True)
    assert_raises(NoSolution, eval, if_p(succeed, fail))
    # This below unusual semantics is part of the ISO and all de-facto Prolog standards.
    # see SWI-Prolog help.
    assert_raises(NoSolution, eval, if_p(fail, succeed))
    assert_raises(NoSolution, eval, if_p(fail, fail))

class XTestArithpred:
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

class XTestTypePredicate:
  def test_ground(self):
    eq_(eval(ground_p(1)), True)
    assert_raises(NoSolution, eval, ground_p(Var('')))
  def test_var(self):
    eq_(eval(isvar(1)), False)
    eq_(eval(isvar(L(1))), False)
    eq_(eval(nonvar(1)), True)
    eq_(eval(nonvar(L(1))), True)
    eq_(eval(isvar(x)), True)
    eq_(eval(nonvar_p(1)), True)
    eq_(eval(nonvar_p(L(1))), True)
    eq_(eval(isvar_p(x)), True)
    assert_raises(NoSolution, eval, isvar_p(1))
    assert_raises(NoSolution, eval, isvar_p(L(1)))
    assert_raises(NoSolution, eval, nonvar_p(x))
    
class Testunify:
  def test1(self):
    eq_(eval(unify(x, 1)), True)
    
  def test2(self):
    eq_(eval(unify(L(1), L(1))), True)
    
  def test3(self):
    eq_(eval(notunify(2, L(1))), True)
    
  def test_unify(self):
    x = Var('x')
    Lx = LogicVar('x')
    assert_raises(NoSolution, eval, begin(unify(Lx, 1), unify(Lx,2)))
    eq_(eval(let([(x,1)], unify(x,1))), True)
    eq_(eval(unify(Lx,1)), True)
    eq_(eval(begin(unify(Lx, 1), unify(Lx,1))), True)
    assert_raises(NoSolution, eval, begin(unify(1, 1), unify(1, 2)))
    assert_raises(NoSolution, eval, begin(unify(2, 1), unify(1, 1)))
    assert_raises(NoSolution, eval, unify(1, 2))
    eq_(eval(unify(1, 1)), True)
    eq_(eval(begin(unify(1, 1), unify(2, 2))), True)
    
class XTestMetacall:
  def testcall(self):
    eq_(eval(call(unify(x, 1))), True)
    eq_(eval(is_(x, quote(prin(1)))&call(x)), None)
  def testonce(self):
    eq_(eval(findall(once(prin('1, ')|prin('2, ')))), True)
    
class Testfindall:
  def test_findall_1(self):
    eq_(eval(findall(or_(prin(1), prin(2)))), None)
  
  def xtest_findall2(self):
    x, y, z = Var('x'), Var('y'), Var('z')
    eq_(eval(let([(f, function(((), 2), ((), 3)))], 
               findall(is_(x, f()), x, y), y)), [2, 3])
      
class XTestRule:
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
    
        
#from dao.builtins.container import concat, subsequence
#from dao.builtins.container import length, contain

class XTestStringConstruct:
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

#from dao.builtins.term import copy_term
class XTestTermConstruct:
  def test_copy_term(self):
    eq_(eval(begin(copy_term(L("abc", 1), x), x)), L("abc", 1))

from dao.builtins.quasiquote import DaoSyntaxError
from dao.builtins import quasiquote as qq, unquote as uq, unquote_splice as uqs

class TestQuasiquote:
  def test_simple1(self):
    eq_(eval(qq(1)), 1)
    
  def test_unquote1(self):
    eq_(eval(qq(uq(add(1,1)))), 2)
    
  def test_tuple1(self):
    eq_(eval(qq((1,))), (1,))
    eq_(eval(qq((1,2))), (1,2))
    eq_(eval(qq((add(1,1),2))), (add(1,1),2))
    
  def test_unquote_add(self):
    eq_(eval(qq(uq(add(1,1)))), 2)
    
  def test_unquote_slice(self):
    eq_(eval(qq(add(uqs(quote((3,4)))))), add(3, 4))
    
  def test_too_many_unquote(self):
    assert_raises(DaoSyntaxError, eval, qq(uq(uq(add(1,1)))))
   
  