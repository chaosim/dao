from nose.tools import eq_, assert_raises

from oad.error import UnifyFail, CatchableError, UncaughtError
from oad.term import Atom, atom, Var, conslist as L, Symbol
from oad.term import SUCCESS as SUCCESS, Integer , UList
from oad.trail import Trail
from oad.eval import eval
from oad.builtins.arith import eq, sub, mul, add, div
from oad.builtins.control import succeed, fail, or_, and_, not_, repeat
from oad.builtins.parser import settext
from oad.builtins.terminal import char
from oad.builtins.unify import unify
from oad.builtins.format import write

from oad.tests.util import *

class TestControl:
  def test_fail(self):
    eq_(eval(L(let, [[f, [function, [[1], [fail]], [[x], [succeed]]]]], [f, x])), SUCCESS)

  def test_success(self):
    eq_(eval(L(let, [[f, [function, [[1], [succeed]]]]], [f, x])), SUCCESS)

  def test_Or(self):
    eq_(eval(L(or_, [fail], [succeed])), SUCCESS)
    
  def test_and(self):
    eq_(eval(L(and_, [succeed], [succeed])), SUCCESS)
    assert_raises(UnifyFail, eval, L(and_, [succeed], [fail]))
    
  def test_not_(self):
    eq_(eval(L(not_, [fail])), SUCCESS)
    assert_raises(UnifyFail, eval, L(not_, [succeed]))
    
  def test_repeat(self):
    eq_(eval(L(and_, (settext, '123'), [repeat], (char, x), (unify, x, '3'))), SUCCESS)
    # the code below loops for ever.
    #eq_(eval(L(and_, (settext, '123'), [repeat], (char, x), (unify, x, '4'))), SUCCESS) 
    
  def test_if(self):
    from oad.builtins.control import ifp
    eq_(eval(L(ifp, [succeed], [succeed])), SUCCESS)
    assert_raises(UnifyFail, eval, (L(ifp, [succeed], [fail])))
    # This below unusual semantics is part of the ISO and all de-facto Prolog standards.
    # see SWI-Prolog help.
    assert_raises(UnifyFail, eval, (L(ifp, [fail], [SUCCESS])))
    assert_raises(UnifyFail, eval, (L(ifp, [fail], [fail])))

class TestException:
  def test_raise(self):
    from oad.builtins.exception import raise_, try_
    assert_raises(UncaughtError, eval, L(raise_, 1))
    assert_raises(UncaughtError, eval, L(try_, (raise_, 1), 2, (write, 1)))
    eq_(eval(L(try_, (raise_, 1), 1, (write, 1))), SUCCESS)
    
class TestArithpred:
  def test_eq_le_ne(self):
    from oad.builtins.arithpred import eq, le, ne
    eq_(eval(L(and_, (le, 1, 1), (ne, 1, 2))), SUCCESS)
    eq_(eval(L(eq, 1, 1)), SUCCESS)
    eq_(eval(L(le, 1, 1)), SUCCESS)
  def test_between(self):
    from oad.builtins.arithpred import between
    eq_(eval(L(between, 1, 3, 2)), SUCCESS)
    eq_(eval(L(between, 1, 3, x)), SUCCESS)

class TestTypePredicate:
  def test_ground(self):
    from oad.builtins.type import ground
    eq_(eval(L(ground, 1)), SUCCESS)
    assert_raises(UnifyFail, eval, L(ground, Var('')))
  def test_var(self):
    from oad.builtins.type import isvar, nonvar
    eq_(eval(L(nonvar, 1)), SUCCESS)
    eq_(eval(L(nonvar, UList(1))), SUCCESS)
    eq_(eval(L(isvar, x)), SUCCESS)
    assert_raises(UnifyFail, eval, L(isvar, 1))
    assert_raises(UnifyFail,eval, L(isvar, UList(1)))
    assert_raises(UnifyFail, eval, L(nonvar,x))
    
class Testunify:
  def test_unify(self):
    from oad.builtins.unify import unify, notunify
    eq_(eval(L(unify, x, 1)), SUCCESS)
    eq_(eval(L(unify, UList(1), UList(1))), SUCCESS)
    eq_(eval(L(notunify, 2, UList(1))), SUCCESS)
    
class TestMetacall:
  def testcall(self):
    from oad.builtins.unify import unify, notunify
    from oad.builtins.metacall import call
    eq_(eval(L(call, (unify, x, 1))), SUCCESS)
  def testonce(self):
    from oad.builtins.unify import unify, notunify
    from oad.builtins.metacall import once
    eq_(eval(L(once, (unify, x, 1))), SUCCESS)
    
class Testfindall:
  def test_findall(self):
    from oad.builtins.findall import findall
    from oad.builtins.arithpred import is_    
    x, y, z = Var('x'), Var('y'), Var('z')
    eq_(eval(L(let, [(f, (function, ((), 2), ((), 3)))], 
               (findall, (is_, x, [f]), x, y), y)), L(2, 3))
    
class TestRuleManipulation:
  def test_abolish(self):
    from oad.builtins.rule import abolish
    eq_(eval(L(let, [(f, [function, ([1], 1)])], (abolish, f, 1))), SUCCESS)
  def test_assert(self):
    from oad.builtins.rule import assert_
    eq_(eval(L(let, [(f, [function, ([1], 1)])], 
               (assert_, f, (quote, ([2], 2))), [f, 2])), Integer(2))
  def test_asserta(self):
    from oad.builtins.rule import asserta
    eq_(eval(L(let, [(f, [function, ([1], 1)])], 
               (asserta, f, (quote, ([2], 2))), [f, 2])), Integer(2))
  def test_retract(self):
    from oad.builtins.rule import retract
    assert_raises(UnifyFail, eval, L(let, [(f, [function, ([1], 1), ([2], 2)])], 
               (retract, f, (quote, ([2], 2))), [f, 2]))
    
class xxxTestformat:
  def test_format(self):
    engine = Engine()
    X = Var()
    f = userTerm("f")
    g = userTerm('g')
    engine.run(write(X), '')
  def test_findall(self):
    engine = Engine()
    f = userTerm("f")
    X = Var()
    Y = Var()
    engine.add_rule(f(1))
    engine.add_rule(f(2))
    engine.run(findall(f(X),X, Y), '')
    eq_(Y.deref(engine.trail), term0(1, 2))    
  def test_exception(self):
    engine = Engine()
    engine.run(catch(throw(1), 1, true), '')
    engine = Engine()
    assert_raises(error.UncaughtError, engine.run, catch(throw(1), 1, throw(1)), '')
  def test_catch(self):
    engine = Engine()
    X = Var()
    t = term0("abc", "def", "abcdef")
    t.name = 'f'
    engine.run(catch(throw(t), X, unify(X, t)), '')
        
class TestAtomConstruct:
  def test_atom_length(self):
    x = Var('x')
    from oad.builtins.atom import atom_length
    eq_(eval(L(atom_length, "abc", x)), SUCCESS)
    eq_(eval(L(begin, (atom_length, "abc", x), x)), Integer(3))
  def test_atom_concat(self):
    from oad.builtins.atom import atom_concat
    x, y = Var('x'), Var('y')
    eq_(eval(L(atom_concat,"abc", "def", "abcdef")), SUCCESS)
    eq_(eval(L(begin, (atom_concat, "abc", "def", x), x)), atom("abcdef"))
    eq_(eval(L(begin, (atom_concat, y, "def", "abcdef"), y)), atom("abc"))
  def test_atom_concat2(self):
    from oad.builtins.atom import atom_concat
    x, y = Var('x'), Var('y')
    eq_(eval(L(begin, (atom_concat,x, y, "abcdef"), y)), atom("abcdef"))
    
  def test_findall_atom_concat(self):
    from oad.builtins.atom import atom_concat
    from oad.builtins.findall import findall    
    x, y, z = Var('x'), Var('y'), Var('z')
    eq_(eval(L(begin, (findall, (atom_concat, x, y, "ab"), (x, y), z), z)), 
             L(("", "ab"), ("a", "b"), ("ab", "")))
    
class TestTermConstruct:
  def test_copy_term(self):
    from oad.builtins.term import copy_term
    x = Var('x')
    eq_(eval(L(begin, (copy_term, UList("abc", 1), x), x)), UList("abc", 1))
  def xtest_univ(self):
    engine = Engine()
    X = Var()
    Y = Var()
    Z = Var()
    t = term0("abc", "def", "abcdef")
    ul = helper.wrap_list([atom(t.name)]+list(t.elements))
    engine.run(univ(t, X), '')
    eq_(X.deref(engine.trail), ul)  
    engine.run(univ(Y, ul), '')
    eq_(Y.deref(engine.trail), t)  
  def xtest_functor(self):
    engine = Engine()
    X = Var()
    Y = Var()
    Z = Var()
    t = Term('', ["abc", "def", "abcdef"])
    t.name = 'f'
    engine.run(functor(t, X, Y), '')
    eq_(X.deref(engine.trail), atom('f'))  
    eq_(Y.deref(engine.trail), Integer(3))  
    
class TestFormat:
  def test_write(self):
    from oad.builtins.format import write, nl
    x = Var('x')
    eq_(eval(L(begin, (write, 1, 2), [nl])), SUCCESS)
    

