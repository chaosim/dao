from nose.tools import eq_, assert_raises

from oad.testutil import *
from oad.special import *

from oad.error import CatchableError, UncaughtError
from oad.term import Var, conslist as L
from oad.solve import eval
from oad.builtins.arith import eq, sub, mul, add, div
from oad.builtins.control import succeed, fail, or_, and_, not_, repeat
from oad.builtins.parser import settext
from oad.builtins.terminal import char
from oad.builtins.unify import unify, notunify
from oad.builtins.format import write
from oad.builtins.metacall import call, once
from oad.builtins.type import ground
from oad.builtins.type import isvar, nonvar
from oad.builtins.findall import findall
from oad.builtins.arithpred import is_    

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
    
  def test_not_(self):
    eq_(eval(not_(fail)), True)
    eq_(eval(not_(succeed)), None)
    
  def test_repeat(self):
    eq_(eval(L(and_, (settext, '123'), [repeat], (char, x), (unify, x, '3'))), True)
    # the code below loops for ever.
    #eq_(eval(L(and_, (settext, '123'), [repeat], (char, x), (unify, x, '4'))), True) 
    
  def test_if(self):
    from oad.builtins.control import ifp
    eq_(eval(ifp(succeed, succeed)), True)
    eq_(eval(ifp(succeed, fail)), None)
    # This below unusual semantics is part of the ISO and all de-facto Prolog standards.
    # see SWI-Prolog help.
    eq_(eval(ifp(fail, succeed)), None)
    eq_(eval(ifp(fail, fail)), None)

class xTestException:
  def test_raise(self):
    from oad.builtins.exception import raise_, try_
    assert_raises(UncaughtError, eval, L(raise_, 1))
    assert_raises(UncaughtError, eval, L(try_, (raise_, 1), 2, (write, 1)))
    eq_(eval(L(try_, (raise_, 1), 1, (write, 1))), True)
    
class TestArithpred:
  def test_eq_le_ne(self):
    from oad.builtins.arithpred import eq, le, ne
    eq_(eval(le(1, 1)&ne(1, 2)), True)
    eq_(eval(eq(1, 1)), True)
    eq_(eval(le(1, 1)), True)
  def test_between(self):
    from oad.builtins.arithpred import between
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
    
class xTestRuleManipulation:
  def test_abolish(self):
    from oad.builtins.rule import abolish
    eq_(eval(L(let, [(f, [function, ([1], 1)])], (abolish, f, 1))), True)
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
    eq_(eval(atom_length("abc", x)), True)
    eq_(eval(begin(atom_length("abc", x), x)), 3)
  def test_atom_concat(self):
    from oad.builtins.atom import atom_concat
    x, y = Var('x'), Var('y')
    eq_(eval(atom_concat("abc", "def", "abcdef")), True)
    eq_(eval(begin(atom_concat("abc", "def", x), x)), "abcdef")
    eq_(eval(begin(atom_concat(y, "def", "abcdef"), y)), "abc")
  def test_atom_concat2(self):
    from oad.builtins.atom import atom_concat
    x, y = Var('x'), Var('y')
    eq_(eval(begin(atom_concat(x, y, "abcdef"), y)), "abcdef")
    
  def test_findall_atom_concat(self):
    from oad.builtins.atom import atom_concat
    from oad.builtins.findall import findall    
    x, y, z = Var('x'), Var('y'), Var('z')
    eq_(eval(begin(findall(atom_concat(x, y, "ab"), L(x, y), z), z)), 
             (L("", "ab"), L("a", "b"), L("ab", "")))
    
class xTestTermConstruct:
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
    
class xTestFormat:
  def test_write(self):
    from oad.builtins.format import write, nl
    x = Var('x')
    eq_(eval(L(begin, (write, 1, 2), [nl])), True)
    

