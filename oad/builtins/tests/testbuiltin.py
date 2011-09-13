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
  def setUp(self): cleanup_vars()
  
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

class TestArithpred:
  def setUp(self): cleanup_vars()
  
  def test_is(self):
    eq_(eval(is_(x, 1)), True)
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
  def setUp(self): cleanup_vars()
  
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
  def setUp(self): cleanup_vars()
  
  def test1(self):
    eq_(eval(unify(x, 1)), True)
  def test2(self):
    eq_(eval(unify(L(1), L(1))), True)
  def test3(self):
    eq_(eval(notunify(2, L(1))), True)
    
class TestMetacall:
  def setUp(self): cleanup_vars()
  
  def testcall(self):
    eq_(eval(call(unify(x, 1))), True)
  def testonce(self):
    eq_(eval(once(unify(x, 1))), True)
    
class Testfindall:
  def setUp(self): cleanup_vars()
  
  def test_findall(self):
    x, y, z = Var('x'), Var('y'), Var('z')
    eq_(eval(let({f: function(((), 2), ((), 3))}, 
               findall(is_(x, f()), x, y), y)), [2, 3])
    
class TestRuleManipulation:
  def setUp(self): cleanup_vars()
  
  def test_abolish(self):
    from oad.builtins.rule import abolish
    eq_(eval(let({f:function([[1], 1])}, abolish(f, 1))), True)
  def test_assert(self):
    from oad.builtins.rule import assert_
    eq_(eval(let({f:function(([1], 1))}, 
               assert_(f, [2], 2), f(2))), 2)
  def test_asserta(self):
    from oad.builtins.rule import asserta
    eq_(eval(let({f:function(([1], 1))}, 
               asserta(f, [2], 2), f(2))), 2)
  def test_retract(self):
    from oad.builtins.rule import retract
    eq_(eval(let({f:function(([1], 1), ([2], 2))}, 
               retract(f, [2], 3), f(2))), 3)
    
        
from oad.builtins.atom import atom_length, atom_concat, charin, sub_atom
class TestAtomConstruct:
  def setUp(self): cleanup_vars()
  def test_atom_length(self):
    eq_(eval(atom_length("abc", x)), True)
  def test_atom_length2(self):
    eq_(eval(begin(atom_length("abc", x), x)), 3)
  def test_charin(self):
    eq_(eval(begin(charin(x, "abc"), x)), 'a')
  def test_atom_concat(self):
    eq_(eval(atom_concat("abc", "def", "abcdef")), True)
    eq_(eval(begin(atom_concat("abc", "def", x), x)), "abcdef")
    eq_(eval(begin(atom_concat(y, "def", "abcdef"), y)), "abc")
  def test_atom_concat2(self):
    eq_(eval(begin(atom_concat(x, y, "abcdef"), y)), "bcdef")    
  def test_sub_atom(self):
    eq_(eval(begin(findall(sub_atom('ab', 0, y, 2, "ab"), y, z), z)), 
             [2])
  def test_sub_atom2(self):
    eq_(eval(begin(findall(sub_atom('ab', x, y, z, k), k, z), z)), 
             ['a', 'ab', 'b'])
  def test_findall_atom_concat(self):
    eq_(eval(begin(findall(atom_concat(x, y, "ab"), L(x, y), z), z)), 
             [L("a", "b")])
  def test_findall_atom_concat2(self):
    eq_(eval(begin(findall(atom_concat(x, y, "abc"), L(x, y), z), z)), 
             [L("a", "bc"), L("ab", "c")])
    

from oad.builtins.term import copy_term
class TestTermConstruct:
  def setUp(self): cleanup_vars()
  
  def test_copy_term(self):
    eq_(eval(begin(copy_term(L("abc", 1), x), x)), L("abc", 1))
    
