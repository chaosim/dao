# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises
from oad.builtin import *
from oad.error import UnifyFail, CatchableError
from oad.term import atom, Var, term, Term, userTerm, nonterminal, function
from oad.rule import rule, Rule 
from oad.engine import Trail, Engine

class TestUnify:
  def test_atom(self):
    a = atom("hallo")
    b = atom("hallo")
    a.unify(b, None) # does not raise
    assert_raises(UnifyFail, a.unify, atom('xxx'), None, False)

  def test_var(self):
    b = Var()
    trail = Trail()
    b.unify(atom("hallo"), trail)
    eq_(b.getvalue(trail), atom("hallo"))
    a = Var()
    b = Var()
    a.unify(b, trail)
    a.unify(atom("hallo"), trail)
    eq_(a.getvalue(trail),atom("hallo"))
    eq_(b.getvalue(trail),atom("hallo"))

  def test_unify_var(self):
    b = Var()
    trail = Trail()
    b.unify(b, trail)
    b.unify(atom("hallo"), trail)
    assert_raises(UnifyFail, b.unify, atom("bye"), trail)

  def test_recursive(self):
    b = Var()
    trail = Trail()
    b.unify(Term("hallo", [b]), trail)

  def test_term(self):
    X = Var()
    Y = Var()
    f = term("f")
    t1 = f("hallo", X)
    t2 = f(Y, "HALLO")
    trail = Trail()
    t1.unify(t2, trail)
    eq_(X.getvalue(trail), atom("HALLO"))
    eq_(Y.getvalue(trail), atom("hallo"))

from oad.term import DynamicVar
class TestDynamicVar:
  def test1(self):
    DX = DynamicVar()
    P = nonterminal('P')
    rules = [Rule(P(), is_(DX, 0)+fail),
             Rule(P(), eq(DX, 0))]
    engine = Engine()
    engine.addRules(rules)
    engine.run(P(), '')
  def test2(self):
    X, X1 = Var(), Var()
    DX = DynamicVar()
    P = nonterminal('P')
    rules = [Rule(P(),is_(DX, 0)+eq(DX, 0)+assign(DX, 1)+eq(DX, 1))]
    engine = Engine()
    engine.addRules(rules)
    engine.run(P(), '')

class TestCons:
  def testUnify(self):
    from oad.termlib import conslist as termList
    trail = Trail()
    l1= termList(12, 23)
    l2 = termList(12, 23)
    l1.unify(l2, trail)
    X, Y = Var(), Var()
    l1= termList(X, 23)
    l2 = termList(12, Y)
    l1.unify(l2, trail)

from oad.termlib import QuasiquoteListTerm as ql, at
class TestListTerm:
  def testUnify(self): 
    trail = Trail()
    l1= ql(12, 23)
    l2 = ql(12, 23)
    l1.unify(l2, trail)
    X, Y = Var(), Var()
    l1= ql(X, 23)
    l2 = ql(12, Y)
    l1.unify(l2, trail)
  def testeq(self):
    eq_(ql(1, '/', at(ql(1))), ql(1, '/', at(ql(1))))
  def testgetvalue(self):
    trail = Trail()
    listTerm1 = ql(1, '/', at(ql(1)))
    eq_(listTerm1.getvalue(trail) , ql(1, '/', 1))

