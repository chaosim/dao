# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad.term import Var
from oad.pycfg import *
from oad.syntax import have

e0, e1, e2 = getattr(have.a==1), ActElement(getattr, ()), ActElement(getattr, ())

class TestElement:
  def test_some(self):
    fsm = FSM()
    state0 = fsm.start_state
    state1 = Some(e0).toFSM(fsm, state0)
  def test_any(self):
    fsm = FSM()
    state0 = fsm.start_state
    state1 = Any(e0).toFSM(fsm, state0)
  def test_sequence(self):
    fsm = FSM()
    state0 = fsm.start_state
    state1 = Some(e0+e1+e2).toFSM(fsm, state0)
  def test_or(self):
    fsm = FSM()
    state0 = fsm.start_state
    state1 = Some(e0|e1|e2).toFSM(fsm, state0)
  def test_classname(self):
    fsm = FSM()
    state0 = fsm.start_state
    state1 = Some(e0|e1|e2).Some_e0_e1_e2.toFSM(fsm, state0)
    eq_(state1.class_name, 'Some_e0_e1_e2')

class Test_null_closure:
  def test1(self):
    fsm = FSM()
    s0 = fsm.start_state
    s1 = nullword.toFSM(fsm, s0)
    s2 = nullword.toFSM(fsm, s0)
    s3 = nullword.toFSM(fsm, s0)
    s4 = nullword.toFSM(fsm, s1)
    s5 = nullword.toFSM(fsm, s4)
    eq_(fsm.null_closure(set([s0])), set([s0, s1, s2, s3, s4, s5]))
    eq_(fsm.null_closure(set([s0, s5])), set([s0, s1, s2, s3, s4, s5]))
    eq_(fsm.null_closure(set([s0, s3, s5])), set([s0, s1, s2, s3, s4, s5]))

class Test_make_class:
  def test_getattr(self):
    fsm = e0.makeFSM()
    fsm.make_class(set([fsm.start_state]))
  def test_getattr2(self):
    x = lead(e0)
    x.a
  def test_getattr3(self):
    x = lead(e0+e1)
    x.a.b

class Test_cfg_make_class:
  def test_getattr(self):
    fsm = FSM()
    state0 = fsm.start_state
    getattr.toFSM(fsm, state0)
    do = fsm.make_class()()
    do.write
    
  def test_some_getattr(self):
    fsm = FSM()
    state0 = fsm.start_state
    some(getattr).toFSM(fsm, state0)
    do = fsm.make_class()()
    do.write
    assert_raises(TypeError, do.write.write)

  def test_some_getattr_getitem(self):
    fsm = FSM()
    state0 = fsm.start_state
    (some(getattr)+getitem).toFSM(fsm, state0)
    do = fsm.make_class()()
    do.write.write[1]

  def test_any_div(self):
    fsm = FSM()
    state0 = fsm.start_state
    any(div).toFSM(fsm, state0)
    do = fsm.make_class()()
    do/'a'/1/fsm
    
  def test_init(self):
    x = init(lambda x:[]).makeFSM().make_class()()
    eq_(x.__data__, [])

class TestSyntax:
  def test_v(self):
    x = v.a
    eq_(x.__class__, Var)

  def test_var(self):
    x = var.a.b.c
    eq_(list(x), [varcache('a'),varcache('b'),varcache('c')])
