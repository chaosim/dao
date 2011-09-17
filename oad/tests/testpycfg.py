# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad.term import Var
from oad.pycfg import *

class TestFSM:
  def test1(self):
    fsm = FSM()
    state0 = fsm.start_state
    state1 = fsm.add_edge(state0, 0)
    state2 = fsm.add_edge(state0, 1)
    assert_raises(AmibiguityPathError, fsm.add_edge, state0, 1, state1)

e0, e1, e2 = Element(), Element(), Element()

class TestElement:
  def test_some(self):
    fsm = FSM()
    state0 = fsm.start_state
    state1 = Some(e0).toFSM(fsm, state0)
  def test_any(self):
    fsm = FSM()
    state0 = fsm.start_state
    state1 = fsm.add_edge(state0, 0)
    state0 = Any(e0).toFSM(fsm, state0, state0)
    assert_raises(AmibiguityPathError, Any(e0).toFSM, fsm, state0, state1)
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
    state1 = Some(e0|e1|e2)['Some_e0_e1_e2'].toFSM(fsm, state0)
    eq_(state1.class_name, 'Some_e0_e1_e2')
    
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
