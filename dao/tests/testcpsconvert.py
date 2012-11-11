# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.compile import Compiler, AlphaConvertEnvironment, VariableNotBound
from dao.compile import trampoline
from dao.command import begin, quote, assign, if_, LogicVar, let, letrec
from dao.command import add
from dao.command import fail, succeed, or_, unify, repeat, _any
from dao.command import lamda

from dao import interlang as il

v, fc = il.Var('v'), il.Var('fc')
a0, a1, a2, a3, a4 = tuple(il.Var('a'+repr(i)) for i in range(5))

class Done(il.Clamda):
  def __repr__(self): return 'done()'
  
def done():
  return Done(v, fc, v)
  
def cps_convert(exp):
  return Compiler().cps(exp, done())

class TestCPSConvert:
  def test_integer(self):
    result = cps_convert(1)
    expect = done()(1)
    eq_(result, expect)
    
  def test_quote(self):
    result = cps_convert(quote(1))
    expect = done()(1)
    eq_(result, expect)
    
  def test_begin(self):
    result = cps_convert(begin(1, 2))
    expect = il.Clamda(v, done()(2))(1)
    eq_(result, expect)
  
  def test_assign(self):
    x = il.Var('x')
    result = cps_convert(assign(x, 2))
    expect = il.Clamda(v, il.Assign(x, v), v)(2)
    eq_(result, expect)

  def test_if(self):
    result = cps_convert(if_(0, 1, 2))
    expect = il.Clamda(v, il.If(v, done()(1), done()(2)))(0)
    eq_(result, expect)
  
  def test_fail(self):
    result = cps_convert(fail)
    expect = il.failcont(True)
    eq_(result, expect)

  def test_succeed(self):
    result = cps_convert(succeed)
    expect = done()(True)
    eq_(result, expect)
    
  def test_repeat(self):
    function = il.Var('function')
    result = cps_convert(repeat)
    expect = il.begin(
       il.SetFailCont(function), 
       il.CFunction(function, v, done()(v)))
    eq_(result, expect)

  def test_or(self):
    cut_or_cont = il.Var('cut_or_cont')
    result = cps_convert(or_(1, 2))
    expect = il.begin(
      il.Assign(cut_or_cont, il.cut_or_cont), 
      il.SetCutOrCont(il.failcont), 
      il.AppendFailCont(
        il.Clamda(v, 
            il.SetCutOrCont(cut_or_cont), 
            done()(v))
        (2)), 
      il.Clamda(v, 
                il.SetCutOrCont(cut_or_cont), 
                done()(v))
      (1))
    eq_(result, expect)
    
  def test_unify(self):
    eq_(cps_convert(unify(1, 2)), il.failcont(True))    
    eq_(cps_convert(unify(1, 1)), done()(True))
    
  def test_unify2(self):
    x = LogicVar('x')
    result = cps_convert(unify(x, 2))
    expect = il.begin(il.AppendFailCont(il.DelBinding(x)), 
                      done()(True))
    eq_(result, expect)
    
  def test_unify3(self):
    x = il.Var('x')
    result = cps_convert(unify(x, 2))
    expect = il.begin(
      il.Assign(x, il.Deref(x)), 
      il.If(il.Isinstance(x, il.LogicVar), 
            il.begin(il.SetBinding(x, 2), 
                     il.AppendFailCont(il.DelBinding(x)), 
                     done()(True)), 
            il.begin(il.Assign(2, il.Deref(2)), 
                     il.If(il.Isinstance(2, il.LogicVar), 
                           il.begin(il.SetBinding(2, x), 
                                    il.AppendFailCont(il.DelBinding(2)), 
                                    done()(True)), 
                           il.If(il.Eq(x, 2), done()(True), il.failcont(True))))))
    eq_(result, expect)
    
  def test_add(self):
    result = cps_convert(add(1, 2))
    expect = il.Clamda(a0, il.Clamda(a1, done()(il.add((a0, a1))))(2))(1)
    eq_(result, expect)

  def test_lambda(self):
    x, y, k = il.Var('x'), il.Var('y'), il.Var('k')
    result = cps_convert(lamda((x,y), 1))
    expect = done()(lamda((x, y, k), k(1)))
    eq_(result, expect)
    
  def test_let(self):
    x, y, k = il.Var('x'), il.Var('y'), il.Var('k')
    result = cps_convert(let(((x,1),), x))
    expect = il.Clamda(x, done()(x))(1)
    eq_(result, expect)
    
  def test_letrec(self):
    f, k, function = il.Var('f'), il.Var('k'), il.Var('function')
    result = cps_convert(letrec([(f, lamda((), f()))], f()))
    expect = il.Clamda(v, 
                       il.Assign(f, v), 
                       v)(
                         il.Lamda((k,), il.Clamda(function, function(k))(f)))
    eq_(result, expect)
    
from dao.command import eoi, char, findall

class TestBuiltin:
  def test_eoi(self):
    x = il.Var('x')
    result = cps_convert(eoi)
    expect = il.If(il.Eq(il.parse_state[1], il.Len(il.parse_state[0])),
          done()(v),
          il.failcont(v))
    eq_(result, expect)

  def test_char(self):
    text, pos = il.Var('text'), il.Var('pos')
    result = cps_convert(char('a'))
    expect = il.Clamda(v, 
                       il.AssignFromList(text, pos, il.parse_state), 
                       il.If2((pos>=il.Len(text)), il.failcont(v)), 
                       il.If(il.Eq('a', text[pos]), 
                             il.begin(il.AppendFailCont(il.SetParseState((text, pos))), 
                                      il.SetParseState((text, il.add((pos, 1)))), 
                                      done()(text[pos])), 
                             il.failcont(v)))
    eq_(result, expect)

  def test_char2(self):
    x = il.Var('x')
    text, pos = il.Var('text'), il.Var('pos')
    result = cps_convert(char(x))
    expect = il.Clamda(v, 
                       il.AssignFromList(text, pos, il.parse_state), 
                       il.If2((pos>=il.Len(text)), 
                              il.failcont(v)), 
                       il.Assign(x, il.Deref(x)), 
                       il.If(il.Isinstance(x, 'str'), 
                             il.If(il.Eq(x, text[pos]), 
                                   il.begin(il.AppendFailCont(il.SetParseState((text, pos))),
                                            il.SetParseState((text, il.add((pos, 1)))), 
                                            done()(text[pos])), 
                                   il.failcont(v)), 
                             il.If(il.Isinstance(x, 'LogicVar'), 
                                   il.begin(il.SetParseState((text, il.add((pos, 1)))), 
                                            il.SetBinding(x, text[pos]), 
                                            il.AppendFailCont(il.SetParseState((text, pos)), 
                                                              il.DelBinding(x)), 
                                            done()(text[pos])), 
                                   il.RaiseTypeError(x))))
    eq_(result, expect)

  def test_findall(self):
    cut_or_cont = il.Var('cut_or_cont')
    result = cps_convert(findall(or_(1, 2)))
    expect = il.begin(il.AppendFailCont(done()(v)), 
                      il.begin(il.Assign(cut_or_cont, il.failcont), 
                               il.SetCutOrCont(il.failcont), 
                               il.AppendFailCont(il.Clamda(v, 
                                                           il.SetCutOrCont(cut_or_cont), 
                                                           il.Clamda(v, il.failcont(v))(v))(2)), 
                               il.Clamda(v, il.SetCutOrCont(cut_or_cont), 
                                         il.Clamda(v, il.failcont(v))(v))(1)))
    eq_(result, expect)
    
  def test_findall2(self):
    cut_or_cont = il.Var('cut_or_cont')
    x, y = il.Var('x'), il.Var('y')
    findall_result = il.Var('findall_result')
    result = cps_convert(findall(or_(1, 2), x, y))
    expect = il.begin(
      il.Assign(findall_result, il.empty_list()), 
      il.AppendFailCont(
        il.begin(
          il.Assign(y, il.Deref(y)), 
          il.If(il.Isinstance(y, il.LogicVar), 
                il.begin(il.SetBinding(y, findall_result), 
                         il.AppendFailCont(il.DelBinding(y)), done()(True)), 
                il.begin(
                  il.Assign(findall_result, il.Deref(findall_result)), 
                  il.If(il.Isinstance(findall_result, il.LogicVar), 
                        il.begin(il.SetBinding(findall_result, y), 
                                 il.AppendFailCont(il.DelBinding(findall_result)), 
                                 done()(True)), 
                        il.If(il.Eq(y, findall_result), 
                              done()(True), 
                              il.failcont(True))))))), 
      il.begin(
        il.Assign(cut_or_cont, il.failcont), 
        il.SetCutOrCont(il.failcont), 
        il.AppendFailCont(
          il.Clamda(v,
                    il.SetCutOrCont(cut_or_cont), 
                    il.Clamda(v, 
                              il.ListAppend(findall_result, il.GetValue(x)), 
                              il.failcont(v))(v))(2)), 
        il.Clamda(v, il.SetCutOrCont(cut_or_cont), 
                  il.Clamda(v, 
                            il.ListAppend(findall_result, il.GetValue(x)), 
                            il.failcont(v))(v))(1)))
    eq_(result, expect)
    
  def test_any(self):
    any_cont = il.Var('any_cont')
    result = cps_convert(_any(1))
    expect = il.CFunction(any_cont, v, il.AppendFailCont(done()(v)), any_cont(1))(None)
    eq_(result, expect)
      
  def test_any2(self):
    text, pos = il.Var('text'), il.Var('pos')
    any_cont = il.Var('any_cont')
    result = cps_convert(_any(char('1')))
    expect = il.CFunction(any_cont, v, 
              il.AppendFailCont(done()(v)), 
              il.Clamda(v, 
                        il.AssignFromList(text, pos, il.parse_state), 
                        il.If2((pos>=il.Len(text)), 
                               il.failcont(v)), 
                        il.If(il.Eq('1', text[pos]), 
                              il.begin(il.AppendFailCont(il.SetParseState((text, pos))), 
                                       il.SetParseState((text, il.add((pos, 1)))), 
                                       any_cont(text[pos])), 
                              il.failcont(v))))(None)
    eq_(result, expect)
  