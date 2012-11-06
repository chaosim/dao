# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.compiler.compile import Compiler, AlphaConvertEnvironment, VariableNotBound
from dao.compiler.compile import trampoline
from dao.compiler.command import begin, quote, assign, if_, LogicVar, let, letrec
from dao.compiler.command import add
from dao.compiler.command import fail, succeed, or_, unify, repeat, _any
from dao.compiler.command import lamda

from dao.compiler import interlang as il

v, fc = il.Var('v'), il.Var('fc')
a0, a1, a2, a3, a4 = tuple(il.Var('a'+repr(i)) for i in range(5))

class Done(il.Clamda):
  def __repr__(self): return 'done()'
  
class End(il.Clamda):
  def __repr__(self): return 'end()'
  
def done():
  return Done(v, fc, il.Return(v, fc))

def end():
  return End(v, fc, il.Return(None, None))
  
def cps_convert(exp):
  return Compiler().cps(exp, done(), end())

class TestCPSConvert:
  def test_integer(self):
    result = cps_convert(1)
    expect = done()(1, end())
    eq_(result, expect)
    
  def test_quote(self):
    result = cps_convert(quote(1))
    expect = done()(1, end())
    eq_(result, expect)
    
  def test_begin(self):
    result = cps_convert(begin(1, 2))
    expect = il.Clamda(v, fc, done()(2, end()))(1, end())
    eq_(result, expect)
  
  def test_assign(self):
    x = il.Var('x')
    result = cps_convert(assign(x, 2))
    expect = il.Clamda(v, fc, il.Assign(x, v), il.Return(v, fc))(2, end())
    eq_(result, expect)

  def test_if(self):
    result = cps_convert(if_(0, 1, 2))
    expect = il.Clamda(v, fc, il.If(v, done()(1, end()), done()(2, end())))(0, end())
    eq_(result, expect)
  
  def test_fail(self):
    result = cps_convert(fail)
    expect = end()
    eq_(result, expect)

  def test_succeed(self):
    result = cps_convert(succeed)
    expect = done()
    eq_(result, expect)
    
  def test_repeat(self):
    function = il.Var('function')
    result = cps_convert(repeat)
    expect = il.CFunction(function, v, fc, done()(v, function))
    eq_(result, expect)


  def test_or(self):
    result = cps_convert(or_(1, 2))
    expect = done()(1, il.Clamda(v, fc, done()(2, end())))
    eq_(result, expect)
    
  def test_unify(self):
    eq_(cps_convert(unify(1, 2)), end())    
    eq_(cps_convert(unify(1, 1)), done()) 
    
  def test_unify2(self):
    x = LogicVar('x')
    result = cps_convert(unify(x, 2))
    expect = il.Unify(x, 2, done(), end())
    eq_(result, expect)
    
  def test_add(self):
    result = cps_convert(add(1, 2))
    expect = il.Clamda(a0, fc, il.Clamda(a1, fc, il.Return(done()(il.add((a0, a1)), fc)))(2, end()))(1, end())
    eq_(result, expect)

  def test_lambda(self):
    x, y, k = il.Var('x'), il.Var('y'), il.Var('k')
    result = cps_convert(lamda((x,y), 1))
    expect = done()(lamda((x, y, k), k(1, end())), end())
    eq_(result, expect)
    
  def test_let(self):
    x, y, k = il.Var('x'), il.Var('y'), il.Var('k')
    result = cps_convert(let(((x,1),), x))
    expect = il.Clamda(x, fc, done()(x, end()))(1, end())
    eq_(result, expect)
    
  def test_letrec(self):
    f, k, function = il.Var('f'), il.Var('k'), il.Var('function')
    result = cps_convert(letrec([(f, lamda((), f()))], f()))
    expect = il.Clamda(v, fc, 
                       il.Assign(f, v), 
                       il.Return(v, fc))(
                         il.Lamda((k,), il.Clamda(function, fc, function(k))(f, end())), end())
    eq_(result, expect)
    
from dao.compiler.command import eoi, char, findall

class TestBuiltin:
  def test_eoi(self):
    x = il.Var('x')
    result = cps_convert(eoi)
    expect = il.Clamda(v, fc, 
                       il.If((il.get_parse_state()[1]<il.Len(il.get_parse_state()[0])), 
                             il.Return(done()(v, fc)), 
                             il.Return(end()(v, fc))))
    eq_(result, expect)

  def test_char(self):
    text, pos = il.Var('text'), il.Var('pos')
    result = cps_convert(char('a'))
    expect = il.Clamda(v, fc, 
        il.assign_from_list(text, pos, il.get_parse_state()), 
        il.If2(pos>=il.Len(text), il.Return(end()(v, fc))), 
        il.If(il.eq('a', text[pos]), 
              il.begin(il.set_parse_state((text, il.add((pos, 1)))), 
                       il.Return(done()(text[pos], il.Clamda(v, fc, 
                              il.set_parse_state((text, pos)), 
                              il.Return(end()(v, fc)))))), 
              il.Return(end()(v, fc))))
    eq_(result, expect)

  def test_char2(self):
    x = il.Var('x')
    text, pos = il.Var('text'), il.Var('pos')
    result = cps_convert(char(x))
    expect = il.Clamda(v, fc, 
        il.assign_from_list(text, pos, il.get_parse_state()), 
        il.If2((pos>=il.Len(text)), il.Return(end()(v, fc))), 
        il.Return(il.Unify(x, text[pos], 
                  il.Clamda(v, fc, 
                      il.begin(il.set_parse_state((text, il.add((pos, 1)))), 
                               il.Return(done()(text[pos], 
                                                il.Clamda(v, fc, 
                                                          il.set_parse_state((text, pos)), 
                                                          il.Return(end()(v, fc))))))), 
                  end())(v, fc)))
    eq_(result, expect)

  def test_findall(self):
    result = cps_convert(findall(or_(1, 2)))
    expect = il.Clamda(v, fc, 
        il.Clamda(v, fc, 
          il.Return(fc(v, fc)))(1, 
            il.Clamda(v, fc, 
              il.Clamda(v, fc, il.Return(fc(v, fc)))(2, 
                  il.Clamda(v, fc, il.Return(done()(v, end())))))
            ))
    eq_(result, expect)
    
  def test_findall2(self):
    x, y = il.Var('x'), il.Var('result')
    result = cps_convert(findall(or_(1, 2), x, y))
    expect = il.Clamda(v, fc, 
      il.Assign(y, il.empty_list()), 
      il.Clamda(v, fc, 
          il.list_append(y, il.getvalue(x)), 
          il.Return(fc(v, fc)))(1, 
              il.Clamda(v, fc, 
                  il.Clamda(v, fc, 
                    il.list_append(y, il.getvalue(x)), 
                    il.Return(fc(v, fc)))(2, 
                        il.Clamda(v, fc, 
                            il.Return(il.Unify(y, y, done(), end())(v, fc)))))))
    eq_(result, expect)
    
  def test_any(self):
    x, y = il.Var('x'), il.Var('result')
    v1, fc1 = il.Var('v1'), il.Var('fc1')
    function = il.Var('function')
    result = cps_convert(_any(1))
    expect = il.Clamda(v, fc, il.CFunction(function, v, fc, function(1, il.Clamda(v, fc, il.Return(done()(v, fc)))))(v, end()))
    eq_(result, expect)
      
  def test_any2(self):
    x, y = il.Var('x'), il.Var('result')
    v1, fc1 = il.Var('v1'), il.Var('fc1')
    text, pos = il.Var('text'), il.Var('pos')
    function = il.Var('function')
    result = cps_convert(_any(char('1')))
    expect = il.Clamda(v, fc, 
                  il.CFunction(function, v, fc, 
                      il.Clamda(v, fc, 
                          il.assign_from_list(text, pos, il.get_parse_state()), 
                          il.If2((pos>=il.Len(text)), il.Return(il.Clamda(v, fc, il.Return(done()(v, fc)))(v, fc))), 
                          il.If(il.eq('1', text[pos]), 
                                il.begin(il.set_parse_state((text, il.add((pos, 1)))), 
                                         il.Return(function(text[pos], 
                                                            il.Clamda(v, fc, 
                                                                      il.set_parse_state((text, pos)), 
                                                                      il.Return(il.Clamda(v, fc, il.Return(done()(v, fc)))(v, fc)))))), 
                                il.Return(il.Clamda(v, fc, il.Return(done()(v, fc)))(v, fc)))))(v, end()))
    eq_(result, expect)
  