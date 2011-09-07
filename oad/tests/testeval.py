# -*- coding: utf-8 -*-
from nose.tools import eq_, ok_, assert_raises

from oad.term import SUCCESS, cons
from oad.eval import eval
from oad.special import quote, set, begin, if_, lambda_, let, letrec, eval_
from oad.special import function, macro

from oad.builtins.control import and_, cut
##from oad.builtins.module import from_
from oad.builtins.format import write
from oad.builtins.arith import eq, sub, mul, add, div
from oad.builtins.arithpred import assign
from oad.builtins.arithpred import define

from oad.testutil import *

class TestEval:
  def testInteger(self):
    eq_(eval(1), (1))    
  def testAtom(self):
    eq_(eval("1"), "1")
    eq_(eval(1), 1)
  def testArithmetic(self):
    eq_(eval(add(1, 1)), 2) 
    eq_(eval(sub(1, 1)), 0)
    eq_(eval(mul(2, 2)), 4)
    eq_(eval(div(2, 2)), 1)
  def testquote(self):
    eq_(eval(quote(x)), x)
  def testbegin(self):
    eq_(eval(begin(1, 2)), 2)
  def testif_(self):
    eq_(eval(if_(0, 0, 0)), 0)
    eq_(eval(if_(0, add, sub)(1, 1)), 0)
    eq_(eval(if_(1, add, sub)(1, 1)), 2)
  def testdefine(self):
    eq_(eval(begin(define(x,1),define(x,2))), 2)
  def testset(self):
    eq_(eval(let([(a,1)], set(a,2), a)), 2)
##    eq_(eval(let([(a,1)], 
##                  let([(b,1)], set(a,2), a))), 2)
  def testeval(self):
    eq_(eval(eval_(quote(1))), (1))
    eq_(eval(let([[x,1]], eval_(quote(x)))), 1)
    eq_(eval(eval_(quote(add(1, 1)))), (2))
    
  def testLambda(self):
    eq_(eval(lambda_([x], 1)(2)), 1)
    eq_(eval(lambda_([x], x)(2)), 2)
    eq_(eval(lambda_((x, y), add(x, y))(1, 3)), 4)
  def testlet(self):
    eq_(eval(let([(x, 1)], x)), (1))
    eq_(eval(let([(x, 1)], let([(x, 2)], x))), 2)
    eq_(eval(let(((x, 1), (y, 2)), add(x, y))), 3)
  def testletdouble(self):
    f = Var('f')
    eq_(eval(let([[f, lambda_([x], add(x, x))]], f(1))), 2)
  def testletrec(self):
    eq_(eval(letrec([(f, lambda_([n], if_(eq(n, 1), 1, f(sub(n, 1)))))],
                  f(2))), 1)
  def testletrecfac(self):
    eq_(eval(letrec([(fac, lambda_([n], if_(eq(n,1), 1, mul(n, fac(sub(n, 1))))))],
                  fac(3))), 6)
  def testletrecoddeven(self):
    eq_(eval(letrec([(odd, lambda_([n], if_(eq(n,0), 0, even(sub(n,1))))),
                    (even, lambda_([n], if_(eq(n,0), 1, odd(sub(n, 1)))))],
                  odd(3))), 1)

class Testfunction:
  def testembedvar(self):
    e, e2, f, g, h = Var('e'), Var('e2'), Var('f'), Var('g'), Var('h')
    eq_(eval(letrec([[f, macro([[cons(1, e2)], g(e2)])],
                        [g, function([[e], h(e)])],
                        [h, function([[1], True])]],
                        f(e), e)), cons(1, 1))
  def test1(self):
    eq_(eval(function([[1], 1],[[2],2])(x)), 1) 
    eq_(eval(function([[1], 1],[[x],x])(2)), (2)) 
    eq_(eval(function([[1], 1])(1)), (1)) 
    eq_(eval(function([[1], 1],[[2],2])(2)), (2)) 
  def testletrecfunction(self):
    eq_(eval(letrec([[f, function([[1], 1],[[x],f(x-1)])]], f(1))), 1)  #可以有一个UObject就可以使用运算符
    eq_(eval(letrec([[f, function([[1], 1],[[x],f(x-1)])]], f(2))), 1) 
  def testdouble(self):
    eq_(eval(function([[x], add(x, x)])(2)), 4) 
  def testdouble2(self):
    f = Var('f')
    eq_(eval(let([[f, function([[x], x+x])]], f(1))), 2) 
    eq_(eval(let([[f, function([[x], x+x])]], f(f(1)))), 4) 

  #http://en.wikibooks.org/wiki/Prolog/Cuts_and_Negatio
  def testCut(self):
    a, b, c, x = Var('a'), Var('b'), Var('c'), Var('x'), 
    eq_(eval(letrec([(a, function([[x], b(x)&cut&c(x)])), #[cut] = cut = cut() 
                     (b, function([[1], SUCCESS],
                                    [[2], SUCCESS],
                                    [[3], SUCCESS])),
                     (c, function([[1], SUCCESS])),
                    ],
             a(x), x)), (1)) 
    assert_raises(UnifyFail, 
        eval, letrec([(a, function([[x], b(x)&cut&c(x)])),
                     (b, function([[1], SUCCESS],
                                    [[2], SUCCESS],
                                    [[3], SUCCESS])),
                     (c, function([[2], SUCCESS])),
                    ],
             a(x), x))
  def testCut2(self):
    a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x'), 
##    eq_(eval(letrec([(a, function([[x], b(x)&c(x)],
##                          [[x], d(x)])),
##                     (b, function([[1], SUCCESS],
##                                    [[4], SUCCESS])),
##                     (c, function([[3], SUCCESS])),
##                     (d, function([[4], SUCCESS]))
##                    ],
##             a(x), x)), 4) 
    assert_raises(UnifyFail, 
          eval, letrec([(a, function([[x], b(x)&cut&c(x)],
                          [[x], d(x)])),
                     (b, function([[1], SUCCESS],
                                    [[4], SUCCESS])),
                     (c, function([[3], SUCCESS])),
                     (d, function([[3], SUCCESS]))
                    ],
             a(x), x))
    
class TestMacro:
  def test1(self):
    eq_(eval(macro([[], write(1)])()), SUCCESS) 
  def test2(self):
    eq_(eval(macro([[x, y], eval_(x)],
                   [[x, y],eval_(y)])(write(1), write(2))), SUCCESS) 
  def test3(self):
    eq_(eval(let([(f, macro([[x], write(eval_(x))])),
                    (x, 1)],
             f(x+x))), SUCCESS) 
    eq_(eval(let([(f, function([[x], write(x)])),
                    (x, 1)],
             f(x+x))), SUCCESS) 
    eq_(eval(let([(f, macro([[x], write(x)])),
                    (x, 1)],
             f(x+x))), SUCCESS) 
  def test4(self):
    eq_(eval(macro([[x], x])(write(1))), write(1)) 
    