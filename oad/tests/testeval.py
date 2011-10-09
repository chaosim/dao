# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad.term import cons
from oad.solve import eval
from oad.special import quote, set, begin, if_, iff, lambda_, let, letrec, eval_
from oad.special import function, macro, block, return_from, catch, throw
from oad.special import unwind_protect, module, loop, CaseForm
from oad.special import LoopTimesForm, LoopUntilForm, LoopWhenForm, EachForm

from oad.builtins.control import and_, cut
from oad.builtins.module import from_
from oad.builtins.format import write
from oad.builtins.arith import gt, eq, sub, mul, add, div
from oad.builtins.arithpred import define
from oad.builtins.callcc import callcc

from oad.testutil import *

class TestSimple:
  def testInteger(self):
    eq_(eval(1), 1)    
  def testAtom(self):
    eq_(eval("1"), "1")
    eq_(eval(1), 1)
  def testArithmetic(self):
    eq_(eval(add(1, 2)), 3) 
    eq_(eval(sub(1, 1)), 0)
    eq_(eval(mul(2, 2)), 4)
    eq_(eval(div(2, 2)), 1)
  def testquote(self):
    eq_(eval(quote(x)), x)
  def testset(self):
    eq_(eval(set(a,2)), 2)
  def testdefine(self):
    eq_(eval(begin(define(x,1),define(x,2))), 2)
    
class TestControl:
  def testbegin(self):
    eq_(eval(begin(1, 2)), 2)
  def testif_(self):
    eq_(eval(if_(0, 1, 2)), 2)
  def testif_add_sub(self):
    eq_(eval(if_(0, add, sub)(1, 1)), 0)
    eq_(eval(if_(1, add, sub)(1, 1)), 2)
  def testiff(self):
    eq_(eval(iff(((0, write(1)), (1,write(2))))), True)
  def testiff2(self):
    eq_(eval(iff(((0, write(1)), (0,write(2))), write(3))), True)
  def testCaseForm(self):
    eq_(eval(CaseForm(2, {0: [write(0)], 1:[write(1)], 2:[write(2)]}, [write(3)])), True)
  def testeval(self):
    eq_(eval(eval_(quote(1))), (1))
    eq_(eval(let({x:1}, eval_(quote(x)))), 1)
    eq_(eval(eval_(quote(add(1, 1)))), (2))

class TestLoop:
  def testloop(self):
    eq_(eval(let({i:3}, 
                 block(a, 
                       loop(set(i, sub(i, 1)), 
                            if_(eq(i, 0), return_from(a, 1)))), i)), 0)
    # eq_(eval(loop(0)), 0) infinite loop
  def testLoopTimes(self):
    eq_(eval(let({i:3}, LoopTimesForm(3, (set(i, sub(i, 1)), write(i))))), None)
  def testLoopWhen(self):
    eq_(eval(let({i:3}, LoopWhenForm((set(i, sub(i, 1)), write(i)), 
                                     gt(i,0)))), None)
  def testLoopUntil(self):
    eq_(eval(let({i:3}, LoopUntilForm((set(i, sub(i, 1)), write(i)), 
                                     eq(i,0)))), None)
  def testEachForm(self):
    eq_(eval(EachForm(i, range(3), [write(i)])), None)
  def testEachForm2(self):
    eq_(eval(EachForm((i, j), zip(range(3), range(3)), [write(i, j)])), None)
    
class TestFunction:
  def test_let_set(self):
    eq_(eval(let({a:1}, set(a,2), a)), 2)
    eq_(eval(let({a:1}, 
                  let({b:1}, set(a,2), a))), 2)
  def testLambda(self):
    eq_(eval(lambda_([x], 1)(2)), 1)
    eq_(eval(lambda_([x], x)(2)), 2)
    eq_(eval(lambda_((x, y), add(x, y))(1, 3)), 4)
  def testlet(self):
    eq_(eval(let({x:1}, x)), (1))
    eq_(eval(let({x:1}, let({x:2}, x))), 2)
    eq_(eval(let({x:1, y:2}, add(x, y))), 3)
  def testletdouble(self):
    f = Var('f')
    eq_(eval(let({f: lambda_([x], add(x, x))}, f(1))), 2)
  def test1(self):
    eq_(eval(function([[1], 1],[[2],2])(x)), 1) 
    eq_(eval(function([[1], 1],[[x],x])(2)), 2) 
    eq_(eval(function([[1], 1])(1)), 1) 
    eq_(eval(function([[1], 1],[[2],2])(2)), 2) 
  def testdouble(self):
    eq_(eval(function([[x], add(x, x)])(2)), 4) 
  def testdouble2(self):
    f = Var('f')
    eq_(eval(let({f: function([[x], x+x])}, f(1))), 2) 
    eq_(eval(let({f: function([[x], x+x])}, f(f(1)))), 4) 
    
class Test_letrec:
  def testembedvar1(self):
    e, e2, f, g, h = Var('e'), Var('e2'), Var('f'), Var('g'), Var('h')
    eq_(eval(letrec({f: function([[1], 1])},
                f(e), e)), 1)
  def testembedvar2(self):
    e, e2, f, g, h = Var('e'), Var('e2'), Var('f'), Var('g'), Var('h')
    eq_(eval(letrec({f: macro([[cons(1, e2)], g(e2)]),
                     g: function([[e], h(e)]),
                     h: function([[1], True])},
                f(e), e)), cons(1, 1))
  def testletrec(self):
    eq_(eval(letrec({f: function([[1], 1],[[x],f(x-1)])}, f(1))), 1)
    eq_(eval(letrec({f: function([[1], 1],[[x],f(x-1)])}, f(2))), 1) 
  def testletrec(self):
    eq_(eval(letrec({f: lambda_([n], if_(eq(n, 1), 1, f(sub(n, 1))))},
                  f(2))), 1)
  def testletrecfac(self):
    eq_(eval(letrec({fac: lambda_([n], if_(eq(n,1), 1, mul(n, fac(sub(n, 1)))))},
                  fac(3))), 6)
  def testletrecoddeven(self):
    eq_(eval(letrec({odd: lambda_([n], if_(eq(n,0), 0, even(sub(n,1)))),
                    even: lambda_([n], if_(eq(n,0), 1, odd(sub(n, 1))))},
                  odd(3))), 1)

class TestCut:
  #http://en.wikibooks.org/wiki/Prolog/Cuts_and_Negatio
  def testCut1(self):
    a, b, c, x = Var('a'), Var('b'), Var('c'), Var('x'), 
    eq_(eval(letrec({a: function([[x], b(x)&cut&c(x)]), #[cut] = cut = cut() 
                     b: function([[1], True],
                                 [[2], True],
                                 [[3], True]),
                     c: function([[1], True])},
             a(x), x)), (1)) 
  def test_cut2(self):
    a, b, c, x = Var('a'), Var('b'), Var('c'), Var('x'), 
    eq_(eval(letrec({a: function([[x], b(x)&cut&c(x)]),
                     b: function([[1], True],
                                    [[2], True],
                                    [[3], True]),
                     c: function([[2], True])},
             a(x), x)), None)
  def test_cut2_no_Cut(self):
    a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x'), 
    eq_(eval(letrec({a: function([[x], b(x)&c(x)],
                                 [[x], d(x)]),
                     b: function([[1], True],
                                 [[4], True]),
                     c: function([[3], True]),
                     d: function([[4], True])},
             a(x), x)), 4) 
  def testCut4(self):
    a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x'), 
    eq_(eval(letrec({a: function([[x], b(x)&cut&c(x)],
                          [[x], d(x)]),
                     b: function([[1], True],
                                 [[4], True]),
                     c: function([[3], True]),
                     d: function([[3], True])},
             a(x), x)), 3)
    
class TestMacro:
  def test1(self):
    eq_(eval(macro([[], write(1)])()), True) 
  def test_eval(self):
    eq_(eval(macro([[x, y], eval_(x)],
                   [[x, y],eval_(y)])(write(1), write(2))), True) 
  def test_closure(self):
    eq_(eval(let({f: macro([[x], write(eval_(x))]),
                  x: 1},
             f(x+x))), True) 
    eq_(eval(let({f: function([[x], write(x)]),
                  x: 1},
             f(x+x))), True) 
    eq_(eval(let({f: macro([[x], write(x)]),
                  x: 1},
             f(x+x))), True) 
  def test4(self):
    eq_(eval(macro([[x], x])(write(1))), write(1)) 
##    eq_(eval(macro([[x], x])(write(1))), (write, 1)) #on to_sexpression
    
class TestCallccBlockCatch:
  def testblock(self):
    f = Var('f')
    eq_(eval(block('foo', let({f: lambda_((), return_from('foo',1))}, 
                            mul(2,block('foo', f()))))), 
        1)
  def testblock2(self):
    eq_(eval(block('a', return_from('a', 2), 3)), 2)
  def testcatch1(self):
    eq_(eval(catch(1, 2)), 2)
  def testcatch2(self):
    eq_(eval(catch(1, throw(1, 2), 3)), 2)
  def test_unwind_protect(self):
    eq_(eval(block('foo', unwind_protect(return_from('foo', 1), write(2)))), 1)
  def test_unwind_protect2(self):
    eq_(eval(block('foo', unwind_protect(return_from('foo', 1), 
                            write(2), write(3)))), 1)
  def testcallcc(self):
    from oad.solve import done
    eq_(eval(callcc(lambda_([k], k(2)))), 2)
##    eq_(eval(callcc(callcc)), done)
    #assert 0, '((call/cc call/cc) (call/cc call/cc)) infinite loop.'
    #eq_(eval('((call/cc call/cc) (call/cc call/cc))'), Integer(2))

class TestModule:
  def testbindings(self):
    a = Var('a')
    m1 = eval(module(define(a,1)))
    ok_(a in m1.bindings)    
    
  def testfrom(self):
    a, m = Var('a'), Var('m')
    eq_(eval(let({m:module(define(a,1))}, from_(m,a))), 1)
    
  def test_embeded_module(self):
    a, m1, m2 = Var('a'), Var('m1'), Var('m2')
    eq_(eval(let({m1:module(define(a,1),define(m2, module(define(a,2))))}, 
               from_(from_(m1,m2),a))), 2) 

  def test_let_in_module(self):
    a, m1, m2 = Var('a'), Var('m1'), Var('m2')
    eq_(eval(let({m1:module(define(a,1), let({a:2}, define(a,3)))}, 
               from_(m1,a))), 1) 

