# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.solve import eval

from dao.command import quote, assign, begin, if_
from dao.command import not_p, fail, succeed, or_
from dao.command import unify, lamda, let, letrec
from dao.command import settext, char, eoi, any
from dao.command import add, eq, sub
from dao.command import eval_

from dao.solvebase import NoSolution

from dao import interlang as il

class TestSimple:
  def test_integer(self):
    eq_(eval(il.Integer(1)), 1)
    
  def test_string(self):
    eq_(eval(il.String("1")), "1")
    
  def test_arithmetic(self):
    eq_(eval(add(1, 2)), 3) 
    #eq_(eval(sub(1, 1)), 0)
    #eq_(eval(mul(2, 2)), 4)
    #eq_(eval(div(2, 2)), 1)
    
  def test_quote(self):
    lo_x = il.LogicVar('$x')
    eq_(eval(quote(lo_x)), il.LogicVar('$x'))
    
  def testassign(self):
    a = il.Var('a')
    eq_(eval(assign(a,2)), None)
    
  def xtestdefine(self):
    eq_(eval(begin(define(x,1),define(x,2))), 2)
    
class TestControl:
  def test_begin(self):
    eq_(eval(begin(1, 2)), 2)
    
  def test_eval(self):
    eq_(eval(eval_(quote(begin(1, 2)))), 2)
    eq_(eval(eval_(quote(begin(1, add(1, 1))))), 2)
    
  def testif_(self):
    eq_(eval(if_(0, 1, 2)), 2)
    
  def test_not_(self):
    eq_(eval(not_p(fail)), True)
    
  def test_not_2(self):
    assert_raises(NoSolution, eval, not_p(succeed))
    
  def test_or_(self):
    eq_(eval(or_(succeed, fail)), True)
    eq_(eval(or_(unify(1,1), unify(1,2))), True)
    
  def test_lamda(self):
    x = il.Var('x')
    eq_(eval(lamda((x,), 1)(1)), 1)
    
  def test_let(self):
    x = il.Var('x')
    eq_(eval(let([(x, 1)], x)), 1)
    eq_(eval(let([(x, 1)], let([(x, 2)], x))), 2)
    eq_(eval(let([(x, 1)], let([(x, 2)], assign(x, 2)))), None)
    
  def test_letrec(self):
    x, y = il.Var('x'), il.Var('y')
    eq_(eval(letrec([(x, 1), (y, x)], y)), 1)
    eq_(eval(letrec([(x, 1), (y, add(x, 1))], y)), 2)
    
  def test_letrec2(self):
    x, f = il.Var('x'), il.Var('f')
    eq_(eval(letrec([(f, lamda((x,), if_(eq(x,1), 1, f(sub(x,1)))))], f(2))), 1)
    
  def test_unify(self):
    x = il.Var('x')
    Lx = il.LogicVar('$x')
    assert_raises(NoSolution, eval, begin(unify(Lx, 1), unify(Lx,2)))
    eq_(eval(let([(x,1)], unify(x,1))), True)
    eq_(eval(unify(Lx,1)), True)
    eq_(eval(begin(unify(Lx, 1), unify(Lx,1))), True)
    assert_raises(NoSolution, eval, begin(unify(1, 1), unify(1, 2)))
    assert_raises(NoSolution, eval, begin(unify(2, 1), unify(1, 1)))
    assert_raises(NoSolution, eval, unify(1, 2))
    eq_(eval(unify(1, 1)), True)
    eq_(eval(begin(unify(1, 1), unify(2, 2))), True)
    
  def test_parse(self):
    eq_(eval(begin(settext('abcde'), char('a'))), 'a')
    
  def test_parse2(self):
    eq_(eval(begin(settext('a'), char('a'), eoi)), True)
    assert_raises(NoSolution, eval, begin(settext('ab'), char('a'), eoi))
    
  def test_parse3(self):
    eq_(eval(begin(settext('aaa'), any(char('a')), eoi)), True)
    
  #def testif_add_sub(self):
    #eq_(eval(if_(0, add, sub)(1, 1)), 0)
    #eq_(eval(if_(1, add, sub)(1, 1)), 2)
  #def testiff(self):
    #eq_(eval(iff(((0, prin(1)), (1, prin(2))))), None)
  #def testiff2(self):
    #eq_(eval(iff(((0, prin(1)), (0,prin(2))), prin(3))), None)
  #def testCaseForm(self):
    #eq_(eval(CaseForm(2, {0: [prin(0)], 1:[prin(1)], 2:[prin(2)]}, [prin(3)])), None)
  #def testeval1(self):
    #eq_(eval(eval_(quote(1))), (1))
    #eq_(eval(eval_(quote(add(1, 1)))), (2))
  #def testeval2(self):
    #eq_(eval(let([(x,1)], eval_(quote(x)))), 1)

class XTestLoop:
  def testloop(self):
    eq_(eval(let([(i,3)], 
                 block(a, set(i, sub(i, 1)), 
                            if_(eq(i, 0), exit_block(a, 1)),
                            continue_block(a)), i)), 0)
  def test_unwind_protect_loop(self):
    eq_(eval(let([(i,3)], 
                 block(a, set(i, sub(i, 1)), 
                            if_(eq(i, 0), exit_block(a, 1)),
                            unwind_protect(continue_block(a), prin(2))), i)), 0)
  def testLoopTimes(self):
    eq_(eval(tag_loop_label(let([(i,3)], LoopTimesForm(3, (set(i, sub(i, 1)), prin(i)))))), None)
  def testLoopWhen(self):
    eq_(eval(tag_loop_label(let([(i,3)], LoopWhenForm((set(i, sub(i, 1)), prin(i)), 
                                     gt(i,0))))), None)
  def testLoopUntil(self):
    eq_(eval(tag_loop_label(let([(i,3)], LoopUntilForm((set(i, sub(i, 1)), prin(i)), 
                                     eq(i,0))))), None)
  def testEachForm(self):
    eq_(eval(tag_loop_label(EachForm(i, range(3), [prin(i)]))), None)
  def testEachForm2(self):
    eq_(eval(tag_loop_label(EachForm((i, j), zip(range(3), range(3)), [prin(i, j)]))), None)
    
class XTestFunction:
  def test_let_set(self):
    eq_(eval(let([(a,1)], set(a,2), a)), 2)
    eq_(eval(let([(a,1)], 
                  let([(b,1)], set(a,2), a))), 2)
  def testLambda(self):
    eq_(eval(lambda_([x], 1)(2)), 1)
    eq_(eval(lambda_([x], x)(2)), 2)
    eq_(eval(lambda_((x, y), add(x, y))(1, 3)), 4)
  def testlet(self):
    eq_(eval(let([(x,1)], x)), (1))
    eq_(eval(let([(x,1)], let([(x,2)], x))), 2)
    eq_(eval(let([(x,1), (y,2)], add(x, y))), 3)
  def testletdouble(self):
    f = Var('f')
    eq_(eval(let([(f, lambda_([x], add(x, x)))], f(1))), 2)
  def test1(self):
    eq_(eval(function([[1], 1],[[x],x])(2)), 2) 
    eq_(eval(function([[1], 1])(1)), 1) 
    eq_(eval(function([[1], 1],[[2],2])(2)), 2) 
    eq_(eval(function([[1], 1],[[2],2])(x)), 1) 
  def testdouble(self):
    eq_(eval(function([[x], add(x, x)])(2)), 4) 
  def testdouble2(self):
    f = Var('f')
    eq_(eval(let([(f, function([[x], x+x]))], f(1))), 2) 
    eq_(eval(let([(f, function([[x], x+x]))], f(f(1)))), 4) 
    
class XTest_letr:
  def testembedvar1(self):
    e, e2, f, g, h = Var('e'), Var('e2'), Var('f'), Var('g'), Var('h')
    eq_(eval(letr([(f, function([[1], 1]))],
                f(e), e)), 1)
  def testembedvar2(self):
    e, e2, f, g, h = Var('e'), Var('e2'), Var('f'), Var('g'), Var('h')
    eq_(eval(letr([(f, macro([[cons(1, e2)], g(e2)])),
                     (g, function([[e], h(e)])),
                     (h, function([[1], True]))],
                f(e), e)), cons(1, 1))
  def testletr(self):
    eq_(eval(letr([(f, function([[1], 1],[[x],f(x-1)]))], f(1))), 1)
    eq_(eval(letr([(f, function([[1], 1],[[x],f(x-1)]))], f(2))), 1) 
  def testletr(self):
    eq_(eval(letr([(f, lambda_([n], if_(eq(n, 1), 1, f(sub(n, 1)))))],
                  f(2))), 1)
  def testletrfac(self):
    eq_(eval(letr([(fac, lambda_([n], if_(eq(n,1), 1, mul(n, fac(sub(n, 1))))))],
                  fac(3))), 6)
  def testletroddeven(self):
    eq_(eval(letr([(odd, lambda_([n], if_(eq(n,0), 0, even(sub(n,1))))),
                    (even, lambda_([n], if_(eq(n,0), 1, odd(sub(n, 1)))))],
                  odd(3))), 1)

class XTestCut:
  #http://en.wikibooks.org/wiki/Prolog/Cuts_and_Negatio
  def testCut1(self):
    a, b, c, x = Var('a'), Var('b'), Var('c'), Var('x'), 
    eq_(eval(letr([(a, function([[x], b(x)&cut&c(x)])), #[cut] = cut = cut() 
                     (b, function([[1], True],
                                 [[2], True],
                                 [[3], True])),
                     (c, function([[1], True]))],
             a(x), x)), (1)) 
  def test_cut2(self):
    a, b, c, x = Var('a'), Var('b'), Var('c'), Var('x'), 
    assert_raises(NoSolutionFound, eval, letr([(a, function([[x], b(x)&cut&c(x)])),
                     (b, function([[1], True],
                                    [[2], True],
                                    [[3], True])),
                     (c, function([[2], True]))],
             a(x), x))
  def test_cut2_no_Cut_and_p(self):
    a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x'), 
    eq_(eval(letr([(a, function([[x], b(x)&c(x)],
                                 [[x], d(x)])),
                     (b, function([[1], 'b1'],
                                 [[4], 'b4'])),
                     (c, function([[4], 'c4'])),
                     (d, function([[3], 'd3']))],
             a(x), x)), 4) 
  def test_cut2_no_Cut2_and_(self):
    # test_cut2_no_Cut_and_p work correct.
    # but this test and test_cut2_no_Cut3_begin work wrong because the bug below:
    # bug in Var.getvalue/Var.setvalue: 
    # dont't restore the longer chain of bindings after shorten it.
    a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x')
    from dao.builtins.arith import and_
    eq_(eval(letr([(a, function([[x], and_(b(x),c(x))],
                                 [[x], d(x)])),
                     (b, function([[1], True],
                                 [[4], True])),
                     (c, function([[4], True])),
                     (d, function([[3], True]))],
             a(x), x)), 4) 
  def test_cut2_no_Cut3_begin(self):
    a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x')
    eq_(eval(letr([(a, function([[x], begin(b(x),c(x))],
                                 [[x], d(x)])),
                     (b, function([[1], 'b1'],
                                 [[4], 'b4'])),
                     (c, function([[4], 'c4'])),
                     (d, function([[3], 'd3']))],
             a(x), x)), 4) 
  def testCut4(self):
    a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x'), 
    eq_(eval(letr([(a, function([[x], b(x)&cut&c(x)],
                          [[x], d(x)])),
                     (b, function([[1], 'b1'],
                                 [[4], 'b4'])),
                     (c, function([[4], 'c4'])),
                     (d, function([[3], 'd3']))],
             a(x), x)), 3)
    
class XTestMacro:
  def test1(self):
    eq_(eval(macro([[], prin(1)])()), None) 
    
  def test_eval(self):
    eq_(eval(macro([[x, y], eval_(x)],
                   [[x, y],eval_(y)])(prin(1), prin(2))), None) 
  def test_or_p(self):
    eq_(eval(macro([[x, y], x],
                   [[x, y],y])(prin(1), prin(2))), None) 
  def test_closure1(self):
    eq_(eval(let([(f, macro([[x], prin(eval_(x))])),
                  (x, 1)],
             f(x+x))), None) 
  def test_closure2(self):
    eq_(eval(let([(f, macro([[x], prin(x)])),
                  (x, 1)],
             f(x+x))), None) 
  def test_closure3(self):
    eq_(eval(let([(f, function([[x], prin(x)])),
                  (x, 1)],
             f(x+x))), None) 
  def test_closure4(self):
    eq_(eval(let([(f, macro([[x], x])),
                  (x, 1)],
             f(x+x))), 2) 
  def test4(self):
    eq_(eval(macro([[x], x])(prin(1))), None) 
    
class XTestCallccBlockCatch:
  def testblock(self):
    f = Var('f')
    eq_(eval(block('foo', let([(f, lambda_((), exit_block('foo',1)))], 
                            mul(2,block('foo', f()))))), 
        1)
  def testblock2(self):
    eq_(eval(block('a', exit_block('a', 2), 3)), 2)
  def testcatch1(self):
    eq_(eval(catch(1, 2)), 2)
  def testcatch2(self):
    eq_(eval(catch(1, throw(1, 2), 3)), 2)
  def test_unwind_protect(self):
    eq_(eval(block('foo', unwind_protect(exit_block('foo', 1), prin(2)))), 1)
  def test_unwind_protect2(self):
    eq_(eval(block('foo', unwind_protect(exit_block('foo', 1), 
                            prin(2), prin(3)))), 1)
  def testcallcc1(self):
    from dao.solve import done
    eq_(eval(callcc(lambda_([k], k(2)))), 2)
  def testcallcc2(self):
    from dao.solve import done
    assert_raises(DaoError, eval, callcc(lambda_([k], k(2,3))))
  def testcallcc3(self):
    eq_(eval(callcc(callcc)), ContinuationFunction(done))
  #def testcallcc4(self):
    ##assert 0, '((call/cc call/cc) (call/cc call/cc)) infinite loop.'

class XTestModule:
  def testbindings(self):
    a = Var('a')
    m1 = eval(module(define(a,1)))
    ok_(a in m1.bindings)    
    
  def testfrom(self):
    a, m = Var('a'), Var('m')
    eq_(eval(let([(m,module(define(a,1)))], from_(m,a))), 1)
    
  def test_embeded_module(self):
    a, m1, m2 = Var('a'), Var('m1'), Var('m2')
    eq_(eval(let([(m1,module(define(a,1),define(m2, module(define(a,2)))))], 
               from_(from_(m1,m2),a))), 2) 

  def test_let_in_module(self):
    a, m1, m2 = Var('a'), Var('m1'), Var('m2')
    eq_(eval(let([(m1,module(define(a,1), let([(a,2)], define(a,3))))], 
               from_(m1,a))), 1) 

