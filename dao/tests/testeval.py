# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.solve import eval

from dao.command import Var, LogicVar, Const, MultiAssignToConstError

from dao.builtins import Integer, String
from dao.builtins import quote, begin, if_, assign
from dao.builtins import unify, not_p, fail, succeed, or_, cut, findall
from dao.builtins import lamda, let, letrec, rules, macro
from dao.builtins import set_text, char, eoi, any
from dao.builtins import add, eq, sub, mul
from dao.builtins import eval_, callcc
from dao.builtins import block, exit_block, continue_block
from dao.builtins import catch, throw, unwind_protect
from dao.builtins import prin

from dao.solvebase import NoSolution

from dao import interlang as il

class TestSimple:
  def test_integer(self):
    eq_(eval(Integer(1)), 1)
    
  def test_string(self):
    eq_(eval(String("1")), "1")
    
  def test_arithmetic(self):
    eq_(eval(add(1, 2)), 3) 
    #eq_(eval(sub(1, 1)), 0)
    #eq_(eval(mul(2, 2)), 4)
    #eq_(eval(div(2, 2)), 1)
    
  def test_quote(self):
    lo_x = LogicVar('x')
    eq_(eval(quote(lo_x)), LogicVar('x'))
    
  def test_quote2(self):
    x = LogicVar('x')
    eq_(eval(quote(x)), LogicVar('x'))
    
  def testassign(self):
    a = Var('a')
    #eq_(eval(assign(a,2)), 2)
    eq_(eval(begin(assign(a,2), a)), 2)
    
  def testassign2(self):
    a = Const('a')
    assert_raises(MultiAssignToConstError, eval, begin(assign(a,2), assign(a, 3)))
    
  def xtestdefine(self):
    eq_(eval(begin(define(x,1),define(x,2))), 2)
    
class TestControl:
  def test_let_assign_const(self):
    a = Const('a')
    assert_raises(MultiAssignToConstError, eval, 
                  let([(a,2)], assign(a, 3)))
    
  def test_begin(self):
    eq_(eval(begin(1, 2)), 2)
    
  def testeval5(self):
    eq_(eval(eval_(quote(begin(1, 2)))), 2)
    eq_(eval(eval_(quote(begin(1, add(1, 2))))), 3)
    
  def testeval2(self):
    x = Var('x')
    eq_(eval(let([(x,1)], eval_(quote(x)))), 1)
    
  def testeval3(self):
    x = Var('x')
    eq_(eval(let([(x, quote(1))], eval_(x))), 1)
    
  def testeval4(self):
    x = Var('x')
    eq_(eval(let([(x, quote(add(1,1)))], eval_(x))), 2)
    
  def testeval1(self):
    eq_(eval(eval_(quote(1))), (1))
    eq_(eval(eval_(quote(add(1, 1)))), (2))
    
  def test_callcc(self):
    k = Var('k')
    eq_(eval(add(callcc(lamda((k,), k(1))),2)), 3)
    
  def testif_(self):
    eq_(eval(if_(0, 1, 2)), 2)
    
  def test_not_(self):
    eq_(eval(not_p(fail)), True)
    
  def test_not_2(self):
    assert_raises(NoSolution, eval, not_p(succeed))
    
  def test_or_(self):
    eq_(eval(or_(succeed, fail)), True)
    
  def test_or_2(self):
    eq_(eval(or_(unify(1,1), unify(1,2))), True)
    
  #def testif_add_sub(self):
    #eq_(eval(if_(0, add, sub)(1, 1)), 0)
    #eq_(eval(if_(1, add, sub)(1, 1)), 2)
  #def testiff(self):
    #eq_(eval(iff(((0, prin(1)), (1, prin(2))))), None)
  #def testiff2(self):
    #eq_(eval(iff(((0, prin(1)), (0,prin(2))), prin(3))), None)
  #def testCaseForm(self):
    #eq_(eval(CaseForm(2, {0: [prin(0)], 1:[prin(1)], 2:[prin(2)]}, [prin(3)])), None)

class TestLambdaLet:
  def test_lamda(self):
    x, y = Var('x'), Var('y')
    eq_(eval(lamda((x,), 1)(1)), 1)
    eq_(eval(lamda((x,), 1)(2)), 1)
    eq_(eval(lamda((x,), x)(2)), 2)
    
  def test_lamda2(self):
    x, y = Var('x'), Var('y')
    eq_(eval(lamda((x, y), add(x, y))(1, 3)), 4)
    
  def test_let(self):
    x = Var('x')
    eq_(eval(let([(x, 1)], x)), 1)
    eq_(eval(let([(x, 1)], let([(x, 2)], x))), 2)
    eq_(eval(let([(x, 1)], let([(x, 2)], assign(x, 2)))), 2)
    
  def test_let2(self):
    x, y = Var('x'), Var('y')
    eq_(eval(let([(x,1), (y,2)], add(x, y))), 3)
    
  def test_let_assign(self):
    a, b = Var('a'), Var('b')
    eq_(eval(let([(a, 1)], assign(a,2), a)), 2)
    eq_(eval(let([(a,1)], 
                  let([(b,1)], assign(a,2), a))), 2)
    
  def test_letrec(self):
    x, y = Var('x'), Var('y')
    eq_(eval(letrec([(x, 1), (y, x)], y)), 1)
    eq_(eval(letrec([(x, 1), (y, add(x, 1))], y)), 2)
    
  def xtest_letrec2(self):
    x, f = Var('x'), Var('f')
    eq_(eval(letrec([(f, lamda((x,), f(1)))], f(2))), 1)
    
  def test_letrec_assign(self):
    x, f = Var('x'), Var('f')
    assert_raises(MultiAssignToConstError, eval,
                  letrec([(f, lamda((x,), f(1)))], f(2), assign(f, 1)))
    
  def test_letrec_assign2(self):
    x, f = Var('x'), Var('f')
    assert_raises(MultiAssignToConstError, eval,
                  letrec([(f, lamda((x,), f(1), assign(f, 1)))], f(2)))
    
  def test_letrec3(self):
    x, f = Var('x'), Var('f')
    eq_(eval(letrec([(f, lamda((x,), if_(eq(x,1), 1, f(sub(x,1)))))], f(2))), 1)
    
  def testletdouble(self):
    x, f = Var('x'), Var('f')
    eq_(eval(let([(f, lamda([x], add(x, x)))], f(1))), 2)
    
  def test_letrec_fac(self):
    from util import m, n, fac
    eq_(eval(letrec([(fac, lamda([n], if_(eq(n,1), 1, mul(n, fac(sub(n, 1))))))],
                  fac(3))), 6)
    
  def test_letrec_odd_even(self):
    from util import n, odd, even  
    eq_(eval(letrec([(odd, lamda([n], if_(eq(n,0), 0, even(sub(n,1))))),
                    (even, lamda([n], if_(eq(n,0), 1, odd(sub(n, 1)))))],
                  odd(3))), 1)

class TestLispConstruct:    
  def testblock(self):
    f = Var('f')
    foo = Var('foo')
    eq_(eval(block(foo, let([(f, lamda((), exit_block(foo,1)))], 
                            mul(2, block(foo, f()))))), 
        1)
    
  def testblock2(self):
    a = Var('a')
    eq_(eval(block(a, exit_block(a, 2), 3)), 2)
    
  def testblock3(self):
    a = Var('a')
    eq_(eval(block(a, 1, if_(0, continue_block(a), 
                             begin(exit_block(a, 2), 3)))), 2)

  def testloop(self):
    from util import a, i
    eq_(eval(let([(i,3)], 
                 block(a, assign(i, sub(i, 1)), 
                          if_(eq(i, 0), 
                              exit_block(a, 1), 
                              continue_block(a))), 
                 i)), 0)
    
  def test_unwind_protect_loop(self):
    from util import a, i
    eq_(eval(let([(i,3)], 
                 block(a, assign(i, sub(i, 1)), 
                          if_(eq(i, 0), 
                            exit_block(a, 1),
                            unwind_protect(continue_block(a), prin(i)))), i)), 0)
    
  def testcatch1(self):
    eq_(eval(catch(1, 2)), 2)
    
  def testcatch2(self):
    eq_(eval(catch(1, throw(1, 2), 3)), 2)
    
  def test_unwind_protect(self):
    foo = Var('foo')
    eq_(eval(block(foo, unwind_protect(exit_block(foo, 1), prin(2)))), 1)
    
  def test_unwind_protect2(self):
    foo = Var('foo')
    eq_(eval(block(foo, unwind_protect(exit_block(foo, 1), 
                            prin(2), prin(3)))), 1)

class TestRules:
  def test1(self):
    x = Var('x')
    lx = LogicVar('x')
    eq_(eval(rules([[x],x])(2)), 2) 
    eq_(eval(rules([[1], 1],[[x],x])(2)), 2) 
    eq_(eval(rules([[1], 1])(1)), 1) 
    
  def test1_2(self):
    eq_(eval(rules([[1], 1],[[2],2])(2)), 2) 
    
  def test2(self):
    lx = LogicVar('x')
    eq_(eval(rules([[1], 1],[[2],2])(lx)), 1)
    
  def testdouble1(self):
    x = Var('x')
    eq_(eval(rules([[x], add(x, x)])(2)), 4)
    eq_(eval(rules([[x], x])(add(2, 2))), 4)
    
  def testdouble2(self):
    f = Var('f')
    x = Var('x')
    eq_(eval(let([(f, rules([[x], add(x, x)]))], f(1))), 2) # passed
    
  def testdouble3(self):
    f = Var('f')
    x = Var('x')
    eq_(eval(let([(f, rules([[x], add(x, x)]))], f(f(1)))), 4) 
    
  def testdouble4(self):
    f = Var('f')
    x = Var('x')
    assert_raises(NoSolution, eval, let([(f, rules([[x], add(x, x)]))], f(1, 2))) # passed
    
  def test_embed_var1(self):
    e, f = LogicVar('e'), Var('f')
    #eq_(eval(let([(f, rules([[1], 1]))], f(e), e)), 1)
    eq_(eval(letrec([(f, rules([[1], 1]))], f(e))), 1)
    
  def test_embed_var2(self):
    e, f = LogicVar('e'), Var('f')
    eq_(eval(let([(f, rules([[1], 1]))], f(e), e)), 1)
    
  def test_letrec_rules(self):
    f = Var('f')
    x = Var('x')
    eq_(eval(letrec([(f, rules([[1], 1],[[x],f(sub(x,1))]))], f(1))), 1) # passed
    eq_(eval(letrec([(f, rules([[1], 1],[[x],f(sub(x,1))]))], f(2))), 1)
    
    
class XTestLoop:
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
    
class XTest_letr:
  def testembedvar2(self):
    e, e2, f, g, h = Var('e'), Var('e2'), Var('f'), Var('g'), Var('h')
    eq_(eval(letrec([(f, macro([[cons(1, e2)], g(e2)])),
                     (g, function([[e], h(e)])),
                     (h, function([[1], True]))],
                f(e), e)), cons(1, 1))
    
class TestCut:
  #http://en.wikibooks.org/wiki/Prolog/Cuts_and_Negatio
  def testCut1(self):
    a, b, c, x = Var('a'), Var('b'), Var('c'), Var('x')
    Lx = LogicVar('x')
    eq_(eval(letrec([(a, rules([[x], begin(b(x), cut, c(x))])), 
                     (b, rules([[1], True],
                                 [[2], True],
                                 [[3], True])),
                     (c, rules([[1], True]))],
             a(Lx), Lx)), 1) 
    
  def test_cut2(self):
    a, b, c, x = Var('a'), Var('b'), Var('c'), Var('x')
    Lx = LogicVar('x')
    assert_raises(NoSolution, eval, letrec([(a, rules([[x], begin(b(x), cut, c(x))])),
                     (b, rules([[1], True],
                                    [[2], True],
                                    [[3], True])),
                     (c, rules([[2], True]))],
             a(Lx), Lx))
    
  def test_cut2_no_Cut_and_p(self):
    a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x'), 
    Lx = LogicVar('x')
    eq_(eval(letrec([(a, rules([[x], begin(b(x), c(x))],
                                 [[x], d(x)])),
                     (b, rules([[1], 'b1'],
                                 [[4], 'b4'])),
                     (c, rules([[4], 'c4'])),
                     (d, rules([[3], 'd3']))],
             a(Lx), Lx)), 4) 
    
  def test_cut2_no_Cut2_and_(self):
    # test_cut2_no_Cut_and_p work correct.
    # but this test and test_cut2_no_Cut3_begin work wrong because the bug below:
    # bug in Var.getvalue/Var.setvalue: 
    # dont't restore the longer chain of bindings after shorten it.
    a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x')
    Lx = LogicVar('x')
    eq_(eval(letrec([(a, rules([[x], begin(b(x),c(x))],
                                 [[x], d(x)])),
                     (b, rules([[1], True],
                                 [[4], True])),
                     (c, rules([[4], True])),
                     (d, rules([[3], True]))],
             a(Lx), Lx)), 4) 
    
  def test_cut2_no_Cut3_begin(self):
    a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x')
    Lx = LogicVar('x')
    eq_(eval(letrec([(a, rules([[x], begin(b(x),c(x))],
                                 [[x], d(x)])),
                     (b, rules([[1], 'b1'],
                                 [[4], 'b4'])),
                     (c, rules([[4], 'c4'])),
                     (d, rules([[3], 'd3']))],
             a(Lx), Lx)), 4) 
    
  def testCut4(self):
    a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x'), 
    Lx = LogicVar('x')
    assert_raises(NoSolution, eval, letrec([(a, rules([[x], begin(b(x), cut, c(x))],
                          [[x], d(x)])),
                     (b, rules([[1], 'b1'],
                                 [[4], 'b4'])),
                     (c, rules([[4], 'c4'])),
                     (d, rules([[3], 'd3']))],
             a(Lx), Lx))
    
  def testCut5(self):
    start, a, b, c, d, x = Var('start'), Var('a'), Var('b'), Var('c'), Var('d'), Var('x'), 
    Lx = LogicVar('x')
    eq_(eval(letrec([
      (start, rules([[x], a(x)],
                    [[x], d(x)])),      
      (a, rules([[x], begin(b(x), c(x))],
                [[x], d(x)])),
      (b, rules([[1], 'b1'],
                [[4], 'b4'])),
      (c, rules([[4], 'c4'])),
      (d, rules([[3], 'd3']))],
      start(Lx), Lx)), 4)
    
  def testCut6(self):
    start, a, b, c, d, x = Var('start'), Var('a'), Var('b'), Var('c'), Var('d'), Var('x'), 
    Lx = LogicVar('x')
    eq_(eval(letrec([
      (start, rules([[x], a(x)],
                    [[x], d(x)])),      
      (a, rules([[x], begin(b(x), cut, c(x))],
                [[x], d(x)])),
      (b, rules([[1], 'b1'],
                [[4], 'b4'])),
      (c, rules([[4], 'c4'])),
      (d, rules([[3], 'd3']))],
      start(Lx), Lx)), 3)
    
class TestMacro:
  def test1(self):
    eq_(eval(macro([[], prin(1)])()), None) 
    
  def test2(self):
    x = Var('x')
    eq_(eval(macro([[x], x])(prin(1))), None) 
    
  def test3(self):
    x = Var('x')
    eq_(eval(macro([[x], prin(x)])(add(1, 1))), None) 
    
  def test4(self):
    x = Var('x')
    y = Var('y')
    eq_(eval(let([(y, 1)], macro([[x], prin(x)])(add(y, 1)))), None) 
    
  def test_or_p(self):
    x, y = Var('x'), Var('y')
    eq_(eval(macro([[x, y], x],
                   [[x, y], y])(prin(1), prin(2))), None) 
    
  def test_findall_or_p(self):
    x, y = Var('x'), Var('y')
    eq_(eval(findall(macro([[x, y], x],
                   [[x, y], y])(prin(1), prin(2)))), None) 
    
  def test_closure1(self):
    x, f = Var('x'), Var('f')
    eq_(eval(let([(f, macro([[x], prin(x)])),
                  (x, 1)],
             f(add(x,x)))), None) 
    
  def test_closure2(self):
    x, f = Var('x'), Var('f')
    eq_(eval(let([(f, macro([[x], x])),
                  (x, 1)],
             f(add(x,x)))), 2)
    
  def test_closure3(self):
    x, y, f = Var('x'), Var('y'), Var('f')
    eq_(eval(let([(f, macro([[x, y], begin(x, y)])),
                  (x, 1)],
             f(prin(x), prin(add(x,x))))), None) 
    
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

