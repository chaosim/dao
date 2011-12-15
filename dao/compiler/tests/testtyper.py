# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from dao.term import cons
from dao.solve import tag_loop_label, NoSolutionFound, done, DaoError
from dao.special import quote, set, begin, if_, iff, lambda_, let, letr, eval_
from dao.special import function, macro
from dao.special import block, exit_block, continue_block, catch, throw
from dao.special import unwind_protect, module, from_, CaseForm
from dao.special import LoopTimesForm, LoopUntilForm, LoopWhenForm, EachForm

from dao.builtins.control import and_p, cut, callcc, ContinuationFunction
from dao.builtins.io import prin
from dao.builtins.arith import gt, eq, sub, mul, add, div
from dao.builtins.term import define
from dao.builtins.container import first

from dao.util import *
from dao.solve import to_sexpression
from dao.builtins.terminal import eoi, Eoi, tabspaces0, tabspaces, _Tabspaces0, _Tabspaces

from dao.solve import set_run_mode, noninteractive
set_run_mode(noninteractive)

from dao.compiler.compile import eval_type
from dao.compiler import type

class TestSimple:
  def testInteger(self):
    eq_(eval_type(1), type.atom)    
  def testString(self):
    eq_(eval_type("1"), type.atom)
  def testArithmetic(self):
    eq_(eval_type(add(1, 2)), type.atom) 
  def testquote(self):
    eq_(eval_type(quote(x)), type.var)
  def testset(self):
    eq_(eval_type(set(a,2)), type.atom)
  def testdefine(self):
    eq_(eval_type(begin(define(x,1),define(x,2))), type.atom)
    
class TestControl:
  def testbegin(self):
    eq_(eval_type(begin(1, 2)), type.atom)
  def testif_(self):
    eq_(eval_type(if_(0, 1, 2)), type.atom)
  def testif_add_sub(self):
    eq_(eval_type(if_(0, add, sub)), BuiltinFunctionType((type.atom,), type.atom))
  def testif_add_sub(self):
    eq_(eval_type(if_(0, add, sub)(1, 1)), type.atom)
  def xxxtestiff(self):
    eq_(eval_type(iff(((0, prin(1)), (1, prin(2))))), NoneType)
  def testeval1(self):
    eq_(eval_type(eval_(quote(1))), type.atom)
    eq_(eval_type(eval_(quote(add(1, 1)))), type.atom)
  def testeval2(self):
    eq_(eval_type(let([(x,1)], eval_(quote(x)))), type.atom)

class TestFunction:
  def test_let_set(self):
    eq_(eval_type(let([(a,1)], set(a,2), a)), type.atom)
  def testLambda1(self):
    eq_(eval_type(lambda_([x], 1)(2)), type.atom)
  def testLambda2(self):
    eq_(eval_type(lambda_([x], 1)), type.UserFunction((unknown_type, ), type.atom))
  def testLambda2(self):
    eq_(eval_type(lambda_((x, y), add(x, y))), type.UserFunction(type.atom))
  def test1(self):
    eq_(eval_type(function([[1], 1],[[x],x])), type.UserFunction(type.root)) 
    #eq_(eval_type(function([[1], 1],[['a'],'a'])), UserFunction(type.atom))
    
class Test_letr:
  def testembedvar1(self):
    e, e2, f, g, h = Var('e'), Var('e2'), Var('f'), Var('g'), Var('h')
    eq_(eval_type(letr([(f, function([[1], 1]))],
                f)), type.UserFunction(type.root))
  def testembedvar2(self):
    e, e2, f, g, h = Var('e'), Var('e2'), Var('f'), Var('g'), Var('h')
    eq_(eval_type(letr([(f, macro([[cons(1, e2)], g(e2)])),
                     (g, function([[e], h(e)])),
                     (h, function([[1], True]))],
                f)), type.UserFunction(type.atom))
  def testletr(self):
    eq_(eval_type(letr([(f, function([[1], 1],[[x],f(x-1)]))], f)), type.UserFunction(type.atom))
    eq_(eval_type(letr([(f, function([[1], 1],[[x],f(x-1)]))], f)), type.UserFunction(type.atom)) 
  def testletrfac(self):
    eq_(eval_type(letr([(fac, lambda_([n], if_(eq(n,1), 1, mul(n, fac(sub(n, 1))))))],
                  fac)), type.UserFunction(type.atom))

class TestCut:
  #http://en.wikibooks.org/wiki/Prolog/Cuts_and_Negatio
  def testCut1(self):
    a, b, c, x = Var('a'), Var('b'), Var('c'), Var('x'), 
    eq_(eval_type(letr([(a, function([[x], b(x)&cut&c(x)])), #[cut] = cut = cut() 
                     (b, function([[1], True],
                                 [[2], True],
                                 [[3], True])),
                     (c, function([[1], True]))],
             a(x), x)), type.atom) 
  def test_cut2(self):
    a, b, c, x = Var('a'), Var('b'), Var('c'), Var('x'), 
    eq_(eval_type(letr([(a, function([[x], b(x)&cut&c(x)])),
                     (b, function([[1], True],
                                    [[2], True],
                                    [[3], True])),
                     (c, function([[2], True]))],
             a)), type.UserFunction(type.atom))
  #def test_cut2_no_Cut_and_p(self):
    #a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x'), 
    #eq_(eval_type(letr([(a, function([[x], b(x)&c(x)],
                                 #[[x], d(x)])),
                     #(b, function([[1], 'b1'],
                                 #[[4], 'b4'])),
                     #(c, function([[4], 'c4'])),
                     #(d, function([[3], 'd3']))],
             #a(x), x)), 4) 
  #def test_cut2_no_Cut2_and_(self):
    ## test_cut2_no_Cut_and_p work correct.
    ## but this test and test_cut2_no_Cut3_begin work wrong because the bug below:
    ## bug in Var.getvalue/Var.setvalue: 
    ## dont't restore the longer chain of bindings after shorten it.
    #a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x')
    #from dao.builtins.arith import and_
    #eq_(eval_type(letr([(a, function([[x], and_(b(x),c(x))],
                                 #[[x], d(x)])),
                     #(b, function([[1], True],
                                 #[[4], True])),
                     #(c, function([[4], True])),
                     #(d, function([[3], True]))],
             #a(x), x)), 4) 
  #def test_cut2_no_Cut3_begin(self):
    #a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x')
    #eq_(eval_type(letr([(a, function([[x], begin(b(x),c(x))],
                                 #[[x], d(x)])),
                     #(b, function([[1], 'b1'],
                                 #[[4], 'b4'])),
                     #(c, function([[4], 'c4'])),
                     #(d, function([[3], 'd3']))],
             #a(x), x)), 4) 
  #def testCut4(self):
    #a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x'), 
    #eq_(eval_type(letr([(a, function([[x], b(x)&cut&c(x)],
                          #[[x], d(x)])),
                     #(b, function([[1], 'b1'],
                                 #[[4], 'b4'])),
                     #(c, function([[4], 'c4'])),
                     #(d, function([[3], 'd3']))],
             #a(x), x)), 3)
    
class TestMacro:
  def test1(self):
    eq_(eval_type(macro([[], prin(1)])()), None) 
    
  def test_eval(self):
    eq_(eval_type(macro([[x, y], eval_(x)],
                   [[x, y],eval_(y)])(prin(1), prin(2))), None) 
  def test_or_p(self):
    eq_(eval_type(macro([[x, y], x],
                   [[x, y],y])(prin(1), prin(2))), None) 
  def test_closure1(self):
    eq_(eval_type(let([(f, macro([[x], prin(eval_(x))])),
                  (x, 1)],
             f(x+x))), None) 
  def test_closure2(self):
    eq_(eval_type(let([(f, macro([[x], prin(x)])),
                  (x, 1)],
             f(x+x))), None) 
  def test_closure3(self):
    eq_(eval_type(let([(f, function([[x], prin(x)])),
                  (x, 1)],
             f(x+x))), None) 
  def test_closure4(self):
    eq_(eval_type(let([(f, macro([[x], x])),
                  (x, 1)],
             f(x+x))), 2) 
  def test4(self):
    eq_(eval_type(macro([[x], x])(prin(1))), None) 
    
class TestCallccBlockCatch:
  def testblock(self):
    f = Var('f')
    eq_(eval_type(block('foo', let([(f, lambda_((), exit_block('foo',1)))], 
                            mul(2,block('foo', f()))))), 
        type.atom)
  def testblock2(self):
    eq_(eval_type(block('a', exit_block('a', 2), 3)), type.atom)
  def testcatch1(self):
    eq_(eval_type(catch(1, 2)), type.atom)
  def testcatch2(self):
    eq_(eval_type(catch(1, throw(1, 2), 3)), type.atom)
  def test_unwind_protect(self):
    eq_(eval_type(block('foo', unwind_protect(exit_block('foo', 1), prin(2)))), type.atom)
  def test_unwind_protect2(self):
    eq_(eval_type(block('foo', unwind_protect(exit_block('foo', 1), 
                            prin(2), prin(3)))), type.atom)
  def testcallcc1(self):
    from dao.solve import done
    eq_(eval_type(callcc(lambda_([k], k(2)))), type.atom)
  def testcallcc2(self):
    from dao.solve import done
    assert_raises(DaoError, eval_type, callcc(lambda_([k], k(2,3))))
  def testcallcc3(self):
    eq_(eval_type(callcc(callcc)), ContinuationFunction(done))
  #def testcallcc4(self):
    ##assert 0, '((call/cc call/cc) (call/cc call/cc)) infinite loop.'

#class TestModule:
  #def testbindings(self):
    #a = Var('a')
    #m1 = eval_type(module(define(a,1)))
    #ok_(a in m1.bindings)    
    
  #def testfrom(self):
    #a, m = Var('a'), Var('m')
    #eq_(eval_type(let([(m,module(define(a,1)))], from_(m,a))), 1)
    
  #def test_embeded_module(self):
    #a, m1, m2 = Var('a'), Var('m1'), Var('m2')
    #eq_(eval_type(let([(m1,module(define(a,1),define(m2, module(define(a,2)))))], 
               #from_(from_(m1,m2),a))), 2) 

  #def test_let_in_module(self):
    #a, m1, m2 = Var('a'), Var('m1'), Var('m2')
    #eq_(eval_type(let([(m1,module(define(a,1), let([(a,2)], define(a,3))))], 
               #from_(m1,a))), 1) 

