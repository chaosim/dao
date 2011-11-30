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

from dao.compiler.typer import eval
from dao.compiler.type import *
from dao.compiler import type

class TestSimple:
  def testInteger(self):
    eq_(eval(1), int)    
  def testString(self):
    eq_(eval("1"), str)
  def testArithmetic(self):
    eq_(eval(add(1, 2)), int) 
  def testquote(self):
    eq_(eval(quote(x)), type.var)
  def testset(self):
    eq_(eval(set(a,2)), int)
  def testdefine(self):
    eq_(eval(begin(define(x,1),define(x,2))), int)
    
class TestControl:
  def testbegin(self):
    eq_(eval(begin(1, 2)), int)
  def testif_(self):
    eq_(eval(if_(0, 1, 2)), int)
  def testif_add_sub(self):
    eq_(eval(if_(0, add, sub)), BuiltinFunctionType((int,), int))
  def testif_add_sub(self):
    eq_(eval(if_(0, add, sub)(1, 1)), int)
  def testiff(self):
    eq_(eval(iff(((0, prin(1)), (1, prin(2))))), NoneType)
  def testeval1(self):
    eq_(eval(eval_(quote(1))), int)
    eq_(eval(eval_(quote(add(1, 1)))), int)
  def testeval2(self):
    eq_(eval(let([(x,1)], eval_(quote(x)))), int)

class TestFunction:
  def test_let_set(self):
    eq_(eval(let([(a,1)], set(a,2), a)), int)
  def testLambda1(self):
    eq_(eval(lambda_([x], 1)(2)), 1)
  def testLambda2(self):
    eq_(eval(lambda_([x], 1)), UserFunctionType((unknown_type, ), int))
  def testLambda2(self):
    eq_(eval(lambda_((x, y), add(x, y))), UserFunctionType((unknown_type, unknown_type), unknown_type))
  def test1(self):
    eq_(eval(function([[1], 1],[[x],x])), UserFunctionType(((unknown_type,), unknown_type))) 
    eq_(eval(function([[1], 1],[['a'],'a'])), UserFunctionType(((int,), int), ((str,), str)))
    
class Test_letr:
  def testembedvar1(self):
    e, e2, f, g, h = Var('e'), Var('e2'), Var('f'), Var('g'), Var('h')
    eq_(eval(letr([(f, function([[1], 1]))],
                f)), UserFunctionType(((int,), int)))
  def testembedvar2(self):
    e, e2, f, g, h = Var('e'), Var('e2'), Var('f'), Var('g'), Var('h')
    eq_(eval(letr([(f, macro([[cons(1, e2)], g(e2)])),
                     (g, function([[e], h(e)])),
                     (h, function([[1], True]))],
                f)), UserFunctionType((cons(1, unknown_type), ValueType(True))))
  def testletr(self):
    eq_(eval(letr([(f, function([[1], 1],[[x],f(x-1)]))], f)), UserFunctionType(((unknown_type,), unknown_type)))
    eq_(eval(letr([(f, function([[1], 1],[[x],f(x-1)]))], f)), UserFunctionType(((unknown_type,), unknown_type))) 
  def testletrfac(self):
    eq_(eval(letr([(fac, lambda_([n], if_(eq(n,1), 1, mul(n, fac(sub(n, 1))))))],
                  fac)), UserFunctionType(((unknown_type,), unknown_type)))

class TestCut:
  #http://en.wikibooks.org/wiki/Prolog/Cuts_and_Negatio
  def testCut1(self):
    a, b, c, x = Var('a'), Var('b'), Var('c'), Var('x'), 
    eq_(eval(letr([(a, function([[x], b(x)&cut&c(x)])), #[cut] = cut = cut() 
                     (b, function([[1], True],
                                 [[2], True],
                                 [[3], True])),
                     (c, function([[1], True]))],
             a(x), x)), UserFunctionType(((unknown_type,), ValueType(True)))) 
  def test_cut2(self):
    a, b, c, x = Var('a'), Var('b'), Var('c'), Var('x'), 
    eval(letr([(a, function([[x], b(x)&cut&c(x)])),
                     (b, function([[1], True],
                                    [[2], True],
                                    [[3], True])),
                     (c, function([[2], True]))],
             a), UserFunctionType(((unknown_type,), ValueType(True))))
  #def test_cut2_no_Cut_and_p(self):
    #a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x'), 
    #eq_(eval(letr([(a, function([[x], b(x)&c(x)],
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
    #eq_(eval(letr([(a, function([[x], and_(b(x),c(x))],
                                 #[[x], d(x)])),
                     #(b, function([[1], True],
                                 #[[4], True])),
                     #(c, function([[4], True])),
                     #(d, function([[3], True]))],
             #a(x), x)), 4) 
  #def test_cut2_no_Cut3_begin(self):
    #a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x')
    #eq_(eval(letr([(a, function([[x], begin(b(x),c(x))],
                                 #[[x], d(x)])),
                     #(b, function([[1], 'b1'],
                                 #[[4], 'b4'])),
                     #(c, function([[4], 'c4'])),
                     #(d, function([[3], 'd3']))],
             #a(x), x)), 4) 
  #def testCut4(self):
    #a, b, c, d, x = Var('a'), Var('b'), Var('c'), Var('d'), Var('x'), 
    #eq_(eval(letr([(a, function([[x], b(x)&cut&c(x)],
                          #[[x], d(x)])),
                     #(b, function([[1], 'b1'],
                                 #[[4], 'b4'])),
                     #(c, function([[4], 'c4'])),
                     #(d, function([[3], 'd3']))],
             #a(x), x)), 3)
    
class TestMacro:
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
    
class TestCallccBlockCatch:
  def testblock(self):
    f = Var('f')
    eq_(eval(block('foo', let([(f, lambda_((), exit_block('foo',1)))], 
                            mul(2,block('foo', f()))))), 
        int)
  def testblock2(self):
    eq_(eval(block('a', exit_block('a', 2), 3)), int)
  def testcatch1(self):
    eq_(eval(catch(1, 2)), int)
  def testcatch2(self):
    eq_(eval(catch(1, throw(1, 2), 3)), int)
  def test_unwind_protect(self):
    eq_(eval(block('foo', unwind_protect(exit_block('foo', 1), prin(2)))), int)
  def test_unwind_protect2(self):
    eq_(eval(block('foo', unwind_protect(exit_block('foo', 1), 
                            prin(2), prin(3)))), int)
  def testcallcc1(self):
    from dao.solve import done
    eq_(eval(callcc(lambda_([k], k(2)))), int)
  def testcallcc2(self):
    from dao.solve import done
    assert_raises(DaoError, eval, callcc(lambda_([k], k(2,3))))
  def testcallcc3(self):
    eq_(eval(callcc(callcc)), ContinuationFunction(done))
  #def testcallcc4(self):
    ##assert 0, '((call/cc call/cc) (call/cc call/cc)) infinite loop.'

#class TestModule:
  #def testbindings(self):
    #a = Var('a')
    #m1 = eval(module(define(a,1)))
    #ok_(a in m1.bindings)    
    
  #def testfrom(self):
    #a, m = Var('a'), Var('m')
    #eq_(eval(let([(m,module(define(a,1)))], from_(m,a))), 1)
    
  #def test_embeded_module(self):
    #a, m1, m2 = Var('a'), Var('m1'), Var('m2')
    #eq_(eval(let([(m1,module(define(a,1),define(m2, module(define(a,2)))))], 
               #from_(from_(m1,m2),a))), 2) 

  #def test_let_in_module(self):
    #a, m1, m2 = Var('a'), Var('m1'), Var('m2')
    #eq_(eval(let([(m1,module(define(a,1), let([(a,2)], define(a,3))))], 
               #from_(m1,a))), 1) 

