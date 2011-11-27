# -*- coding: utf-8 -*-

pyset = set

from nose.tools import eq_, ok_, assert_raises

from dao.term import cons
from dao.solve import to_sexpression
from dao.builtins.arith import add, sub
from dao.builtins.term import define
from dao.special import *

# compiler
from dao.compiler.compile import compile_to_cont, make_compiler
from dao.compiler import vop
from dao.compiler.cont import *

from dao.util import *

class TestSimple:
  def testInteger(self):
    eq_(compile_to_cont(1), V(1, done))    
  def testVar(self):
    eq_(compile_to_cont(x), V(vop.GetValue(x), done)) 
  def testquote(self):
    eq_(compile_to_cont(quote(x)), V(x, done))
  def testset(self):
    eq_(compile_to_cont(set(a,2)), V(2, Set(a, done)))
  def testdefine1(self):
    compiler = make_compiler()
    compiler.parse_compile_to_cont(define(x,1))
    eq_(len(compiler.cont_set), 4)
  def testdefine2(self):
    compiler = make_compiler()
    compiler.parse_compile_to_cont(begin(define(x,1),define(x,2)))
    cont_list = list(compiler.cont_set)
    eq_(len(cont_list), 6)
    ok_(not(cont_list[0].depend_on(cont_list[1]))) # two ApplyCont
    
    
class TestControl:
  def testBegin(self):
    eq_(compile_to_cont(begin(1, 2)), V(1, V(2, done)))    
  def testif_(self):
    eq_(compile_to_cont(if_(0, 1, 2)), V(0, If(V(1, done), V(2, done))))
  
  def testblock1(self):
    eq_(compile_to_cont(block('a', exit_block('a', 2))), V(2, done))
    
  def testblock2(self):
    compiler = make_compiler()
    result = compiler.parse_compile_to_cont(block('a', exit_block('a', 2), 3))
    #print result
    eq_(result, V(2, done))
  def testblock3(self):
    compiler = make_compiler()
    result = compiler.parse_compile_to_cont(block('a', if_(1, exit_block('a', 2)), 3))
    eq_(result, V(1, If(V(2, done), V(3, done))))
    
  def testblock4(self):
    compiler = make_compiler()
    result = compiler.parse_compile_to_cont(block('a', if_(1, continue_block('a')), 3))
    expect = V(1, If(Blk(result), V(3, done)))
    eq_(result, expect)
    
  def testcatch1(self):
    compiler = make_compiler()
    result = compiler.parse_compile_to_cont(catch(1, 2))
    expect = V(1, Catch(V(2, Lbl(done))))
    eq_(result, expect)
    
    
  def testcatch2(self):
    compiler = make_compiler()
    result = compiler.parse_compile_to_cont(catch(1, throw(1, 2), 3))
    #expect = V(1, Catch(V(1, Throw()), Lbl(done)))
    eq_(result, expect)

  #def testif_add_sub(self):
    #eq_(compile_to_cont(if_(0, add, sub)(1, 1)), 0)
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

class TestBuiltin:
  def testArithmetic(self):
    compiler = make_compiler()
    appc = App(add, done)
    argc2 = Arg()
    argc1 = Arg()
    gc2 = Gat(argc2, appc)
    gc1 = Gat(argc1, gc2)
    vc0 = V((), gc1)
    vc2 = V(2, argc2)
    vc1 = V(1, argc1)
    cont_set = pyset([vc1, vc2, vc0, gc1, gc2, argc1, argc2, appc, done, fail_done])
    compiler.parse_compile_to_cont(add(1,2))
    eq_(len(compiler.cont_set), len(cont_set))
    eq_(compiler.cont_set, cont_set)
    
  def testArithmetic2(self):
    compiler = make_compiler()
    appc2 = App(sub, done)
    argc4 = Arg()
    argc3 = Arg()
    gc4 = Gat(argc4, appc2)
    gc3 = Gat(argc3, gc4)
    vc5 = V((), gc3)
    appc1 = App(add, argc4)
    vc1 = V(1, argc3)
    argc2 = Arg()
    argc1 = Arg()
    gc2 = Gat(argc2, appc1)
    gc1 = Gat(argc1, gc2)
    vc4 = V((), gc1)
    vc3 = V(3, argc2)
    vc2 = V(2, argc1)
    
    cont_set = pyset([vc1, vc2, vc3, vc4, vc5, gc1, gc2, gc3, gc4, 
                      argc1, argc2, argc3, argc4, appc1, appc2, done, fail_done])
    compiler.parse_compile_to_cont(sub(1, add(2,3)))
    eq_(len(compiler.cont_set), len(cont_set))
    eq_(compiler.cont_set, cont_set)
    

class TestFunction:
  def test_function0(self):
    compiler = make_compiler()
    compiler.parse_compile_to_cont(function(((), 1))(1))
    cont_list = list(compiler.cont_set)
    eq_(len(cont_list), 13)
    ok_(not(cont_list[0].depend_on(cont_list[1]))) # two ApplyCont
    
  #def testLambda(self):
    #eq_(eval(lambda_([x], 1)(2)), 1)
    #eq_(eval(lambda_([x], x)(2)), 2)
    #eq_(eval(lambda_((x, y), add(x, y))(1, 3)), 4)
    
  #def test_let_set(self):
    #eq_(eval(let([(a,1)], set(a,2), a)), 2)
    #eq_(eval(let([(a,1)], 
                  #let([(b,1)], set(a,2), a))), 2)
