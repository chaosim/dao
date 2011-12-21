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
    eq_(compile_to_cont(1), done(1))    
  def testVar(self):
    eq_(compile_to_cont(x), done(x)) 
  def testquote(self):
    eq_(compile_to_cont(quote(x)), done(x))
  def testset(self):
    eq_(compile_to_cont(set(a,2)), lambda_((x,), done(set(a,x)))(2))
    
    
class TestControl:
  def testBegin(self):
    eq_(compile_to_cont(begin(1, 2)), done(2)(1))    
  def testif_(self):
    eq_(compile_to_cont(if_(0, 1, 2)), lambda_((x,), if_(x, done(1),done(2)))(0))
  
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
    
  def testUnify(self):
    compiler = make_compiler()
    compiler.parse_compile_to_cont(unify(x,1))
    eq_(len(compiler.cont_set), len(cont_set))
    eq_(compiler.cont_set, cont_set)

class TestLambda:
  def testLambda(self):
    compiler = make_compiler()
    eq_(make_compiler().cont(lambda_([x], 1), done), done(lambda_([k, x], k(1))))
    
class TestLet:
  def testlet(self):
    compiler = make_compiler()
    eq_(compiler.cont(let([(x,1)], x), done),lambda_((x,),done(x))(1))

  def testlet2(self):
    compiler = make_compiler()
    eq_(compiler.cont(let([(x,1),(y,2)], x, y), done),
        lambda_((x,),lambda_((y,),done(y)(x))(2))(1))

  def testlet3(self):
    compiler = make_compiler()
    eq_(compiler.cont(let([(x, 1)], let([(x,2)], x), x), done),
        lambda_((x,),lambda_((x,),done(x)(x))(2))(1))

class TestFunction:
  def test_function1(self):
    compiler = make_compiler()
    result = compiler.cont(function(((), 1)), done)
    expect = done(lambda_(k),k(1))
    
  def test_function2(self):
    compiler = make_compiler()
    eq_(compiler.cont(function(((1,), 1), ((x,), x)), done),
        lambda_((k, x),done(x))(1))
