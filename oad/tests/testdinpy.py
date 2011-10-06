# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad import *
from oad.builtins.format import write
from oad.term import Var
from oad.dinpy import *
from oad import special
from oad.builtins.rule import replace, remove, assert_, asserta, \
     abolish, retractall, retract
from oad.dinpy import AtForm, varcache

a, b, c = var.a.b.c
i, j = v.i, v.j
n = v.n
x, y = v.x, v.y

class TestVarDeclare:
  def test1(self):
    ok_(isinstance(a, Var))    
    ok_(isinstance(b, Var))    
    ok_(isinstance(c, Var))    
    ok_(isinstance(i, Var)) 
    
class Test_v_var:
  def test_v(self):
    x = v.a
    eq_(x.__class__, Var)
    eq_(x, varcache('a'))

  def test_var(self):
    x = var.a.b.c
    eq_(list(x), [varcache('a'),varcache('b'),varcache('c')])

class TestAssign:
  def test_assign1(self):
    eq_(parse(put.i==1), special.set(i, 1))
  def test_assign2(self):
    eq_(parse(put.i.j==(1,2)), special.set_list([i,j], (1,2)))
##  def test_assign3(self):
##    put1 = put[i, my.j]==(100, 200)
##    eq_(put1, MultipleAssign([('any_scope', i), ('local',j)], (100, 200)))
    
class TestLet:
  def test_let1(self):
    eq_(parse(let({i:1}).do[1,2]), special.let({i:1}, 1, 2))
  def test_let2(self):
    let1 = let({a:1}).do[write(1)]
    eq_(parse(let1), special.let({a:1}, write(1)))

class TestIff:
  def test_iff1(self):
    eq_(parse(iff(1).then[2]), special.iff([(1, 2)]))
  def test_iff2(self):
    eq_(parse(iff(1).then[2]
              .elsif(3).then[4].
              els[5]), 
        special.iff([(1, 2),(3, 4)], 5))

class TestDo:
  def test_do_when(self):
    eq_(parse(do[write(1)].when(1)), special.LoopWhenForm([write(1)], 1)) 
  def test_do_when2(self):
    eq_(parse(when(1).do[write(1)]), special.LoopWhenForm([write(1)], 1)) 
  def test_do_until(self):
    eq_(parse(do[write(1)].until(1)), special.LoopUntilForm([write(1)], 1)) 
    
class TestCase:
  def test_Case1(self):
    x = v.x
    eq_(parse(case(x).of(1)[write(1)].of(2,3)[write(4)].els[write(5)]), 
        special.CaseForm(x,{1:[write(1)], 2:[write(4)], 3:[write(4)]}, [write(5)])) 
  def test_Case2(self):
    x = v.x
    eq_(parse(case(x)/{1:[write(1)],2:[write(4)],3:[write(4)], els:[write(5)]}), 
        special.CaseForm(x,{1:[write(1)], 2:[write(4)], 3:[write(4)]}, [write(5)]))
    
class TestEach:
  def test_slice(self):
    i = v.i; j = v.j
    eq_(parse(each(i,j)[1:3][1:3].do[write(i)]), 
        special.EachForm((i,j), zip(range(1,3),range(1,3)),[write(i)])) 
##  def test_slice2(self):
##    i = v.i; j = v.j
##    eq_(parse(each(i,j)['a':'z']['A':'Z'].do[write(i)]), 
##        special.EachForm((i,j), zip(range(1,3),range(1,3)),[write(i)])) 
  def test_getitem1(self):
    i = v.i; j = v.j
    eq_(parse(each(i,j)[zip(range(2), range(2))].do[write(i,j)]), 
        special.EachForm((i,j), tuple(zip(range(2),range(2))),[write(i,j)])) 
  def test_getitem2(self):
    i = v.i; j = v.j
    eq_(parse(each(i,j)[range(2)][range(2)].do[write(i,j)]), 
        special.EachForm((i,j), zip(range(2),range(2)),[write(i,j)])) 

class TestFun:
  def test_at(self):
    eq_(parse(at(i)[1](j)[2][3](x,y)[4]), 
        AtForm([((i,),[[1]]), ((j,),[[2],[3]]),((x,y),[[4]])])) 
  def test_at2(self):
    eq_(parse(at[write(1)]), AtForm([(None,[[write(1)]])]))
  def test1(self):
    eq_(parse(fun. a(x)== [write(1)]), replace(v.a, (x,), write(1)))
  def test2(self):
    eq_(parse(fun. a(x)== at[write(1)]), replace(v.a, (x,), write(1)))
  def test3(self):
    eq_(parse(fun. a(x)>= [write(1)]), assert_(v.a, (x,), write(1)))
  def test4(self):
    eq_(parse(fun. a(x)>= at[write(1)]), assert_(v.a, (x,), write(1)))
  def test41(self):
    eq_(parse(fun. a(x)>= at[write(1)][write(2)]), 
        special.begin(assert_(v.a, (x,), [write(1)]), assert_(v.a, (x,), [write(2)])))
  def test42(self):
    eq_(parse(fun. a(x)<= at[write(1)]), asserta(v.a, (x,), write(1)))
  def test5(self):
    eq_(parse(fun. a== at()[write(1)]), 
        special.set(v.a, special.FunctionForm([((), [write(1)])])))
  def test6(self):
    eq_(parse(fun. a>= at()[write(1)]), 
        special.begin(assert_(v.a, (), [write(1)])))
  def test6(self):
    eq_(parse(fun. a<= at()[write(1)]), 
        special.begin(asserta(v.a, (), [write(1)])))
  def test7(self):
    eq_(parse(-fun. a/3),abolish(a,3))
  def test8(self):
    eq_(parse(-fun. a(x)), retractall(a,(x,)))
class TestMacro:
  def test1(self):
    eq_(parse(fun. a== at()[write(1)]), 
        special.set(v.a, special.MacroForm([((), [write(1)])])))
    

