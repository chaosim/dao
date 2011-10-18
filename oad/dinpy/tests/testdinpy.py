# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad.dinpy.dinpy import *
from oad import *
from oad.builtins.io import prin
from oad.term import Var
from oad.dinpy.dexpr import _VarSymbol
from oad import special
from oad.builtins.rule import replace_def, remove, append_def, insert_def, \
     abolish, retractall, retract
from oad.dinpy.dinpy import AtForm, varcache
from oad.builtins import arith

##class TestVarDeclare:
##  def test1(self):
##    eq_(1,2)

a, b, c = var.a.b.c
a, b, c = preparse([a, b, c])
i, j = preparse([v.i, v.j])
n = preparse(v.n)
x, y = preparse([v.x, v.y])

class TestVarDeclare:
  def test1(self):
    ok_(isinstance(a, Var))    
    ok_(isinstance(b, Var))    
    ok_(isinstance(c, Var))    
    ok_(isinstance(i, Var)) 
    
class Test_v_var:
  def test_v(self):
    x = v.a
    eq_(x.__class__, _VarSymbol)
    eq_(preparse(x), varcache('a'))

  def test_var(self):
    x = var.a.b.c
    eq_(preparse(list(x)), 
        [varcache('a'),varcache('b'),varcache('c')])

class TestAssign:
  def test_assign1(self):
    eq_(preparse(v.i<<1), preparse(special.set(i, 1)))
  def test_assign2(self):
    eq_(preparse(put.i.j<<v.i+1), preparse(special.set_list([i,j], arith.add(i, 1))))
  def test_assign3(self):
    eq_(preparse(put.i.j<<(1,2)), preparse(special.set_list([i,j], (1,2))))
    
class TestLet:
  def test_let1(self):
    eq_(preparse(let(v.i << 1).do[1,2]), special.let([(i,1)], 1, 2))
  def test_eval_let1(self):
    eq_(eval(let(v.i << 1).do[v.i]), 1)
  def test_let2(self):
    let1 = let(v.a<<1).do[prin(1)]
    eq_(preparse(let1), special.let([(a,1)], prin(1)))
  def test_let3(self):
    let1 = let(v.a << v.b << 1).do[prin(1)]
    eq_(preparse(let1), special.let(((b,1), (a,b)), prin(1)))
  def test_let4(self):
    let1 = let( v.a/ v.b << (1,2)).do[prin(1)]
  def test_eval_let4(self):
    let1 = let( v.a/ v.b << (1,2)).do[v.a+v.b]
    eq_(eval(let1), 3)

class TestIff:
  def test_iff1(self):
    eq_(preparse(iff(v.i==1) [2]), special.iff([(arith.eq(i,1), 2)]))
  def test_iff2(self):
    eq_(preparse(iff(1) [2]
              .elsif(3) [4].
              els [5]), 
        special.iff([(1, 2),(3, 4)], 5))
  def test_eval_iff2(self):
    eq_(eval(iff(0) [1]
              .elsif(1) [2]
              .els [3]), 
        2)
    eq_(eval(iff(0) [1]
              .elsif(0) [2]
              .els [3]), 
        3)

class TestLoop:
  def test_loop(self):
    eq_(preparse(loop[prin(1)]), special.LoopForm([prin(1)])) 
  def test_eval_loop(self):
    i = v.i
    eq_(eval([i<<0, loop[prin(i), ++i, iff(i==3)[exit >>i]], i]), 3)
  def test_loop_times(self):
    eq_(preparse(loop(10)[prin(1)]), special.LoopTimesForm(10, [prin(1)], 'a')) 
  def test_eval_loop_times(self):
    eq_(eval(loop(3)[prin(1)]), None) 

class TestDo:
  def test_do_when(self):
    eq_(preparse(do[prin(1)].when(1)), special.LoopWhenForm([prin(1)], 1)) 
  def test_eval_do_when(self):
    eq_(eval([ v.i<<0, do[prin(v.i), ++v.i].when(v.i<3), v.i]), 3) 
  def test_do_when2(self):
    eq_(preparse(when(1).do[prin(1)]), special.WhenLoopForm([prin(1)], 1)) 
  def test_do_until(self):
    eq_(preparse(do[prin(1)].until(v.i==1)), 
        special.LoopUntilForm([prin(1)], arith.eq(i, 1))) 
  def test_eval_do_until(self):
    eq_(eval([ v.i<<0, do[prin(v.i), ++v.i].until(v.i==3), v.i]), 3) 
    
class TestCase:
  def test_Case1(self):
    x = preparse(v.x)
    eq_(preparse(case(x).of(1)[prin(1)].of(2,3)[prin(4)].els[prin(5)]), 
        special.CaseForm(x,{1:[prin(1)], 2:[prin(4)], 3:[prin(4)]}, [prin(5)])) 
  def test_eval_Case1(self):
    x = preparse(v.x)
    eq_(eval([v.x<<3, case(x).of(1)[prin(1)].of(2,3)[prin((2,3)), (2,3)].els[prin(5)]]), 
        (2,3)) 
  def test_eval_Case2(self):
    x = preparse(v.x)
    eq_(eval([v.x<<3, case(x).of(1)[prin(1), 1].of(2)[prin(2), 2].els[prin(3), 3]]), 
        3) 
  def test_eval_Case3(self):
    x = preparse(v.x)
    eq_(eval([v.x<<(1,2), case(x).of((1,2), (3,4))[prin(x), x].of(2,3)[prin((2,3)), (2,3)].els[prin(5)]]), 
        (1,2)) 
##  def test_Case2(self):
##    x = preparse(v.x)
##    eq_(preparse(case(x)/{1:[prin(1)],2:[prin(4)],3:[prin(4)], els:[prin(5)]}), 
##        special.CaseForm(x,{1:[prin(1)], 2:[prin(4)], 3:[prin(4)]}, [prin(5)]))
    
class TestEach:
  def test_slice(self):
    i = preparse(v.i); j = preparse(v.j)
    eq_(preparse(each(v.i,v.j)[1:3][1:3].do[prin(v.i)]), 
        special.EachForm((i,j), zip(range(1,3),range(1,3)),[prin(i)])) 
  def test_eval_slice(self):
    eq_(eval(each(v.i,v.j)[1:3][1:3].do[prin(v.i, v.j), (v.i, v.j)]), 
        None) 
##  def test_slice2(self):
##    i = v.i; j = v.j
##    eq_(preparse(each(i,j)['a':'z']['A':'Z'].do[prin(i)]), 
##        special.EachForm((i,j), zip(range(1,3),range(1,3)),[prin(i)])) 
  def test_getitem1(self):
    i = preparse(v.i); j = preparse(v.j)
    eq_(preparse(each(v.i,v.j)[zip(range(2), range(2))].do[prin(v.i, v.j)]), 
        special.EachForm((i,j), tuple(zip(range(2),range(2))),[prin(i,j)])) 
  def test_eval_getitem1(self):
    eq_(eval(each(v.i,v.j)[zip(range(2), range(2))].do[prin(v.i, v.j), (v.i, v.j)]), 
        None) 
  def test_getitem2(self):
    i = preparse(v.i); j = preparse(v.j)
    eq_(preparse(each(v.i,v.j)[range(2)][range(2)].do[prin(v.i, v.j)]), 
        special.EachForm((i,j), zip(range(2),range(2)),[prin(i,j)]))
  def test_eval_getitem2(self):
    eq_(eval(each(v.i,v.j)[range(2)][range(2)].do[prin(v.i, v.j), (v.i, v.j)]), 
        None) 
    
class TestExitNext:
  def test_exit1(self):
    eq_(preparse(exit.loop), special.exit(None, 'loop')) 
  def test_exit2(self):
    eq_(preparse(exit.loop/2>>v.i), special.exit(i, 'loop', 2)) 
  def test_next1(self):
    eq_(preparse(next.loop), special.next('loop')) 
  def test_next2(self):
    eq_(preparse(next.loop/2), special.next('loop', 2)) 
    
class TestBlockLabel:
  def test_label(self):
    eq_(preparse(label.a%loop[0]), special.LoopForm([0], 'a')) 
  def test_block(self):
    eq_(preparse(block.a[1]), special.block('a', 1)) 
  def test_block2(self):
    eq_(preparse(block.a[ v.i << 1 ]), special.block('a', special.set(i,1))) 

class TestFun:
  def test_at(self):
    at1 = at(i)[1](j)[2][3](x,y)[4]
    eq_(preparse(at1), 
        AtForm([((i,),[[1]]), ((j,),[[2],[3]]),((x,y),[[4]])])) 
  def test_at2(self):
    eq_(preparse(at[prin(1)]), AtForm([(None,[[prin(1)]])]))
  def test1(self):
    eq_(preparse(fun. a(x)== [prin(1)]), replace_def(a, (x,), [(prin(1),)]))
  def test_eval_a_x(self):
    eq_(eval([fun. a(x)== [prin(x), x], a(1)]), 1)
  def test2(self):
    eq_(preparse(fun. a(x)== at[prin(1)]), replace_def(a, (x,), [[prin(1)]]))
  def test_eval_a_x2(self):
    x = v.x
    eq_(eval([fun. a(x)== at[prin(x), x], a(1),
              fun. a(x)== at[prin(-x), -x], a(1)]), -1)
    eq_(eval([fun. a(x)== at[prin(x), x], a(1),
              fun. a(x, i)== at[prin(-x, i), -x], a(3), a(1, 2)]), -1)
  def test3(self):
    eq_(preparse(fun. a(x)>= [prin(1)]), 
        append_def(a, (x,), [(prin(1),)], special.UserFunction))
  def test4(self):
    eq_(preparse(fun. a(x)>= at[prin(1)]), 
        append_def(a, (x,), [[prin(1)]], special.UserFunction))
  def test41(self):
    eq_(preparse(fun. a(x)>= at[prin(1)][prin(2)]), 
        append_def(a, (x,), [[prin(1)],[prin(2)]], special.UserFunction))
  def test42(self):
    eq_(preparse(fun. a(x)<= at[prin(1)]),
        insert_def(a, (x,), [[prin(1)]], special.UserFunction))
  def test5(self):
    eq_(preparse(fun. a== at()[prin(1)]), 
        special.set(a, special.FunctionForm(((), prin(1)))))
  def test6(self):
    eq_(preparse(fun. a>= at()[prin(1)]), 
        special.begin(append_def(a, (), [[prin(1)]], special.UserFunction)))
  def test61(self):
    eq_(preparse(fun. a<= at()[prin(1)]), 
        special.begin(insert_def(a, (), [[prin(1)]], special.UserFunction)))
  def test7(self):
    eq_(preparse(-fun. a/3),abolish(a,3))
  def test8(self):
    eq_(preparse(-fun. a(x)), remove(a,(x,), special.UserFunction))
  def test9(self):
    eq_(preparse(fun()[prin(1)]), special.FunctionForm(((), [prin(1)])))
    
class TestMacro:
  def test5(self):
    eq_(preparse(fun. a== at()[prin(1)]), 
        special.set(a, special.MacroForm(((), prin(1)))))
    

