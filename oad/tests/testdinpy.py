# -*- coding: utf-8 -*-

from nose.tools import eq_, ok_, assert_raises

from oad import *
from oad.builtins.format import write
from oad.term import Var
from oad.dexpr import VarSymbol
from oad.dinpy import *
from oad import special
from oad.builtins.rule import replace_def, remove, append_def, insert_def, \
     abolish, retractall, retract
from oad.dinpy import AtForm, varcache
from oad.builtins import arith

a, b, c = var.a.b.c
a, b, c = parse([a, b, c])
i, j = parse([v.i, v.j])
n = parse(v.n)
x, y = parse([v.x, v.y])

class TestVarDeclare:
  def test1(self):
    ok_(isinstance(a, Var))    
    ok_(isinstance(b, Var))    
    ok_(isinstance(c, Var))    
    ok_(isinstance(i, Var)) 
    
class Test_v_var:
  def test_v(self):
    x = v.a
    eq_(x.__class__, VarSymbol)
    eq_(parse(x), varcache('a'))

  def test_var(self):
    x = var.a.b.c
    eq_(parse(list(x)), 
        [varcache('a'),varcache('b'),varcache('c')])

class TestAssign:
  def test_assign1(self):
##    eq_(parse(put.i<<1), parse(special.set(i, 1)))
    eq_(parse(v.i<<1), parse(special.set(i, 1)))
  def test_assign2(self):
    eq_(parse(put.i.j<<v.i+1), parse(special.set_list([i,j], arith.add(i, 1))))
  def test_assign3(self):
    eq_(parse(put.i.j<<(1,2)), parse(special.set_list([i,j], (1,2))))
##  def test_assign3(self):
##    put1 = put[i, my.j]==(100, 200)
##    eq_(put1, MultipleAssign([('any_scope', i), ('local',j)], (100, 200)))
    
class TestLet:
  def test_let1(self):
    eq_(parse(let(v.i==1).do[1,2]), special.let({i:1}, 1, 2))
  def test_let2(self):
    let1 = let(v.a==1).do[write(1)]
    eq_(parse(let1), special.let({a:1}, write(1)))

class TestIff:
  def test_iff1(self):
    eq_(parse(iff(v.i==1) [2]), special.iff([(arith.eq(i,1), 2)]))
  def test_iff2(self):
    eq_(parse(iff(1) [2]
              .elsif(3) [4].
              els [5]), 
        special.iff([(1, 2),(3, 4)], 5))

class TestLoop:
  def test_loop(self):
    eq_(parse(loop[write(1)]), special.LoopForm([write(1)])) 
  def test_loop_times(self):
    eq_(parse(loop(10)[write(1)]), special.LoopTimesForm(10, [write(1)], 'a')) 

class TestDo:
  def test_do_when(self):
    eq_(parse(do[write(1)].when(1)), special.LoopWhenForm([write(1)], 1)) 
  def test_do_when2(self):
    eq_(parse(when(1).do[write(1)]), special.WhenLoopForm([write(1)], 1)) 
  def test_do_until(self):
    eq_(parse(do[write(1)].until(v.i==1)), 
        special.LoopUntilForm([write(1)], arith.eq(i, 1))) 
    
class TestCase:
  def test_Case1(self):
    x = parse(v.x)
    eq_(parse(case(x).of(1)[write(1)].of(2,3)[write(4)].els[write(5)]), 
        special.CaseForm(x,{1:[write(1)], 2:[write(4)], 3:[write(4)]}, [write(5)])) 
  def test_Case2(self):
    x = parse(v.x)
    eq_(parse(case(x)/{1:[write(1)],2:[write(4)],3:[write(4)], els:[write(5)]}), 
        special.CaseForm(x,{1:[write(1)], 2:[write(4)], 3:[write(4)]}, [write(5)]))
    
class TestEach:
  def test_slice(self):
    i = parse(v.i); j = parse(v.j)
    eq_(parse(each(v.i,v.j)[1:3][1:3].do[write(v.i)]), 
        special.EachForm((i,j), zip(range(1,3),range(1,3)),[write(i)])) 
##  def test_slice2(self):
##    i = v.i; j = v.j
##    eq_(parse(each(i,j)['a':'z']['A':'Z'].do[write(i)]), 
##        special.EachForm((i,j), zip(range(1,3),range(1,3)),[write(i)])) 
  def test_getitem1(self):
    i = parse(v.i); j = parse(v.j)
    eq_(parse(each(v.i,v.j)[zip(range(2), range(2))].do[write(v.i, v.j)]), 
        special.EachForm((i,j), tuple(zip(range(2),range(2))),[write(i,j)])) 
  def test_getitem2(self):
    i = parse(v.i); j = parse(v.j)
    eq_(parse(each(v.i,v.j)[range(2)][range(2)].do[write(v.i, v.j)]), 
        special.EachForm((i,j), zip(range(2),range(2)),[write(i,j)]))
    
class TestExitNext:
  def test_exit1(self):
    eq_(parse(exit.loop), special.exit(None, 'loop')) 
  def test_exit2(self):
    eq_(parse(exit.loop/2>>v.i), special.exit(i, 'loop', 2)) 
  def test_next1(self):
    eq_(parse(next.loop), special.next('loop')) 
  def test_next2(self):
    eq_(parse(next.loop/2), special.next('loop', 2)) 
    
class TestBlockLabel:
  def test_label(self):
    eq_(parse(label.a%loop[0]), special.LoopForm([0], 'a')) 
  def test_block(self):
    eq_(parse(block.a[1]), special.block('a', 1)) 
  def test_block2(self):
    eq_(parse(block.a[ v.i << 1 ]), special.block('a', special.set(i,1))) 

class TestFun:
  def test_at(self):
    at1 = at(i)[1](j)[2][3](x,y)[4]
    eq_(parse(at1), 
        AtForm([((i,),[[1]]), ((j,),[[2],[3]]),((x,y),[[4]])])) 
  def test_at2(self):
    eq_(parse(at[write(1)]), AtForm([(None,[[write(1)]])]))
  def test1(self):
    eq_(parse(fun. a(x)== [write(1)]), replace_def(a, (x,), [(write(1),)]))
  def test2(self):
    eq_(parse(fun. a(x)== at[write(1)]), replace_def(a, (x,), [[write(1)]]))
  def test3(self):
    eq_(parse(fun. a(x)>= [write(1)]), 
        append_def(a, (x,), [(write(1),)], special.UserFunction))
  def test4(self):
    eq_(parse(fun. a(x)>= at[write(1)]), 
        append_def(a, (x,), [[write(1)]], special.UserFunction))
  def test41(self):
    eq_(parse(fun. a(x)>= at[write(1)][write(2)]), 
        append_def(a, (x,), [[write(1)],[write(2)]], special.UserFunction))
  def test42(self):
    eq_(parse(fun. a(x)<= at[write(1)]),
        insert_def(a, (x,), [[write(1)]], special.UserFunction))
  def test5(self):
    eq_(parse(fun. a== at()[write(1)]), 
        special.set(a, special.FunctionForm([((), [write(1)])])))
  def test6(self):
    eq_(parse(fun. a>= at()[write(1)]), 
        special.begin(append_def(a, (), [[write(1)]], special.UserFunction)))
  def test61(self):
    eq_(parse(fun. a<= at()[write(1)]), 
        special.begin(insert_def(a, (), [[write(1)]], special.UserFunction)))
  def test7(self):
    eq_(parse(-fun. a/3),abolish(a,3))
  def test8(self):
    eq_(parse(-fun. a(x)), remove(a,(x,), special.UserFunction))
  def test9(self):
    eq_(parse(fun()[write(1)]), special.FunctionForm(((), [write(1)])))
class TestMacro:
  def test5(self):
    eq_(parse(fun. a== at()[write(1)]), 
        special.set(a, special.MacroForm([((), [write(1)])])))
    

