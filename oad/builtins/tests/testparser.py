from nose.tools import eq_, assert_raises

from oad.term import Var, DummyVar, conslist as L
from oad.solve import eval
from oad.builtins.control import fail, or_, and_, not_, cut

from oad.special import function, let, letrec, macro, begin
from oad.builtins.arith import add
from oad.builtins.parser import settext, parse
from oad.builtins.parser import step, left, next, position, subtext, goto, skip
from oad.builtins.terminal import char, nullword, number, eof, literal, letter 
from oad.builtins.terminal import dqstring, sqstring, spaces, uLetterdigitString
from oad.builtins.matchterm import optional, parallel, any, some, times, seplist
from oad.builtins.atom import charin
from oad.testutil import *

class xTestLineparser:
  def test_row_column(self):
    from oad.builtins.lineparser import settext, row, column
    eq_(eval(begin(settext('ab'), row())), 0)
    eq_(eval(begin(settext('ab'), row(),char('a'), column())), 1)
    
class TestLowLevelPrimitive:
  def test_step(self):
    eq_(eval(begin(settext('ab'), step(), step())), 'b')
    eq_(eval(begin(settext('ab'), step(), left())), 'b')
  def test_next(self):
    eq_(eval(begin(settext('ab'), next(), next())), 'a')
  def test_left(self):
    eq_(eval(begin(settext('ab'), left())), 'ab')
  def test_position(self):
    eq_(eval(begin(settext('ab'), position())), 0)
  def test_subtext(self):
    eq_(eval(begin(settext('abcde'), subtext(0,3))), 'abc')
  def test_skip(self):
    eq_(eval(begin(settext('abcde'), skip(3), next())), 'd')
    eq_(eval(begin(settext('abcde'), skip(4), skip(-2), next())), 'c')
  def test_skip(self):
    eq_(eval(begin(settext('abcde'), goto(2), next())), 'c')
  def test_goto(self):
    eq_(eval(begin(settext('abcde'), goto(1))), 'bcde')

class xTestParameterize:
  def test_chars(self):
    x, cs,chars = Var('x'), Var('cs'), Var('chars')
    eq_(eval(let({chars: function(((x, cs), and_(char(x),charin(x, cs))))},
                            parse(chars, x, 'a'), 'a')), True)
  def test_kleene1(self):
    f, item, kleene = Var('f'), Var('item'), Var('kleene')
    fun = macro(((item,),  
                         letrec({f:macro(((), and_(item, f())),
                                         ((), epsilon()))}, 
                              f())))
    eq_(eval(let({kleene:fun}, settext('aa'), kleene(char('a')))), True)
  def test_kleene2(self):
    f, pred, kleene = Var('f'), Var('pred'), Var('kleene')
    fun = macro(((pred,),  
                         letrec({f:macro( ((x,), and_(pred(x), f(x))),
                                          ((x,), epsilon()))}, 
                              f())))
    eq_(eval(let({kleene:fun}, settext('ab'), kleene(char)(x))), True)
    
class Testterminal:
  def test_char(self):
    eq_(eval(parse(char('a'), 'a')), 'a')
  def test_nullword1(self):
    eq_(eval(parse(char('a')&nullword&char('b'), 'ab')), 'b')    
  def testnullword2(self):
    eq_(eval(parse(and_(nullword, eof), 'a')), None) 
    eq_(eval(parse(nullword, '')), True)
  def testnullword3(self):
    rule = and_(char('a'), nullword, char('b'))
    eq_(eval(parse(rule, 'ab')), 'b')
    eq_(eval(parse(rule, 'a b')), None)
    eq_(eval(parse(rule, 'a')), None)
    
  def test_number(self):
    x, y, z = Var('y'), Var('x'), Var('z')
    eq_(eval(begin(parse(number(x), '2'), x)), 2)
##    eq_(eval(begin(parse(number(y), '234'), y)), 234)
##    eq_(eval(begin(parse(number(z), '0232'), z)), 154) #0ctal
    
  def test_literal(self):
    eq_(eval(parse(literal('if'), 'if')), True)
    eq_(eval(parse(literal('if'), 'ssf')), None)

  def test_string(self):
    x ,y  = Var('x'), Var('y')
    eq_(eval(begin(parse(dqstring(x), '"2"'), x)), "2")
    eq_(eval(begin(parse(sqstring(y), "'1'"), y)), "1")

  def test_spaces(self):
    x ,y  = Var('x'), Var('y')
    eq_(eval(begin(parse(spaces(x), ' '), x)), " ")
    eq_(eval(begin(parse(spaces(y), "\r\t\n"), y)), "\r\t\n")

  def test_uLetterdigit(self):
    x ,y  = Var('x'), Var('y')
    eq_(eval(begin(parse(uLetterdigitString(x), '_ad123'), x)), "_ad123")
    eq_(eval(begin(parse(uLetterdigitString(y), "ad1sff23"), y)), "ad1sff23")
    
class Testrule:
  def test_two_rule(self):
    eq_(eval(letrec({f:function( ((),char('a')),((),char('b')))}, 
               parse(f(),'a'), parse(f(), 'b'))), 
        'b')
    
  def test_right_recursive1(self):
    function1 = function( ((), and_(char('a'), f())),
                     ((),char('b')))
    eq_(eval(letrec({f:function1}, 
               parse(f(),'b'), parse(f(),'ab'), parse(f(),'aab'))), 
        'b')
    eq_(eval(letrec({f:function1}, parse(f(), 'a'))), None)
    
  def test_right_recursive2(self):
    x, p = Var('x'), Var('p')
    function1 = {p:function( ((), and_(char(x), p())),
                     ((),char(x)))}
    eq_(eval(letrec(function1, parse(p(),'a'), parse(p(),'ab'), parse(p(),'abc'))),
        'c')
    eq_(eval(letrec(function1, parse(p(), ''))), None)

  def test_unify_right_recursive(self):
    x, p = Var('x'), Var('p')
    function1 = {p:function( ((x,), and_(char(x), p(x))),
                          ((x,),char(x)))}
    eq_(eval(letrec(function1, parse(p(x), 'aa'))), 'a')
    eq_(eval(letrec(function1, parse(p(x), 'a'))), 'a')
    eq_(eval(letrec(function1, parse(and_(p(x), eof), 'xy'))), None)
    eq_(eval(letrec(function1, parse(p(x), ''))), None)
  def xxxtest_left_recursive(self):
    assert 0, 'a good idea is waiting to be implemented'
    engine = Engine()
    X = Var()
    f = userTerm("f")
    engine.add_rule(rule(f(X), f(X)+char('1')))
    engine.add_rule(rule(f(X),char('2')))
    X = Var()
    engine.run(f(X), '21')
    engine.run(f(X), '2')
    assert_raises(UnifyFail, engine.run, f(X), '1')
      
class TestOr:
  def testOr(self):
    x, s, one, two = Var('x'), Var('s'), Var('one'), Var('two')
    ruleList = {s: function( ((x,), or_(one(x), two(x)))),
                one: function( (('1',),char('1'))),
                two: function( (('2',),char('2')))}
    eq_(eval(letrec(ruleList, parse(s(x), '1'))), '1')
    eq_(eval(letrec(ruleList, parse(s(y),  '2'))), '2')         
    eq_(eval(letrec(ruleList, parse(s(x), '3'))), None)
    eq_(eval(letrec(ruleList, parse(and_(s(x), eof), '12'))), None)
    eq_(eval(letrec(ruleList, parse(s(x), ''))), None)

class TestOptional:
  def test_optional(self):
    x = Var('x')
    eq_(eval(parse(optional(char(x)), '1')), '1')
    eq_(eval(parse(optional(char(x)), '')), True)

  def testoptionalRule(self):
    x, s = Var('x'), Var('s')
    ruleList ={s: function( ((x,), and_(optional(char('a')),char(x))))}
    eq_(eval(let(ruleList, parse(s(x), 'a'), x)), 'a')
    eq_(eval(let(ruleList, parse(s(x),  'aa'), x)), 'a')         
    eq_(eval(let(ruleList, parse(s(x),  'b'), x)), 'b')
    
  def testoptionalcut(self):
    x, s = Var('x'), Var('s')
    ruleList = {s: function( ((x,), and_(optional(char('a')), cut, char(x))))}
    eq_(eval(let(ruleList, parse(s(x),  'aa'), x)), 'a')         
    eq_(eval(let(ruleList, parse(s(x),  'b'), x)), 'b') 
    eq_(eval(let(ruleList, parse(s(x), 'a'), x)), None)
    
class TestParallel:
  def test_parallel(self):
    x = Var('x')
    eq_(eval(parse(parallel(letter(x), char(x)), 'a')), True)
    eq_(eval(parse(parallel(number(x), char('3')), '2')), None)
  def testparallelRule(self):
    x, s, gt, lt = Var('x'), Var('s'), Var('>'), Var('<')
    ruleList = {s:function( ((x,), parallel(gt(x, 3), lt(x, 5)))),
                gt:function( ((4, 3),char('4'))),
                lt:function( ((4, 5),char('4')))}
    eq_(eval(letrec(ruleList, parse(s(x), '4'), x)), 4)
##    eq_(eval(letrec(ruleList, parse(s(x), '6'), x)), None)
##    eq_(eval(letrec(ruleList, parse(and_(s(x), eof), '41'), x)), None)
##    eq_(eval(letrec(ruleList, parse(s(x), ''), x)), None)
        
class xTestAnySomeTimesSepList:
  def test_any(self):
    X, Y = Var('X'), Var('Y')
    eq_(eval(begin(parse(any(char(X), X, Y), '222'), Y)), ['2','2','2'])
    eq_(eval(begin(parse(any(char(X), X, Y), '234'), Y)), ['2'])
    eq_(eval(begin(parse(any(char(X), X, Y), ''), Y)), [])
  def test_any2(self):
    eq_(eval(parse(any( or_( char('1'), char('2'))), '2')), True)
  def test_anydummy(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(begin(parse(any(char(_), _, Y), '222'), Y)), ['2','2','2'])
    eq_(eval(begin(parse(any(char(_), _, Y), '234'), Y)), ['2', '3', '4'])
    eq_(eval(begin(parse(any(char(_), _, Y), ''), Y)), [])
    
  def test_some(self):
    X, Y = Var('X'), Var('Y')
    eq_(eval(begin(parse(some(char(X), X, Y), '222'), Y)), ['2','2','2'])
    eq_(eval(begin(parse(some(char(X), X, Y), '234'), Y)), ['2'])
    eq_(eval(begin(parse(some(char(X), X, Y), ''), Y)), None)
  def test_some2(self):
    X = Var('X')
    eq_(eval(parse(and_(some( char(X)),char('4')), '224')), True)
    eq_(eval(parse(and_(some( char(X)), cut, char('4')), '224')), True)
    eq_(eval(parse(and_(some( char(X)), char('3'), char('5')), '2234')), None)
  def test_some3(self):
    eq_(eval(parse(some( or_(char('1'),char('2'))), '2')), True)
  def test_somedumy(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(begin(parse(some(char(_), _, Y), '222'), Y)), ['2','2','2'])
    eq_(eval(begin(parse(some(char(_), _, Y), '234'), Y)),['2', '3', '4'])
    eq_(eval(begin(parse(some(char(_), _, Y), ''), Y)), None)
    
  def test_times(self):
    X, Y = Var('X'), Var('Y')
    eq_(eval(begin(parse(times(char(X), 3, X, Y), '222'), Y)), ['2','2','2'])
    eq_(eval(parse(times(char(X), 3, X, Y), '2234')), None)
  def test_timesdummy(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(begin(parse(times(char(_), 3, _, Y), '234'), Y)), ['2','3','4'])
  def testtimes_a2(self): 
    X, Y, S = Var('X'), Var('Y'), Var('S')
    function1 = function( ((Y,), times(char('a'), 2, 'a', Y)))
    eq_(eval(begin(parse(function1(X),'aa'), X)), ['a', 'a'])
    eq_(eval(begin(parse(function1(X), 'a'), X)), None)
    eq_(eval(begin(parse(and_(function1(X), eof), 'aaa'), X)), None)
  def testtimes_an(self): 
    X, Y, S, n = Var('X'), Var('Y'), Var('S'), Var('n')
    function1 = function( ((Y,), times(char('a'), n, 'a', Y)))
    eq_(eval(begin(parse(function1(X), 'a'), X)), ['a'])
    eq_(eval(begin(parse(function1(X), 'aa'), X)), ['a', 'a'])
    eq_(eval(begin(parse(function1(X), 'aaa'), X)), ['a', 'a', 'a'])
    
  def test_seplist(self):
    X, Y = Var('X'), Var('Y')
    eq_(eval(begin(parse(seplist(char(X), char(','), X, Y), '2,2,2'), Y)), ['2','2','2'])
    eq_(eval(begin(parse(seplist(char(X), char(','), X, Y), '2,3,4'), Y)), ['2'])
    eq_(eval(begin(parse(seplist(char(X), char(','), X, Y), '2'), Y)), ['2'])
    #eq_(eval(begin(parse(seplist, char(X), char(','), X, Y), ''), Y))
  def test_seplistdummy(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(begin(parse(seplist(char(_), char(','), _, Y), '2,2,2'), Y)), ['2','2','2'])
    eq_(eval(begin(parse(seplist(char(_), char(','), _, Y), '2,3,4'), Y)), ['2', '3', '4'])
    eq_(eval(begin(parse(seplist(char(_), char(','), _, Y), '2'), Y)), ['2'])
    
class xTestKleeneByfunction:  
  def testKleene1(self): #occurs_check
    x, s, kleene = Var('x'), Var('s'), Var('kleene')
    ruleList = {s:function( ((x,), (kleene, x))),
                kleene:function( 
                  ((('a', x),), (and_(char('a'), kleene(x))),
                  (('',), epsilon())))}
    eq_(eval(letrec(ruleList, parse(s(x), 'aa'), x)), ['a', 'a', ''])
    eq_(eval(letrec(ruleList, parse(s(x), ''), x)), [''])
    eq_(eval(letrec(ruleList, (parse(and_(s, x), eof), '6'), x)), None)
    eq_(eval(letrec(ruleList, (parse(and_(s, x), eof), '41'), x)), None)

  def testKleene2(self):
    x, c, s, kleene = Var('x'), Var('c'), Var('s'), Var('kleene')
    ruleList = {s:function( ((x), (kleene, c, x))),
                kleene:function( 
                  ((c, (c, x)), (and_(char(c), (kleene, c, x))),
                  ((c, ''), epsilon())))}
    eq_(eval(letrec(ruleList, parse(s(x), 'aa'), x)), ['a', 'a', ''])
    eq_(eval(letrec(ruleList, parse(s(x), 'aaa'), x)), ['a', 'a', 'a', ''])
    eq_(eval(letrec(ruleList, parse(s(x), 'bbb'), x)), ['b', 'b', 'b', ''])
    eq_(eval(letrec(ruleList, parse(s(x), ''), x)), [''])
    eq_(eval(letrec(ruleList, (parse(and_(s, x), eof), 'aab'), x)), None)
    eq_(eval(letrec(ruleList, (parse(and_(s, x), eof), 'abc'), x)), None)
    eq_(eval(letrec(ruleList, (parse(and_(s, x), eof), '41'), x)), None)

  def testKleene3(self):
    x, _, c, s, kleene = Var('x'), DummyVar('_'), Var('c'), Var('s'), Var('kleene')
    ruleList = {s:function( ((x), (kleene, _, x))),
                kleene:function( 
                  ((_, (c, x)), (and_(char(c), (kleene, _, x))),
                  ((_, ''), epsilon())))}
    eq_(eval(letrec(ruleList, parse(s(x), 'aa'), x)), ['a', 'a', ''])
    eq_(eval(letrec(ruleList, parse(s(x), 'aaa'), x)), ['a', 'a', 'a', ''])
    eq_(eval(letrec(ruleList, parse(s(x), 'bbb'), x)), ['b', 'b', 'b', ''])
    eq_(eval(letrec(ruleList, parse(s(x), 'abc'), x)), ['a', 'b', 'c', ''])
    eq_(eval(letrec(ruleList, parse(s(x), ''), x)), [''])

class xtestIndentUndent:
  def testIndentUndent(self):
    _, n, s, line = DummyVar('_'), Var('n'), Var('s'), Var('line')
    space = char(' ')
    ruleList = {s:function( ((n,), and_(some( line(n)), (s, add(n, 1)))),
                            ((n,), some( line(n)))),
                line:function( ((n,), and_(times(space(n), some(letter(_)), any(space),char('\n')))))}
    eq_(eval(letrec(ruleList, parse(s(0),  'a\n b\n c\n'))), True)
    eq_(eval(letrec(ruleList, parse(s(0),  'asd\n bdf\n cdfh\n'))), True)

class xTestExpression:          
  def testExpressionByRightRecursiveList(self):
    E, e, e1, e2 = Var('E'), Var('e'), Var('e1'), Var('e2')  
    ruleList = {E:function( 
                     (((e1, '/', e2), 1), and_(E(e1,2), char('/'), E(e2, 1)),
                     ((1, 2), char('1')),
                     ((e, 1), E(e, 2))))}
    eq_(eval(letrec(ruleList, parse(E(e, 1),  '1/1/1'), e)), [1, '/', (1, '/', 1)])
    eq_(eval(letrec(ruleList, parse(E(e, 1),  '1/1'), e)), [1, '/', 1])
    eq_(eval(letrec(ruleList, parse(E(e, 1),  '1'), e)), 1)
    eq_(eval(letrec(ruleList, parse(and_(E(e, 1), eof), '1+1/1'), e)), None)
    eq_(eval(letrec(ruleList, parse(and_(E(e, 1), eof), '2'), e)), None)
    eq_(eval(letrec(ruleList, parse(and_(E(e, 1), eof), '1/'), e)), None)
    eq_(eval(letrec(ruleList, parse(and_(E(e, 1), eof), '/'), e)), None)
    eq_(eval(letrec(ruleList, parse(and_(E(e, 1), eof), ''), e)), None)
