from nose.tools import eq_, assert_raises

from dao.command import Var, DummyVar, LogicVar
from dao.solve import eval
from dao.solvebase import NoSolution
from dao.builtins import rules, let, letrec, begin, eval_
from dao.builtins import fail, or_, and_, not_p, cut
from dao.builtins import add
from dao.builtins import set_text, parse_text, unify_parse_text
from dao.builtins import step, next_char, position, goto, skip, left, subtext
from dao.builtins import char, eoi, word, literal#, integer, eoi, literal, letter, digit 
#from dao.builtins import dqstring, sqstring, unify_tabspaces, unify_whitespaces, uLetterdigitString
from dao.builtins import may, nullword, identifier, integer#, optional, parallel
from dao.builtins import any#, some, times, times_more, times_less, seplist
from dao.builtins import lazy#, times_between
#from dao.builtins import contain
from dao.builtins import println

from dao.tests.util import *

class TestLowLevelPrimitive:
  def test_step(self):
    eq_(eval(begin(set_text('ab'), step())), 'a') #, step()
    #eq_(eval(begin(set_text('ab'), step(), left())), 'b')
    
  def test_next(self):
    eq_(eval(begin(set_text('ab'), next_char(), next_char())), 'a')
    
  def test_left(self):
    eq_(eval(begin(set_text('ab'), left())), 'ab')
    
  def test_position(self):
    eq_(eval(begin(set_text('ab'), position())), 0)
  def test_subtext(self):
    eq_(eval(begin(set_text('abcde'), subtext(0,3))), 'abc')
    
  def test_skip(self):
    eq_(eval(begin(set_text('abcde'), skip(3), next())), 'd')
    eq_(eval(begin(set_text('abcde'), skip(4), skip(-2), next())), 'c')
    
  def test_skip(self):
    eq_(eval(begin(set_text('abcde'), goto(2), next_char())), 'c')
    
  def test_goto(self):
    eq_(eval(begin(set_text('abcde'), goto(1))), 'b')
    
  def test_unify_parse_text(self):
    x = LogicVar('x')
    eq_(eval(begin(set_text('abcde'), unify_parse_text(x), x)), 'abcde')
    eq_(eval(begin(set_text('abcde'), unify_parse_text('abcde'))), True)
    
  def test_parse(self):
    eq_(eval(begin(set_text('abcde'), char('a'))), 'a')
    
  def test_parse2(self):
    eq_(eval(begin(set_text('a'), char('a'), eoi)), True)
    #assert_raises(NoSolution, eval, begin(set_text('ab'), char('a'), eoi))
    
  def test_parse3(self):
    eq_(eval(begin(set_text('aaa'), any(char('a')), eoi)), True)
    

class XTestParameterize:
  def test_chars(self):
    x, cs,chars = Var('x'), Var('cs'), Var('chars')
    eq_(eval(let([(chars, function(((x, cs), and_p(char(x), contain(cs, x)))))],
                            parse_text(chars(x, 'a'), 'a'))), True)
  def test_kleene1(self):
    f, item, kleene = Var('f'), Var('item'), Var('kleene')
    fun = macro(((item,),  
                         letr([(f,macro(((), eval_(item), f()),
                                         ((), nullword)))], 
                              f())))
    eq_(eval(let([(kleene,fun)], set_text('aa'), kleene(char('a')))), True)
  def test_kleene2(self):
    f, pred, kleene = Var('f'), Var('pred'), Var('kleene')
    fun = macro(((pred,),  
                         letr([(f,macro( ((x,), pred(x), f(x)),
                                          ((x,), nullword)))], 
                              f(x))))
    eq_(eval(let([(kleene,fun)], set_text('ab'), kleene(char))), True)
    
class Testterminal:
  def test_char(self):
    eq_(eval(parse_text(char('a'), 'a')), 'a')
    
  def test_nullword1(self):
    eq_(eval(parse_text(begin(char('a'), nullword, char('b')), 'ab')), 'b')
    
  def testnullword2(self):
    assert_raises(NoSolution, eval, parse_text(and_(nullword, eoi), 'a')) 
    eq_(eval(parse_text(nullword, '')), True)
    
  def testnullword3(self):
    rule = and_(char('a'), nullword, char('b'))
    #eq_(eval(parse_text(rule, 'ab')), 'b') # passed
    assert_raises(NoSolution, eval, parse_text(rule, 'a b'))
    #assert_raises(NoSolution, eval, parse_text(rule, 'a'))
    
  def test_word(self):
    x, y, z = LogicVar('y'), LogicVar('x'), LogicVar('z')
    eq_(eval(begin(parse_text(word(x), 'ab'), x)), 'ab')
    eq_(eval(begin(parse_text(word('ab'), 'ab'))), 'ab')
     
  def test_identifier(self):
    x = LogicVar('x')
    eq_(eval(begin(parse_text(identifier(x), '_a1b_23'), x)), '_a1b_23')
    #eq_(eval(begin(parse_text(identifier('_a1b_23'), '_a1b_23'))), '_a1b_23')
     
  def test_number(self):
    x, y, z = LogicVar('y'), LogicVar('x'), LogicVar('z')
    eq_(eval(begin(parse_text(integer(x), '2'), x)), '2')
    eq_(eval(begin(parse_text(integer(y), '234'), y)), '234')
    eq_(eval(begin(parse_text(integer(z), '0232'), z)), '0232') 
    
  def test_literal(self):
    eq_(eval(parse_text(literal('if'), 'if')), 'if')
    assert_raises(NoSolution, eval, parse_text(literal('if'), 'ssf'))

  def xtest_string(self):
    x ,y  = Var('x'), Var('y')
    eq_(eval(begin(parse_text(dqstring(x), '"2"'), x)), "2")
    eq_(eval(begin(parse_text(sqstring(y), "'1'"), y)), "1")

  def xtest_spaces(self):
    x ,y  = Var('x'), Var('y')
    eq_(eval(begin(parse_text(unify_tabspaces(x), ' '), x)), " ")
    eq_(eval(begin(parse_text(unify_whitespaces(y), "\r\t\n"), y)), "\r\t\n")

  def xtest_uLetterdigit(self):
    x ,y  = Var('x'), Var('y')
    eq_(eval(begin(parse_text(uLetterdigitString(x), '_ad123'), x)), "_ad123")
    eq_(eval(begin(parse_text(uLetterdigitString(y), "ad1sff23"), y)), "ad1sff23")
    
class XTestrule:
  def test_two_rule(self):
    eq_(eval(letr([(f,function( ((),char('a')),((),char('b'))))], 
               parse_text(f(),'a'), parse_text(f(), 'b'))), 
        'b')
    
  def test_right_recursive1(self):
    function1 = function( ((), and_p(char('a'), f())),
                     ((),char('b')))
    eq_(eval(letr([(f,function1)], 
               parse_text(f(),'b'), parse_text(f(),'ab'), parse_text(f(),'aab'))), 
        'b')
    assert_raises(NoSolution, eval, letr([(f,function1)], parse_text(f(), 'a')))
    
  def test_right_recursive2(self):
    x, p = Var('x'), Var('p')
    function1 = [(p,function( ((), and_p(char(x), p())),
                     ((),char(x))))]
    eq_(eval(letr(function1, parse_text(p(),'a'), parse_text(p(),'ab'), parse_text(p(),'abc'))),
        'c')
    assert_raises(NoSolution, eval, letr(function1, parse_text(p(), '')))

  def test_unify_right_recursive(self):
    x, p = Var('x'), Var('p')
    function1 = [(p,function( ((x,), and_p(char(x), p(x))),
                          ((x,),char(x))))]
    eq_(eval(letr(function1, parse_text(p(x), 'aa'))), 'a')
    eq_(eval(letr(function1, parse_text(p(x), 'a'))), 'a')
    assert_raises(NoSolution, eval, letr(function1, parse_text(and_p(p(x), eoi), 'xy')))
    assert_raises(NoSolution, eval, letr(function1, parse_text(p(x), '')))
    
  def xxxtest_left_recursive(self):
    assert 0, 'a good idea is waiting to be implemented'
    assert 0, 'googd idea failed!'
    engine = Engine()
    X = Var()
    f = userTerm("f")
    engine.add_rule(rule(f(X), f(X)+char('1')))
    engine.add_rule(rule(f(X),char('2')))
    X = Var()
    engine.run(f(X), '21')
    engine.run(f(X), '2')
    assert_raises(UnifyFail, engine.run, f(X), '1')
      
class XTestOr:
  def testOr(self):
    x, s, one, two = Var('x'), Var('s'), Var('one'), Var('two')
    ruleList = [(s, function( ((x,), or_p(one(x), two(x))))),
                (one, function( (('1',),char('1')))),
                (two, function( (('2',),char('2'))))]
    eq_(eval(letr(ruleList, parse_text(s(x), '1'))), '1')
    eq_(eval(letr(ruleList, parse_text(s(y),  '2'))), '2')         
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(s(x), '3')))
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(and_p(s(x), eoi), '12')))
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(s(x), '')))

class XTestOptional:
  def test_optional(self):
    x = Var('x')
    assert_raises(NoSolution, eval, preparse(parse_text(~char(x)+char('1'), '1')))
    eq_(eval(parse_text(optional(char(x)), '1')), '1')
    eq_(eval(parse_text(optional(char(x)), '')), True)

  def testoptionalRule(self):
    x, s = Var('x'), Var('s')
    ruleList =[(s, function( ((x,), and_p(~char('a'),char(x)))))]
    assert_raises(NoSolution, eval, let(ruleList, parse_text(s(x), 'a'), x))
    eq_(eval(let(ruleList, parse_text(s(x),  'aa'), x)), 'a')         
    eq_(eval(let(ruleList, parse_text(s(x),  'b'), x)), 'b')
    
  def test_nongreedy_optional(self):
    x, s = Var('x'), Var('s')
    ruleList =[(s, function( ((x,), and_p(-char('a'),char(x)))))]
    eq_(eval(let(ruleList, parse_text(s(x), 'a'), x)), 'a')
    eq_(eval(let(ruleList, parse_text(s(x),  'aa'), x)), 'a')         
    eq_(eval(let(ruleList, parse_text(s(x),  'b'), x)), 'b')
    
  def testoptionalcut(self):
    x, s = Var('x'), Var('s')
    ruleList = [(s, function( ((x,), and_p(-char('a'), cut, char(x)))))]
    eq_(eval(let(ruleList, parse_text(s(x),  'aa'), x)), 'a')         
    eq_(eval(let(ruleList, parse_text(s(x),  'b'), x)), 'b') 
    assert_raises(NoSolution, eval, let(ruleList, parse_text(s(x), 'a'), x))
    
class XTestParallel:
  def test_parallel(self):
    x = Var('x')
    eq_(eval(parse_text(parallel(letter(x), char(x)), 'a')), 'a')
    assert_raises(NoSolution, eval, parse_text(parallel(integer(x), char('3')), '2'))
    
  def testparallelRule(self):
    x, s, gt, lt = Var('x'), Var('s'), Var('>'), Var('<')
    ruleList = [(s,function( ((x,), parallel(gt(x, 3), lt(x, 5))))),
                (gt,function( ((4, 3),char('4')))),
                (lt,function( ((4, 5),char('4'))))]
    eq_(eval(letr(ruleList, parse_text(s(x), '4'), x)), 4)
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(s(x), '6'), x))
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(and_p(s(x), eoi), '41'), x))
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(s(x), ''), x))
        
class XTestAnySomeTimesSepList:
  def test_any_some(self):
    X, Y = Var('X'), Var('Y')
    eq_(eval(begin(parse_text(char(X)+any(~char('b')+some(char(X)))+eoi, 'abaaaa'), X)), 'a')
  def test_any(self):
    X, Y = Var('X'), Var('Y')
    eq_(eval(begin(parse_text(any(char(X), X, Y), '222'), Y)), ['2','2','2'])
    eq_(eval(begin(parse_text(any(char(X), X, Y), '234'), Y)), ['2'])
    eq_(eval(begin(parse_text(any(char(X), X, Y), ''), Y)), [])
  def test_any2(self):
    eq_(eval(parse_text(any(or_p(char('1'), char('2'))), '2')), True)
  def test_dummy_any(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(begin(parse_text(any(char(_), _, Y), '222'), Y)), ['2','2','2'])
    eq_(eval(begin(parse_text(any(char(_), _, Y), '234'), Y)), ['2', '3', '4'])
    eq_(eval(begin(parse_text(any(char(_), _, Y), ''), Y)), [])
    
  def test_some(self):
    X, Y = Var('X'), Var('Y')
    eq_(eval(begin(parse_text(some(char(X), X, Y), '222'), Y)), ['2','2','2'])
    eq_(eval(begin(parse_text(some(char(X), X, Y), '234'), Y)), ['2'])
    assert_raises(NoSolution, eval, begin(parse_text(some(char(X), X, Y), ''), Y))
  def test_some2(self):
    X = Var('X')
    eq_(eval(parse_text(and_p(some( char(X)),char('4')), '224')), '4')
    eq_(eval(parse_text(and_p(some(char(X)), cut, char('4')), '224')), '4')
    assert_raises(NoSolution, eval, parse_text(and_p(some(char(X)), char('3'), char('5')), '2234'))
  def test_some3(self):
    eq_(eval(parse_text(some(or_p(char('1'),char('2'))), '2')), True)
  def test_dumy_some(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(begin(parse_text(some(char(_), _, Y), '222'), Y)), ['2','2','2'])
    eq_(eval(begin(parse_text(some(char(_), _, Y), '234'), Y)),['2', '3', '4'])
    assert_raises(NoSolution, eval, begin(parse_text(some(char(_), _, Y), ''), Y))
    
  def test_times(self):
    X, Y = Var('X'), Var('Y')
    eq_(eval(begin(parse_text(times(char(X), 3, X, Y), '222'), Y)), ['2','2','2'])
    assert_raises(NoSolution, eval, parse_text(times(char(X), 3, X, Y), '2234'))
  def test_dummy_times(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(begin(parse_text(times(char(_), 3, _, Y), '234'), Y)), ['2','3','4'])
  def test_dummy_times_more(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(begin(parse_text((times_more,char(_), 3, _, Y), '234'), Y)), ['2','3','4'])
    eq_(eval(begin(parse_text((times_more,char(_), 3, _, Y), '2345'), Y)), ['2','3','4', '5'])
    assert_raises(NoSolution, eval, begin(parse_text(times_more(char(_), 3, _, Y), '23'), Y))
  def test_dummy_times_less(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(preparse(begin(parse_text(times_less(char(_), 3, _, Y)+char('4'), '234'), Y))), ['2','3'])
    eq_(eval(begin(parse_text(times_less(char(_), 3, _, Y), '234'), Y)), ['2','3','4'])
    eq_(eval(begin(parse_text(times_less(char(_), 3, _, Y), '23'), Y)), ['2','3'])
    assert_raises(NoSolution, eval, preparse(begin(parse_text(times_less(char(_), 3, _, Y)+eoi, '2345'), Y)))
  def test_dummy_times_less_lazy(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(preparse(begin(parse_text(times_less(char(_), 3, _, Y, lazy)+char('4'), '234'), Y))), ['2','3'])
  def test_dummy_times_between(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(begin(parse_text(times_between(char(_), 2, 3, _, Y), '234'), Y)), ['2','3', '4'])
    eq_(eval(begin(parse_text(times_between(char(_), 2, 3, _, Y), '23'), Y)), ['2','3'])
    eq_(eval(begin(parse_text(times_between(char(_), 2, 3, _, Y), '2345'), Y)), ['2','3', '4'])
    assert_raises(NoSolution, eval, begin(parse_text(times_between(char(_), 2, 3, _, Y), '2'), Y))
    assert_raises(NoSolution, eval, begin(parse_text((and_p, times_between(char(_), 2, 3, _, Y), eoi), '2345'), Y))
  def test_times_a2(self): 
    X, Y, S = Var('X'), Var('Y'), Var('S')
    function1 = function(((Y,), times(char('a'), 2, 'a', Y)))
    eq_(eval(begin(parse_text(function1(X),'aa'), X)), ['a', 'a'])
    assert_raises(NoSolution, eval, begin(parse_text(function1(X), 'a'), X))
    assert_raises(NoSolution, eval, begin(parse_text(and_p(function1(X), eoi), 'aaa'), X))
  def test_times_an(self): 
    X, Y, S, n = Var('X'), Var('Y'), Var('S'), Var('n')
    function1 = function( ((Y,), times(char('a'), n, 'a', Y)))
    eq_(eval(begin(parse_text(function1(X), 'a'), X)), ['a'])
    eq_(eval(begin(parse_text(function1(X), 'aa'), X)), ['a', 'a'])
    eq_(eval(begin(parse_text(function1(X), 'aaa'), X)), ['a', 'a', 'a'])
    
  def test_seplist(self):
    X, Y = Var('X'), Var('Y')
    eq_(eval(begin(parse_text(seplist(char(X), char(','), X, Y), '2,2,2'), Y)), ['2','2','2'])
    eq_(eval(begin(parse_text(seplist(char(X), char(','), X, Y), '2,3,4'), Y)), ['2'])
    eq_(eval(begin(parse_text(seplist(char(X), char(','), X, Y), '2'), Y)), ['2'])
    eq_(eval(begin(parse_text(seplist(char(X), char(','), X, Y), ''), Y)), [])
  def test_dummy_seplist(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(begin(parse_text(seplist(char(_), char(','), _, Y), '2,2,2'), Y)), ['2','2','2'])
    eq_(eval(begin(parse_text(seplist(char(_), char(','), _, Y), '2,3,4'), Y)), ['2', '3', '4'])
    eq_(eval(begin(parse_text(seplist(char(_), char(','), _, Y), '2'), Y)), ['2'])
    
class XTestKleeneByfunction:  
  def testKleene1(self): #occurs_check
    x, s, kleene = Var('x'), Var('s'), Var('kleene')
    ruleList = [(s,function( ((x,), kleene(x)))),
                (kleene,function( 
                  ((Cons('a', x),), and_p(char('a'), kleene(x))),
                  ((nil,), nullword)))]
    eq_(eval(letr(ruleList, parse_text(s(x), 'aa'), x)), L('a', 'a'))
    eq_(eval(letr(ruleList, parse_text(s(x), ''), x)), nil)
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(and_p(s(x), eoi), '6'), x))
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(and_p(s(x), eoi), '41'), x))

  def testKleene2(self):
    x, c, s, kleene = Var('x'), Var('c'), Var('s'), Var('kleene')
    ruleList = [(s,function( ((x,), kleene(c, x)))),
                (kleene,function( 
                  ((c, Cons(c, x)), and_p(char(c), kleene(c, x))),
                  ((c, nil), nullword)))]
    eq_(eval(letr(ruleList, parse_text(s(x), 'aa'), x)), L('a', 'a'))
    eq_(eval(letr(ruleList, parse_text(s(x), 'aaa'), x)), L('a', 'a', 'a'))
    eq_(eval(letr(ruleList, parse_text(s(x), 'bbb'), x)), L('b', 'b', 'b'))
    eq_(eval(letr(ruleList, parse_text(s(x), ''), x)), nil)
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(and_p(s(x), eoi), 'aab'), x))
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(and_p(s(x), eoi), 'abc'), x))
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(and_p(s(x), eoi), '41'), x))

  def testKleene3(self):
    x, c, kleene = Var('x'), Var('c'), Var('kleene')
    ruleList = [(kleene,function( 
                  ((Cons(c, x),), char(c),kleene(x)),
                  ((nil,), nullword)))]
    eq_(eval(letr(ruleList, parse_text(kleene(x), 'a'), x)), L('a'))
    eq_(eval(letr(ruleList, parse_text(kleene(x), 'aa'), x)), L('a', 'a'))
    eq_(eval(letr(ruleList, parse_text(kleene(x), 'aaa'), x)), L('a', 'a', 'a'))
    eq_(eval(letr(ruleList, parse_text(kleene(x), 'bbb'), x)), L('b', 'b', 'b'))
    eq_(eval(letr(ruleList, parse_text(kleene(x), 'abc'), x)), L('a', 'b', 'c'))
    eq_(eval(letr(ruleList, parse_text(kleene(x), ''), x)), nil)

  def testKleene4(self):
    x, _, c, s, kleene = Var('x'), DummyVar('_'), Var('c'), Var('s'), Var('kleene')
    ruleList = [(s,function( ((x,), kleene(_, x)))),
                (kleene,function( 
                  ((_, Cons(c, x)), char(c)&kleene(_, x)),
                  ((_, nil), nullword)))]
    eq_(eval(letr(ruleList, parse_text(s(x), 'a'), x)), L('a'))
    eq_(eval(letr(ruleList, parse_text(s(x), 'aa'), x)), L('a', 'a'))
    eq_(eval(letr(ruleList, parse_text(s(x), 'aaa'), x)), L('a', 'a', 'a'))
    eq_(eval(letr(ruleList, parse_text(s(x), 'bbb'), x)), L('b', 'b', 'b'))
    eq_(eval(letr(ruleList, parse_text(s(x), 'abc'), x)), L('a', 'b', 'c'))
    eq_(eval(letr(ruleList, parse_text(s(x), ''), x)), nil)


class XTestIndentUndent:
  def testIndentUndent(self):
    _, n, s, line = DummyVar('_'), Var('n'), Var('s'), Var('line')
    space = char(' ')
    ruleList = [(s,function( ((n,), some(line(n)),s(add(n,1))),
                            ((n,), some(line(n))))),
                (line,function( ((n,), times(space, n),some(letter(_)),any(space),char('\n'))))
                ]
    eq_(eval(letr(ruleList, parse_text(s(0),  'a\n b\n c\n'))), True)
    eq_(eval(letr(ruleList, parse_text(s(0),  'asd\n bdf\n cdfh\n'))), True)    

class XTestExpression:          
  def testRecursiveReturnValue1(self):
    E, F, G, e, e1 = Var('E'), Var('F'), Var('G'), Var('e'), Var('e1')
    ruleList = [(E,function(((e,), F(e)))),
                (E,function(((e1,), G(e1)))),
                (G,function(((1,), char('1'))))]
    eq_(eval(letr(ruleList, parse_text(E(e),  '1'), e)), 1)
    
  def testRecursiveReturnValue2(self):
    E, F, e, e1 = Var('E'), Var('F'), Var('e'), Var('e1')
    ruleList = [(E,function((((e, e),), F(e)))),
                (F,function(((1,), char('1'))))]
    eq_(eval(letr(ruleList, parse_text(E(e),  '1'), e)), (1, 1))
    
  def testRecursiveReturnValue3(self):
    E, e, e1, e2 = Var('E'), Var('e'), Var('e1'), Var('e2')  
    ruleList = [(E,function( 
                     ((e, 1), E(e, 2)),
                     ((e, 2), char(e)),
                     (((e1, e2), 1), E(e1,2), E(e2, 1))
                         ))]
    eq_(eval(letr(ruleList, parse_text(E(e, 1)+eoi, '12'), e)), ('1', '2'))

  def testExpressionByRightRecursiveList(self):
    E, e, e1, e2 = Var('E'), Var('e'), Var('e1'), Var('e2')  
    ruleList = [(E,function( 
                     (((e1, '/', e2), 1), E(e1,2), char('/'), E(e2, 1)),
                     ((1, 2), char('1')),
                     ((e, 1), E(e, 2))))]
    eq_(eval(letr(ruleList, parse_text(E(e, 1),  '1/1/1'), e)), (1, '/', (1, '/', 1)))
    eq_(eval(letr(ruleList, parse_text(E(e, 1),  '1/1'), e)), (1, '/', 1))
    eq_(eval(letr(ruleList, parse_text(E(e, 1),  '1'), e)), 1)
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(and_p(E(e, 1), eoi), '1+1/1'), e))
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(and_p(E(e, 1), eoi), '2'), e))
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(and_p(E(e, 1), eoi), '1/'), e))
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(and_p(E(e, 1), eoi), '/'), e))
    assert_raises(NoSolution, eval, letr(ruleList, parse_text(and_p(E(e, 1), eoi), ''), e))

class XTestLeftRecursive:          
  def testDirectLeftRecursive(self):
    #assert 0, 'temporary mask'
    E = Var('E')
    ruleList = [(E,function( 
                     ((), #println('e1'), 
                          E(), 
                          #println('e2'), 
                          char('a'), 
                          #println('e3')
                          ),
                     ((), char('b'), 
                          #println('eb')
                          ),
                     ))]
    eq_(eval(letr(ruleList, parse_text(E()+eoi,  'b'))), True)
    eq_(eval(letr(ruleList, parse_text(E()+eoi,  'ba'))), True)
    eq_(eval(letr(ruleList, parse_text(E()+eoi,  'baa'))), True)
  def testDirectLeftRecursiveWithArguments(self):
    #assert 0, 'temporary mask'
    E, X = Var('E'), Var('X')
    ruleList = [(E,function( 
                     ((), #println('e1'), 
                          E(), 
                          #println('e2'), 
                          digit(X), 
                          #println('e3', X)
                          ),
                     ((), char('b'), 
                          #println('eb')
                          ),
                     ))]
    #eq_(eval(letr(ruleList, parse_text(E()+eoi,  'b'))), True)
    #eq_(eval(letr(ruleList, parse_text(E()+eoi,  'b1'))), True)
    eq_(eval(letr(ruleList, parse_text(E()+eoi,  'b123'))), True)
  def testIndirectLeftRecursive(self):
    #assert 0, 'temporary mask'
    A, B, C = vars('A, B, C')
    ruleList = [(A, function(((), B()))), 
                (B, function(
                      ((), A()+char('a')),
                       ((), char('b')),
                     ))]
    eq_(eval(letr(ruleList, parse_text(A()+eoi,  'b'))), True)
    eq_(eval(letr(ruleList, parse_text(A()+eoi,  'ba'))), True)
    eq_(eval(letr(ruleList, parse_text(A()+eoi,  'baa'))), True)

class XTestChartParsing:          
  def testABCD(self):
    A, B, C, D = vars('A, B, C, D')
    ruleList = [(A,function(((), B()|C()))), 
                (B,function(((), D()+char('b')))), 
                (C,function(((), D()+char('c')))), 
                (D,function(((), char('d')))), 
               ]
    eq_(eval(letr(ruleList, parse_text(A()+eoi,  'dc'))), True)
    
