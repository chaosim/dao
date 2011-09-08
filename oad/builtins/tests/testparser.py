from nose.tools import eq_, assert_raises

from oad.error import UnifyFail
from oad.term import Var, DummyVar, conslist as L, Symbol, atom
from oad.trail import Trail
from oad.eval import eval
from oad.builtins.control import True, fail, or_, and_, not_, cut
from oad.term import True as True, Integer, String

from oad.builtins.arith import add
from oad.builtins.parser import settext, parse
from oad.builtins.parser import step, left, next, position, subtext, goto, skip
from oad.builtins.terminal import char, epsilon, number, eof, literal, letter 
from oad.builtins.terminal import dqstring, sqstring, spaces, uLetterdigitString
from oad.builtins.matchterm import optional, parallel, any, some, times, seplist
from oad.builtins.atom import charin
from oad.tests.util import *

class TestLineparser:
  def test_row_column(self):
    from oad.builtins.lineparser import settext, row, column
    eq_(eval(L(begin, (settext, 'ab'), [row])), Integer(0))
    eq_(eval(L(begin, (settext, 'ab'), [row], [char, 'a'], [column])), Integer(1))
    
class TestLowLevelPrimitive:
  def test_step(self):
    eq_(eval(L(begin, (settext, 'ab'), [step], [step])), atom('b'))
    eq_(eval(L(begin, (settext, 'ab'), [step], [left])), atom('b'))
  def test_next(self):
    eq_(eval(L(begin, (settext, 'ab'), [next], [next])), atom('a'))
  def test_left(self):
    eq_(eval(L(begin, (settext, 'ab'), [left])), atom('ab'))
  def test_position(self):
    eq_(eval(L(begin, (settext, 'ab'), [position])), Integer(0))
  def test_subtext(self):
    eq_(eval(L(begin, (settext, 'abcde'), [subtext, 0,3])), atom('abc'))
  def test_skip(self):
    eq_(eval(L(begin, (settext, 'abcde'), [skip, 3], [next])), atom('d'))
    eq_(eval(L(begin, (settext, 'abcde'), [skip, 4], [skip, -2], [next])), atom('c'))
  def test_skip(self):
    eq_(eval(L(begin, (settext, 'abcde'), [goto, 2], [next])), atom('c'))

class TestParameterize:
  def test_chars(self):
    x, cs, chars = Var('x'), Var('cs'), Var('chars')
    eq_(eval(L(let, [[chars, [function, [[x, cs], (and_, (char, x), (charin, x, cs))]]]],
                            (parse, (chars, x, 'a'), 'a'))), True)
  def test_kleene1(self):
    f, item, kleene = Var('f'), Var('item'), Var('kleene')
    fun = [macro, [[item],  
                         [letrec, [[f, [macro, [[], (and_, item, [f])],
                                              [[], [epsilon]]]]], 
                              f]]]
    eq_(eval(L(let, [(kleene, fun)], (settext, 'aa'), [(kleene, (char, 'a'))])), True)
  def test_kleene2(self):
    f, pred, kleene = Var('f'), Var('pred'), Var('kleene')
    fun = [macro, [[pred],  
                         [letrec, [[f, [macro, [[x], (and_, (pred, x), [f, x])],
                                              [[x], [epsilon]]]]], 
                              f]]]
    eq_(eval(L(let, [(kleene, fun)], (settext, 'ab'), [(kleene, char), x])), True)
    
class Testterminal:
  def test_char(self):
    eq_(eval(L(parse, (char, 'a'), 'a')), True)
  def test_epsilon1(self):
    eq_(eval(L(parse, (and_, (char, 'a'), [epsilon], (char, 'b')), 'ab')), 
        True)    
  def testepsilon2(self):
    assert_raises(UnifyFail, eval, L(parse, (and_, [epsilon], [eof]), 'a')), 
    eq_(eval(L(parse, [epsilon], '')), True)
  def testepsilon3(self):
    rule = (and_, (char, 'a'), [epsilon], (char, 'b'))
    eq_(eval(L(parse, rule, 'ab')), True)
    assert_raises(UnifyFail, eval, L(parse, rule, 'a b'))
    assert_raises(UnifyFail, eval, L(parse, rule, 'a'))
    
  def test_number(self):
    x, y, z = Var('y'), Var('x'), Var('z')
    eq_(eval(L(begin, (parse, (number, x), '2'), x)), Integer(2))
    eq_(eval(L(begin, (parse, (number, y), '234'), y)), Integer(234))
    eq_(eval(L(begin, (parse, (number, z), '0232'), z)), Integer(154)) #0ctal
    
  def test_literal(self):
    eq_(eval(L(parse, (literal, 'if'), 'if')), True)
    assert_raises(UnifyFail, eval, L(parse, (literal, 'if'), 'ssf'))

  def test_string(self):
    x ,y  = Var('x'), Var('y')
    eq_(eval(L(begin, (parse, (dqstring, x), '"2"'), x)), String("2"))
    eq_(eval(L(begin, (parse, (sqstring, y), "'1'"), y)), String("1"))

  def test_spaces(self):
    x ,y  = Var('x'), Var('y')
    eq_(eval(L(begin, (parse, (spaces, x), ' '), x)), atom(" "))
    eq_(eval(L(begin, (parse, (spaces, y), "\r\t\n"), y)), atom("\r\t\n"))

  def test_uLetterdigitString(self):
    x ,y  = Var('x'), Var('y')
    eq_(eval(L(begin, (parse, (uLetterdigitString, x), '_ad123'), x)), atom("_ad123"))
    eq_(eval(L(begin, (parse, (uLetterdigitString, y), "ad1sff23"), y)), atom("ad1sff23"))
    
class Testrule:
  def test_two_rule(self):
    eq_(eval(L(letrec, [(f, (function, ((), (char, 'a')),((), (char, 'b'))))], 
               (parse, [f], 'a'), (parse, [f],  'b'))), 
        True)
    
  def test_right_recursive1(self):
    function1 = (function, ([], (and_, (char, 'a'), [f])),
                     ([], (char, 'b')))
    eq_(eval(L(letrec, [(f, function1)], 
               (parse, [f], 'b'), (parse, [f],  'ab'), (parse, [f],  'aab'))), 
        True)
    assert_raises(UnifyFail, eval, L(letrec, [(f, function1)], (parse, [f], 'a')))
    
  def test_right_recursive2(self):
    x, p = Var('x'), Var('p')
    function1 = [(p, (function, ([], (and_, (char, x), [p])),
                     ([], (char,x))))]
    eq_(eval(L(letrec, function1, 
               (parse, [p], 'abc'), (parse, [p],  'ab'), (parse, [p],  'a'))), 
        True)
    assert_raises(UnifyFail, eval, L(letrec, function1, (parse, [p], '')))

  def test_unify_right_recursive(self):
    x, p = Var('x'), Var('p')
    function1 = [(p, (function, ([x], (and_, (char, x), [p, x])),
                          ([x], (char,x))))]
    eq_(eval(L(letrec, function1, (parse, [p, x], 'aa'))), True)
    eq_(eval(L(letrec, function1, (parse, [p, x], 'a'))), True)
    assert_raises(UnifyFail, eval, 
      L(letrec, function1, (parse, (and_, [p, x], [eof]), 'xy')))
    assert_raises(UnifyFail, eval, L(letrec, function1, (parse, [p, x], '')))
  def xxxtest_left_recursive(self):
    assert 0, 'a good idea is waiting to be implemented'
    engine = Engine()
    X = Var()
    f = userTerm("f")
    engine.add_rule(rule(f(X), f(X)+char('1')))
    engine.add_rule(rule(f(X), char('2')))
    X = Var()
    engine.run(f(X), '21')
    engine.run(f(X), '2')
    assert_raises(UnifyFail, engine.run, f(X), '1')
      
class TestOr:
  def testOr(self):
    x, s, one, two = Var('x'), Var('s'), Var('one'), Var('two')
    ruleList = [(s, (function, ([x], (or_, (one, x), (two, x))))),
                (one, (function, (['1'], (char, '1')))),
                (two, (function, (['2'], (char, '2'))))]
    eq_(eval(L(letrec, ruleList, (parse, [s, x], '1'))), True)
    eq_(eval(L(letrec, ruleList, (parse, [s, y],  '1'))), True)         
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, [s, x], '3')))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, [and_, [s, x], [eof]], '12')))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, [s, x], '')))

class TestOptional:
  def test_optional(self):
    x = Var('x')
    eq_(eval(L(parse, [optional, [char, x]], '1')), True)
    eq_(eval(L(parse, [optional, [char, x]], '')), True)

  def testoptionalRule(self):
    x, s = Var('x'), Var('s')
    ruleList = [(s, (function, ([x], (and_, (optional, (char, 'a')), (char, x)))))]
    eq_(eval(L(let, ruleList, (parse, [s, x], 'a'), x)), atom('a'))
    eq_(eval(L(let, ruleList, (parse, [s, x],  'aa'), x)), atom('a'))         
    eq_(eval(L(let, ruleList, (parse, [s, x],  'b'), x)), atom('b'))         
    
  def testoptionalcut(self):
    x, s = Var('x'), Var('s')
    ruleList = [(s, (function, ([x], (and_, (optional, (char, 'a')), [cut], (char, x)))))]
    eq_(eval(L(let, ruleList, (parse, [s, x],  'aa'), x)), atom('a'))         
    eq_(eval(L(let, ruleList, (parse, [s, x],  'b'), x)), atom('b')) 
    assert_raises(UnifyFail, eval, L(let, ruleList, (parse, [s, x], 'a'), x))
    
class TestParallel:
  def test_parallel(self):
    x = Var('x')
    eq_(eval(L(parse, [parallel, [letter, x], [char, x]], 'a')), True)
    assert_raises(UnifyFail, eval, L(parse, [parallel, [number, x], [char, '3']], '2'))
  def testparallelRule(self):
    x, s, gt, lt = Var('x'), Var('s'), Var('>'), Var('<')
    ruleList = [(s, (function, ([x], (parallel, (gt, x, 3), (lt, x, 5))))),
                (gt, (function, ([4, 3], (char, '4')))),
                (lt, (function, ([4, 5], (char, '4'))))]
    eq_(eval(L(letrec, ruleList, (parse, [s, x], '4'), x)), Integer(4))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, [s, x], '6'), x))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, [and_, [s, x], [eof]], '41'), x))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, [s, x], ''), x))
        
class TestAnySomeTimesSepList:
  def test_any(self):
    X, Y = Var('X'), Var('Y')
    eq_(eval(L(begin, (parse, [any, (char,X), X, Y], '222'), Y)), L('2','2','2'))
    eq_(eval(L(begin, (parse, [any, (char,X), X, Y], '234'), Y)), L('2'))
    eq_(eval(L(begin, (parse, [any, (char,X), X, Y], ''), Y)), L())
  def test_any2(self):
    eq_(eval(L(parse, (any, (or_, (char, '1'), (char, '2'))), '2')), True)
  def test_anydummy(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(L(begin, (parse, [any, (char,_), _, Y], '222'), Y)), L('2','2','2'))
    eq_(eval(L(begin, (parse, [any, (char,_), _, Y], '234'), Y)), L('2', '3', '4'))
    eq_(eval(L(begin, (parse, [any, (char,_), _, Y], ''), Y)), L())
    
  def test_some(self):
    X, Y = Var('X'), Var('Y')
    eq_(eval(L(begin, (parse, [some, (char,X), X, Y], '222'), Y)), L('2','2','2'))
    eq_(eval(L(begin, (parse, [some, (char,X), X, Y], '234'), Y)), L('2'))
    assert_raises(UnifyFail, eval, L(begin, (parse, [some, (char,X), X, Y], ''), Y))
  def test_some2(self):
    X = Var('X')
    eq_(eval(L(parse, (and_, (some, (char, X)), (char, '4')), '224')), True)
    eq_(eval(L(parse, (and_, (some, (char, X)), [cut], (char, '4')), '224')), True)
    assert_raises(UnifyFail, eval, L(parse, (and_, (some, (char, X)), (char, '3'), (char, '5')), '2234'))
  def test_some3(self):
    eq_(eval(L(parse, (some, (or_, (char, '1'), (char, '2'))), '2')), True)
  def test_somedumy(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(L(begin, (parse, [some, (char,_), _, Y], '222'), Y)), L('2','2','2'))
    eq_(eval(L(begin, (parse, [some, (char,_), _, Y], '234'), Y)), L('2', '3', '4'))
    assert_raises(UnifyFail, eval, L(begin, (parse, [some, (char,_), _, Y], ''), Y))
    
  def test_times(self):
    X, Y = Var('X'), Var('Y')
    eq_(eval(L(begin, (parse, (times, (char, X), 3, X, Y), '222'), Y)), L('2','2','2'))
    assert_raises(UnifyFail, eval, L(parse, (times, (char, X), 3, X, Y), '2234'))
  def test_timesdummy(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(L(begin, (parse, (times, (char, _), 3, _, Y), '234'), Y)), L('2','3','4'))
  def testtimes_a2(self): 
    X, Y, S = Var('X'), Var('Y'), Var('S')
    function1 = L(function, ([Y], [times, (char, 'a'), 2, 'a', Y]))
    eq_(eval(L(begin, (parse, (function1, X), 'aa'), X)), L('a', 'a'))
    assert_raises(UnifyFail, eval, L(begin, (parse, (function1, X), 'a'), X))
    assert_raises(UnifyFail, eval, L(begin, (parse, (and_, (function1, X), [eof]), 'aaa'), X))
  def testtimes_an(self): 
    X, Y, S, n = Var('X'), Var('Y'), Var('S'), Var('n')
    function1 = L(function, ([Y], [times, (char, 'a'), n, 'a', Y]))
    eq_(eval(L(begin, (parse, (function1, X), 'a'), X)), L('a'))
    eq_(eval(L(begin, (parse, (function1, X), 'aa'), X)), L('a', 'a'))
    eq_(eval(L(begin, (parse, (function1, X), 'aaa'), X)), L('a', 'a', 'a'))
    
  def test_seplist(self):
    X, Y = Var('X'), Var('Y')
    eq_(eval(L(begin, (parse, [seplist, (char,X), (char, ','), X, Y], '2,2,2'), Y)), L('2','2','2'))
    eq_(eval(L(begin, (parse, [seplist, (char,X), (char, ','), X, Y], '2,3,4'), Y)), L('2'))
    eq_(eval(L(begin, (parse, [seplist, (char,X), (char, ','), X, Y], '2'), Y)), L('2'))
    #assert_raises(UnifyFail, eval, L(begin, (parse, [seplist, (char, X), (char, ','), X, Y], ''), Y))
  def test_seplistdummy(self):
    _, Y = DummyVar('_'), Var('Y')
    eq_(eval(L(begin, (parse, [seplist, (char, _), (char, ','), _, Y], '2,2,2'), Y)), L('2','2','2'))
    eq_(eval(L(begin, (parse, [seplist, (char,_), (char, ','), _, Y], '2,3,4'), Y)), L('2', '3', '4'))
    eq_(eval(L(begin, (parse, [seplist, (char,_), (char, ','), _, Y], '2'), Y)), L('2'))
    
class TestKleeneByfunction:  
  def testKleene1(self): #occurs_check
    x, s, kleene = Var('x'), Var('s'), Var('kleene')
    ruleList = [(s, (function, ([x], (kleene, x)))),
                (kleene, (function, 
                  ([('a', x)], (and_, (char, 'a'), (kleene, x))),
                  ([''], [epsilon])))]
    eq_(eval(L(letrec, ruleList, (parse, [s, x], 'aa'), x)), L('a', L('a', '')))
    eq_(eval(L(letrec, ruleList, (parse, [s, x], ''), x)), atom(''))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, (and_, [s, x], [eof]), '6'), x))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, [and_, [s, x], [eof]], '41'), x))

  def testKleene2(self):
    x, c, s, kleene = Var('x'), Var('c'), Var('s'), Var('kleene')
    ruleList = [(s, (function, ([x], (kleene, c, x)))),
                (kleene, (function, 
                  ([c, (c, x)], (and_, (char, c), (kleene, c, x))),
                  ([c, ''], [epsilon])))]
    eq_(eval(L(letrec, ruleList, (parse, [s, x], 'aa'), x)), L('a', L('a', '')))
    eq_(eval(L(letrec, ruleList, (parse, [s, x], 'aaa'), x)), L('a', L('a', L('a', ''))))
    eq_(eval(L(letrec, ruleList, (parse, [s, x], 'bbb'), x)), L('b', L('b', L('b', ''))))
    eq_(eval(L(letrec, ruleList, (parse, [s, x], ''), x)), atom(''))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, (and_, [s, x], [eof]), 'aab'), x))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, (and_, [s, x], [eof]), 'abc'), x))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, [and_, [s, x], [eof]], '41'), x))

  def testKleene3(self):
    x, _, c, s, kleene = Var('x'), DummyVar('_'), Var('c'), Var('s'), Var('kleene')
    ruleList = [(s, (function, ([x], (kleene, _, x)))),
                (kleene, (function, 
                  ([_, (c, x)], (and_, (char, c), (kleene, _, x))),
                  ([_, ''], [epsilon])))]
    eq_(eval(L(letrec, ruleList, (parse, [s, x], 'aa'), x)), L('a', L('a', '')))
    eq_(eval(L(letrec, ruleList, (parse, [s, x], 'aaa'), x)), L('a', L('a', L('a', ''))))
    eq_(eval(L(letrec, ruleList, (parse, [s, x], 'bbb'), x)), L('b', L('b', L('b', ''))))
    eq_(eval(L(letrec, ruleList, (parse, [s, x], 'abc'), x)), L('a', L('b', L('c', ''))))
    eq_(eval(L(letrec, ruleList, (parse, [s, x], ''), x)), atom(''))

class testIndentUndent:
  def testIndentUndent(self):
    _, n, s, line = DummyVar('_'), Var('n'), Var('s'), Var('line')
    space = [char, ' ']
    ruleList = [(s, (function, ([n], (and_, (some, (line, n)), (s, (add, n, 1)))),
                            ([n], (some, (line, n))))),
                (line, (function, ([n], (and_, (times, space, n), (some, (letter, _)), (any, space),  [char, '\n']))))]
    eq_(eval(L(letrec, ruleList, (parse, [s, 0],  'a\n b\n c\n'))), True)
    eq_(eval(L(letrec, ruleList, (parse, [s, 0],  'asd\n bdf\n cdfh\n'))), True)

class TestExpression:          
  def testExpressionByRightRecursiveList(self):
    E, e, e1, e2 = Var('E'), Var('e'), Var('e1'), Var('e2')  
    ruleList = [(E, (function, 
                     ([L(e1, '/', e2), 1], (and_, (E, e1, 2), (char, '/'), (E, e2, 1))),
                     ([1, 2], (char, '1')),
                     ([e, 1], (E, e, 2))))]
    eq_(eval(L(letrec, ruleList, (parse, [E, e, 1],  '1/1/1'), e)), L(1, '/', (1, '/', 1)))
    eq_(eval(L(letrec, ruleList, (parse, [E, e, 1],  '1/1'), e)), L(1, '/', 1))
    eq_(eval(L(letrec, ruleList, (parse, [E, e, 1],  '1'), e)), Integer(1))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, [and_, [E, e, 1], [eof]], '1+1/1'), e))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, [and_, [E, e, 1], [eof]], '2'), e))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, [and_, [E, e, 1], [eof]], '1/'), e))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, [and_, [E, e, 1], [eof]], '/'), e))
    assert_raises(UnifyFail, eval, L(letrec, ruleList, (parse, [and_, [E, e, 1], [eof]], ''), e))
