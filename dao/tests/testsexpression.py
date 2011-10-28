from nose.tools import eq_, ok_, assert_raises
from dao.builtins.arith import add, sub, eq

from dao.term import nil
from dao.term import conslist as L
from dao.rule import Rule
from dao.builtins.terminal import spaces0, char
##from dao.sexpression import sexpression, number, sexpressionList
##from dao.sexpression import grammar, grammar1, grammar2, grammar3
##from dao.parse import parse, eval

class xTestParse:
  def testgrammar1_Digits(self):
    eq_(parse(grammar1, '1'), Integer(1))
    eq_(parse(grammar1, '12'), Integer(12))
    
  def testgrammar2_sexprlist(self):
    eq_(parse(grammar2, '1 a'), L(1, var('a')))
    assert_raises(UnifyFail, parse, grammar2, '1a')
    eq_(parse(grammar2, ''), NIL)
    
  def testDigitSexpression(self):
    eq_(parse(grammar, '1'), Integer(1))
    eq_(parse(grammar, '12'), Integer(12))
  def testString(self):
    eq_(parse(grammar, '"1"'), String('1'))
    eq_(parse(grammar, '"12"'), String('12'))
  def testSymbol(self):
    eq_(parse(grammar, '!@#').name, var('!@#').name)
  def testquote(self):
    eq_(parse(grammar, "'@"), L(Symbol('quote'), var('@')))
  def testquasi(self):
    eq_(parse(grammar, "`a"), L('quasiquote', var('a')))
    eq_(parse(grammar, ",@a"), L('unquote-splicing', var('a')))
    eq_(parse(grammar, "`(1 2)"), L('quasiquote', (1, 2)))
    eq_(parse(grammar, "(1 3 (a) () `(1 2))"), L(1,3,[var('a')], NIL,('quasiquote', (1,2))))
  def testslist(self):
    eq_(parse(grammar, '(1)'), L(Integer(1)))
    eq_(parse(grammar, '("12")'), L(String('12')))
  def testif(self):
    eq_(parse(grammar, '((if 0 + -) 1 1)'), L(('if',0, add, sub),1,1))
  def testsexprlist2(self):
    eq_(parse(grammar2, 'if 0 2 3'), L(Symbol('if'), 0, 2, 3))
  def testletr(self):
    odd, even, n = var('odd?'), var('even?'), var('n')
    eq_(parse(grammar2, '''(letr ((odd? (lambda (n) (if (== n 0) 0 (even? (- n 1)))))
                    (even? (lambda (n) (if (== n 0) 1 (odd? (- n 1))))))
                  (odd? 3))'''),  
          L(('letr',
                               ((odd,('lambda',[n],('if',(eq,n,0),0,(even,(sub,n,1))))),
                              (even,('lambda',[n],('if',(eq,n,0),1,(odd,(sub,n,1)))))),
                               (odd,3))))

class Test_eval_while_parsing:
  def testDigitSexpression2(self):
    eq_(parse(grammar, '{1}'), Integer(1))
  def testif(self):
    eq_(parse(grammar, '{((if 0 + -) 1 1)}'), Integer(0))
  def testlet(self):
    eq_(parse(grammar, '{(let ((a 1)) a)}'), Integer(1))
    #eq_(parse(grammar, '{(let ((a 1)) {(+ a 1)})}'), Integer(2))
  def testbegin(self):
    eq_(parse(grammar, '({begin} 1)'), L('begin', 1))
    eq_(parse(grammar, '({begin} {(+ 1 1)})'), L('begin', 2))
  def testchar(self):
    eq_(parse(grammar, '{(char "a")}a'), True) # beautiful, so cool, eureka!
  def testlet_char_rule(self):
    eq_(parse(grammar, '{(let ((f (lambda () (char x)))) (f))}a'), True) 
  def testlet_char_rule(self):
    eq_(parse(grammar, '{(let ((f (macro ((x) (char x))))) (f x) x)}a'), Symbol('a')) 

class Test_eval_by_parse:
  def testString(self):
    eq_(parse(grammar3, '1'), Integer(1))
    eq_(parse(grammar3, '12'), Integer(12))
    eq_(parse(grammar3, '"12"'), String("12"))
    eq_(parse(grammar3, '"1"'), String("1"))
  def testArithmetic(self):
    eq_(parse(grammar3, '(+ 1 1)'), Integer(2))
    eq_(parse(grammar3, '(- 1 1)'), Integer(0))
    eq_(parse(grammar3, '(* 2 2)'), Integer(4))
    eq_(parse(grammar3, '(/ 2 2)'), Integer(1))
  def testbegin(self):
    eq_(parse(grammar3, '(begin 1 2 3 4 5)'), Integer(5))
  def testspecialForm(self):
    eq_(parse(grammar3, '(let ((x 1) (y 2)) (+ x y))'), Integer(3))
    eq_(parse(grammar3, '((if 0 + -) 1 1)'), Integer(0))
    eq_(parse(grammar3, '((if 1 + -) 1 1)'), Integer(2))
  def testlambda(self):
    eq_(parse(grammar3, '((lambda (x y) (+ x y)) 1 1)'), Integer(2))
  def testlambda2(self):
    eq_(parse(grammar3, '((lambda (x) x) 2)'), Integer(2))
  def testletr(self):
    eq_(parse(grammar3, 
        '''(letr ((fac (lambda (n) (if (== n 1) 1 (* n (fac (- n 1)))))))
                  (fac 3))'''), Integer(6))
  def testletr2(self):
    eq_(parse(grammar3, '''(letr ((odd? (lambda (n) (if (== n 0) 0 (even? (- n 1)))))
                    (even? (lambda (n) (if (== n 0) 1 (odd? (- n 1))))))
                  (odd? 3))'''), Integer(1))
  def testcallcc(self):
    ##from dao.cont import done
    eq_(parse(grammar3, '(call/cc (lambda (k) (k 2)))'), Integer(2))
    #eq_(parse(grammar3, '(call/cc call/cc)'), done)
    #assert 0, '((call/cc call/cc) (call/cc call/cc)) infinite loop.'
    #eq_(parse(grammar3, '((call/cc call/cc) (call/cc call/cc))'), Integer(2))
  def testeval(self):
    eq_(parse(grammar3, "(eval '1)"), Integer(1))
    eq_(parse(grammar3, "(eval '(+ 1 1))"), Integer(2))
    
class Testeval:
  def testString(self):
    eq_(eval(grammar, '"1"'), String("1"))
    eq_(eval(grammar, '1'), Integer(1))
    eq_(eval(grammar, '12'), Integer(12))
    eq_(eval(grammar, '"12"'), String("12"))
  def testbegin(self):
    eq_(eval(grammar, '(begin 1 2 3 4 5)'), Integer(5))
  def testset(self):
    eq_(eval(grammar, '(let ((a 1)) (begin (set a 2) a))'), Integer(2))
  def testArithmetic(self):
    eq_(eval(grammar, '(+ 1 1)'), Integer(2))
    eq_(eval(grammar, '(- 1 1)'), Integer(0))
    eq_(eval(grammar, '(* 2 2)'), Integer(4))
    eq_(eval(grammar, '(/ 2 2)'), Integer(1))
  def testspecialForm(self):
    eq_(eval(grammar, '(let ((x 1) (y 2)) (+ x y))'), Integer(3))
    eq_(eval(grammar, '((if 0 + -) 1 1)'), Integer(0))
    eq_(eval(grammar, '((if 1 + -) 1 1)'), Integer(2))
  def testlambda(self):
    eq_(eval(grammar, '((lambda (x) x) 2)'), Integer(2))
    eq_(eval(grammar, '((lambda (x y) (+ x y)) 1 1)'), Integer(2))
  def testletr(self):
    eq_(eval(grammar, 
        '''(letr ((fac (lambda (n) (if (== n 1) 1 (* n (fac (- n 1)))))))
                  (fac 3))'''), Integer(6))
  def testletr2(self):
    eq_(eval(grammar, '''(letr ((odd? (lambda (n) (if (== n 0) 0 (even? (- n 1)))))
                    (even? (lambda (n) (if (== n 0) 1 (odd? (- n 1))))))
                  (odd? 3))'''), Integer(1))
  def testcallcc(self):
    ##from dao.cont import done
    eq_(eval(grammar, '(call/cc (lambda (k) (k 2)))'), Integer(2))
    #eq_(eval(grammar, '(call/cc call/cc)'), done)
    #assert 0, '((call/cc call/cc) (call/cc call/cc)) infinite loop.'
    #eq_(eval(grammar, '((call/cc call/cc) (call/cc call/cc))'), Integer(2))
  def testcatch(self):
    eq_(eval(grammar, '(catch 1 2)'), Integer(2))
    eq_(eval(grammar, '(catch 1 (throw 1 2))'), Integer(2))
  def testblock(self):
    eq_(eval(grammar, "(block foo (let ((f (lambda () (return-from foo  1)))) (* 2 (block foo (f)))))"), 
        Integer(1))
    eq_(eval(grammar, "(block a (return-from a 2) 3)"), Integer(2))
  def test_unwind_protect(self):
    eq_(eval(grammar, "(block foo (unwind-protect (return-from foo 2) 2))"), Integer(2))
  def testeval(self):
    eq_(eval(grammar, "(eval '1)"), Integer(1))
    eq_(eval(grammar, "(eval '(+ 1 1))"), Integer(2))