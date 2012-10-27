from nose.tools import eq_, ok_, assert_raises
from samples.sexpression import *
from samples import sexpression
from dao.solve import NoSolutionFound

def parse(grammar, text):
  return cons2tuple(sexpression.parse(grammar, text))

class TestParse:
  def testgrammar1_Digits(self):
    eq_(parse(grammar1, '1'), 1)
    eq_(parse(grammar1, '12'), 12)
    
  def testgrammar2_sexprlist(self):
    #assert_raises(NoSolutionFound, parse, grammar2, '1a')
    eq_(parse(grammar2, '1 a'), (1, var('a')))
    eq_(parse(grammar2, ''), nil)
    
  def testDigitSexpression(self):
    eq_(parse(grammar, '1'), 1)
    eq_(parse(grammar, '12'), 12)
  def testString(self):
    eq_(parse(grammar, '"1"'), '1')
    eq_(parse(grammar, '"12"'), '12')
  def testSymbol(self):
    eq_(parse(grammar, '!@#'), var('!@#'))
  def testquote(self):
    eq_(parse(grammar, "'@"), (quote, var('@')))
  def testquasi(self):
    eq_(parse(grammar, "`(1 2)"), (quasiquote, (1, 2)))
    eq_(parse(grammar, "`a"), (quasiquote, var('a')))
    eq_(parse(grammar, ",@a"), (unquote_splice, var('a')))
  def testquasi2(self):
    eq_(parse(grammar, "(1 3 (a) () `(1 2))"), (1,3,(var('a'),), nil,(quasiquote, (1,2))))
    eq_(parse(grammar, "(1 3 (a) () `'(1 2))"), (1,3,(var('a'),), nil,(quasiquote, (quote, (1,2)))))
  def testslist(self):
    eq_(parse(grammar, '(1)'), (1,))
    eq_(parse(grammar, '("12")'), ('12',))
  def testif(self):
    eq_(parse(grammar, '((if 0 + -) 1 1)'), ((if_,0, add, sub),1,1))
  def testsexprlist2(self):
    eq_(parse(grammar2, 'if 0 2 3'), (if_, 0, 2, 3))
  def testletr(self):
    odd, even, n = var('odd?'), var('even?'), var('n')
    eq_(parse(grammar2, '''(letr ((odd? (lambda (n) (if (== n 0) 0 (even? (- n 1)))))
                    (even? (lambda (n) (if (== n 0) 1 (odd? (- n 1))))))
                  (odd? 3))'''),  
          ((letr,
                               ((odd,(lambda_,(n,),(if_,(eq,n,0),0,(even,(sub,n,1))))),
                              (even,(lambda_,(n,),(if_,(eq,n,0),1,(odd,(sub,n,1)))))),
                               (odd,3)),))

class Testeval:
  def testString(self):
    eq_(eval(grammar, '"1"'), "1")
    eq_(eval(grammar, '1'), 1)
    eq_(eval(grammar, '12'), 12)
    eq_(eval(grammar, '"12"'), "12")
  def testArithmetic(self):
    eq_(eval(grammar, '(/ 2 2)'), 1)
    eq_(eval(grammar, '(+ 1 1)'), 2)
    eq_(eval(grammar, '(- 1 1)'), 0)
    eq_(eval(grammar, '(* 2 2)'), 4)
  def testbegin(self):
    eq_(eval(grammar, '(begin 1 2 3 4 5)'), (5))
  def testset(self):
    eq_(eval(grammar, '(let ((a 1)) (begin (set a 2) a))'), (2))
  def test_let(self):
    eq_(eval(grammar, '(let ((x 1) (y 2)) (+ x y))'), (3))
  def testspecialForm2(self):
    eq_(eval(grammar, '((if 0 + -) 1 1)'), (0))
    eq_(eval(grammar, '((if 1 + -) 1 1)'), (2))
  def testlambda1(self):
    eq_(eval(grammar, '((lambda (x) x) 2)'), (2))
  def testlambda2(self):
    eq_(eval(grammar, '((lambda (x y) (+ x y)) 1 1)'), (2))
  def testletr(self):
    eq_(eval(grammar, 
        '''(letr ((fac (lambda (n) (if (== n 1) 1 (* n (fac (- n 1)))))))
                  (fac 3))'''), (6))
  def testletr2(self):
    eq_(eval(grammar, '''(letr ((odd? (lambda (n) (if (== n 0) 0 (even? (- n 1)))))
                    (even? (lambda (n) (if (== n 0) 1 (odd? (- n 1))))))
                  (odd? 3))'''), (1))
  def testcallcc(self):
    eq_(eval(grammar, '(call/cc (lambda (k) (k 2)))'), (2))
  def testcatch(self):
    eq_(eval(grammar, '(catch 1 2)'), (2))
    eq_(eval(grammar, '(catch 1 (throw 1 2))'), (2))
  def testblock1(self):
    eq_(eval(grammar, "(block a (return-from a 2) 3)"), (2))
  def testblock2(self):
    eq_(eval(grammar, "(block foo (let ((f (lambda () (return-from foo  1)))) (* 2 (block foo (f)))))"), 
        (1))
  def test_unwind_protect(self):
    eq_(eval(grammar, "(block foo (unwind-protect (return-from foo 2) 2))"), 2)
  def testeval(self):
    eq_(eval(grammar, "(eval '1)"), 1)
    eq_(eval(grammar, "(eval '(+ 1 1))"), (2))
    
class Test_eval_by_parse:
  def testString(self):
    eq_(parse(grammar3, '1'), 1)
    eq_(parse(grammar3, '12'), 12)
    eq_(parse(grammar3, '"12"'), "12")
    eq_(parse(grammar3, '"1"'), "1")
  def testArithmetic(self):
    eq_(parse(grammar3, '(+ 1 1)'), (2))
    eq_(parse(grammar3, '(- 1 1)'), (0))
    eq_(parse(grammar3, '(* 2 2)'), (4))
    eq_(parse(grammar3, '(/ 2 2)'), (1))
  def testbegin(self):
    eq_(parse(grammar3, '(begin 1 2 3 4 5)'), (5))
  def testspecialForm(self):
    eq_(parse(grammar3, '(let ((x 1) (y 2)) (+ x y))'), (3))
    eq_(parse(grammar3, '((if 0 + -) 1 1)'), (0))
    eq_(parse(grammar3, '((if 1 + -) 1 1)'), (2))
  def testlambda(self):
    eq_(parse(grammar3, '((lambda (x y) (+ x y)) 1 1)'), (2))
  def testlambda2(self):
    eq_(parse(grammar3, '((lambda (x) x) 2)'), (2))
  def testletr(self):
    eq_(parse(grammar3, 
        '''(letr ((fac (lambda (n) (if (== n 1) 1 (* n (fac (- n 1)))))))
                  (fac 3))'''), (6))
  def testletr2(self):
    eq_(parse(grammar3, '''(letr ((odd? (lambda (n) (if (== n 0) 0 (even? (- n 1)))))
                    (even? (lambda (n) (if (== n 0) 1 (odd? (- n 1))))))
                  (odd? 3))'''), (1))
  def testcallcc(self):
    ##from dao.cont import done
    eq_(parse(grammar3, '(call/cc (lambda (k) (k 2)))'), (2))
    #eq_(parse(grammar3, '(call/cc call/cc)'), done)
    #assert 0, '((call/cc call/cc) (call/cc call/cc)) infinite loop.'
    #eq_(parse(grammar3, '((call/cc call/cc) (call/cc call/cc))'), (2))
  def testeval(self):
    eq_(parse(grammar3, "(eval '1)"), (1))
    eq_(parse(grammar3, "(eval '(+ 1 1))"), (2))
    
class Test_eval_while_parsing:
  def testDigitSexpression2(self):
    eq_(parse(grammar, '{1}'), 1)
  def testif(self):
    eq_(parse(grammar, '{((if 0 + -) 1 1)}'), 0)
  def testlet1(self):
    eq_(parse(grammar, '{(let ((a 1)) a)}'), 1)
  def testlet2(self):
    eq_(parse(grammar, '{(let ((a 1)) {(+ a 1)})}'), 2)
  def testbegin1(self):
    eq_(parse(grammar, '({begin} 1)'), (begin, 1))
  def testbegin2(self):
    eq_(parse(grammar, '({begin} {(+ 1 1)})'), (begin, 2))
  def testbegin3(self):
    eq_(parse(grammar, '{({begin} {(+ 1 1)})}'), 2)
  def testchar(self):
    eq_(parse(grammar, '{(char "a")}a'), 'a') # beautiful, so cool, eureka!
  def testlet_char_rule1(self):
    eq_(parse(grammar, '{(let ((f (lambda () (char x)))) (f))}a'), 'a') 
  def testlet_char_rule2(self):
    eq_(parse(grammar, '{(let ((f (macro ((x) (char x))))) (f x) x)}a'), 'a') 

