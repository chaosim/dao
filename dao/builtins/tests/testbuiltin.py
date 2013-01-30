from nose.tools import eq_, assert_raises

#from dao.term import Var, conslist as L
from dao.solve import eval
from dao.solvebase import NoSolution

from dao.builtins import Var
from dao.command import LogicVar, Const
from dao.builtins import rules
from dao.builtins import begin, let, quote, assign
from dao.builtins import eq, sub, mul, add, div
from dao.builtins import succeed, fail, or_, and_, not_p, cut_or, repeat, first_p
from dao.builtins import findall, once#, call, once
from dao.builtins import set_text
from dao.builtins import char
from dao.builtins import unify, is_#, notunify
from dao.builtins import prin, format, open_file, read, readline, readlines, write, close_file
#from dao.builtins.term import ground_p
#from dao.builtins.term import isvar, nonvar, is_, define, nonvar_p, isvar_p

from dao.command import Cons, conslist as L

from dao.tests.util import *

class TestControl:
  def test_fail(self):
    x = LogicVar('x')
    eq_(eval(let([(f,rules([[1], fail], [[x], succeed]))], f(x))), True)

  def test_succeed(self):
    x = LogicVar('x')
    eq_(eval(let([(f,rules([[1], succeed]))], f(x))), True)

  def test_or(self):
    eq_(eval(or_(fail, succeed)), True)
    
  def test_or2(self):
    x = LogicVar('x')
    eq_(eval(or_(begin(unify(x, 1), unify(x, 2)), 
                 unify(x, 2))), True)
    
  def test_cut_or(self):
    eq_(eval(or_(begin(prin(1), fail), prin(2))), None)
    
  def test_cut_or2(self):
    assert_raises(NoSolution, eval, or_(begin(prin(1), cut_or, fail), 
                                        prin(2)))
    
  def test_cut_or3(self):
    x = LogicVar('x')
    assert_raises(NoSolution, eval, 
                  or_(begin(unify(x, 1), cut_or, unify(x, 2)), 
                      unify(x, 2)))
    
  def test_cut_or4(self):
    x = LogicVar('x')
    eq_(eval(or_(or_(begin(unify(x, 1), cut_or, unify(x, 2)), 
                      unify(x, 2)),
                 unify(x, 2))), True)
    
  def test_and(self):
    eq_(eval(and_(succeed, succeed)), True)
    
  def test_and2(self):
    eq_(eval(and_(prin(1), prin(2))), None)
    
  def test_and3(self):
    assert_raises(NoSolution, eval, and_(succeed, fail))
    
  def test_not_p(self):
    eq_(eval(not_p(fail)), True)
    assert_raises(NoSolution, eval, not_p(succeed))
    
  def test_repeat(self):
    #return
    # the code below loops for ever, after modifie the behaviour of solver.parse_state and terminals.
    x = LogicVar('x')
    eq_(eval(and_(set_text('123'), repeat, char(x), unify(x, '3'))), True)
    
  def test_repeat2(self):
    #return
    # the code below loops for ever.
    x = LogicVar('x')
    eq_(eval(and_(set_text('123'), repeat, char(x), unify(x, '4'))), True) 
    
  def test_if_p(self):
    from dao.builtins.control import if_p
    eq_(eval(if_p(succeed, succeed)), True)
    assert_raises(NoSolution, eval, if_p(succeed, fail))
    # This below unusual semantics is part of the ISO and all de-facto Prolog standards.
    # see SWI-Prolog help.
    assert_raises(NoSolution, eval, if_p(fail, succeed))
    assert_raises(NoSolution, eval, if_p(fail, fail))

class TestIO:
  def test_format(self):
    eq_(eval(format('%s', 1)), '1')
    eq_(eval(format('%s%s', 1, 2)), '12')
    
  def test_open_read(self):
    file1 = Var('file1')
    eq_(eval(let([(file1, open_file('test.txt'))], 
                 assign(x, read(file1)), 
                 close_file(file1),
                 x)), 'hello\nhello')
    
  def test_open_read2(self):
    file1 = Var('file1')
    x = Var('x')
    eq_(eval(let([(file1, open_file('test.txt'))], 
                 readline(file1), 
                 assign(x, readlines(file1)), 
                 close_file(file1), 
                 x)), 
        ['hello'])
    
  def test_write(self):
    file1 = Var('file1')
    eq_(eval(let([(file1, open_file('test2.txt', 'w'))], 
                 write(file1, 'test'), close_file(file1))), None)
    
from dao.builtins import eq, le, ne, and_a, between 

class TestArithpred:
  def test_eq_le_ne(self):
    eq_(eval(and_a(le(1, 1), ne(1, 2))), True)
    eq_(eval(eq(1, 1)), True)
    eq_(eval(le(1, 1)), True)
    
  def test_between(self):
    eq_(eval(between(1, 3, 2)), True)
    
  def test_between2(self):
    x = LogicVar('x')
    eq_(eval(between(1, 3, x)), True)
    
  def test_between3(self):
    x = LogicVar('x')
    y = LogicVar('y')
    eq_(eval(begin(findall(between(1, 3, x), x, y), getvalue(y))), [1, 2, 3])

class XTestTypePredicate:
  def test_ground(self):
    eq_(eval(ground_p(1)), True)
    assert_raises(NoSolution, eval, ground_p(Var('')))
    
  def test_var(self):
    eq_(eval(isvar(1)), False)
    eq_(eval(isvar(L(1))), False)
    eq_(eval(nonvar(1)), True)
    eq_(eval(nonvar(L(1))), True)
    eq_(eval(isvar(x)), True)
    eq_(eval(nonvar_p(1)), True)
    eq_(eval(nonvar_p(L(1))), True)
    eq_(eval(isvar_p(x)), True)
    assert_raises(NoSolution, eval, isvar_p(1))
    assert_raises(NoSolution, eval, isvar_p(L(1)))
    assert_raises(NoSolution, eval, nonvar_p(x))

from dao.builtins import notunify

class Testunify:
  def test1(self):
    x = LogicVar('x')
    eq_(eval(unify(x, 1)), True)
    
  def test2(self):
    x = Var('x')
    Lx = LogicVar('x')
    eq_(eval(unify(1, 1)), True)
    eq_(eval(begin(unify(1, 1), unify(2, 2))), True)
    eq_(eval(begin(unify(Lx, 1), unify(Lx,1))), True)
    eq_(eval(let([(x,1)], unify(x,1))), True)
    
  def test3(self):
    assert_raises(NoSolution, eval, begin(unify(1, 1), unify(1, 2)))
    assert_raises(NoSolution, eval, begin(unify(2, 1), unify(1, 1)))
    assert_raises(NoSolution, eval, unify(1, 2))
    
  def test4(self):
    x = Var('x')
    Lx = LogicVar('x')
    eq_(eval(begin(unify(Lx, 1), unify(Lx,1))), True)
    
  def test5(self):
    x = Var('x')
    Lx = LogicVar('x')
    assert_raises(NoSolution, eval, begin(unify(Lx, 1), unify(Lx,2)))
    
  def test6(self):
    eq_(eval(unify(L(1), L(1))), True)
    
  def test7(self):
    x = LogicVar('x')
    eq_(eval(unify(L(x), L(1))), True)
    
  def test8(self):
    x = LogicVar('x')
    eq_(eval(unify(L(L(1, x)), L(L(1, x)))), True)
    
  def test9(self):
    x = LogicVar('x')
    x1 = Var('x')
    y1 = Var('y')
    eq_(eval(begin(assign(x1, L(L(1, x))),
                   assign(y1, L(L(1, x))),
                   unify(x1, y1))), True)
    
  def test10(self):
    x = LogicVar('x')
    x1 = Const('x')
    y1 = Const('y')
    eq_(eval(begin(assign(x1, L(L(1, x))),
                   assign(y1, L(L(1, x))),
                   unify(x1, y1))), True)
    
  def test20(self):
    eq_(eval(notunify(2, L(1))), True)
    
class XTestMetacall:
  def testcall(self):
    eq_(eval(call(unify(x, 1))), True)
    eq_(eval(is_(x, quote(prin(1)))&call(x)), None)
    
class Testfindall:
  def test_findall_or(self):
    eq_(eval(findall(or_(prin(1), prin(2)))), None)
  
  def test_findall_or_once(self):
    eq_(eval(findall(once(or_(prin(1), prin(2))))), None)
  
  def test_findall_first_p(self):
    eq_(eval(findall(first_p(prin(1), prin(2)))), None)
  
  def test_findall_template_or(self):
    x, y, z = LogicVar('x'), LogicVar('y'), LogicVar('z')
    f = Var('f')
    eq_(eval(begin(findall(or_(is_(x, 1), is_(x, 2)), x, y), getvalue(y))), [1, 2])
    
  def test_findall_template_func(self):
    x, y, z = LogicVar('x'), LogicVar('y'), LogicVar('z')
    f = Var('f')
    eq_(eval(let([(f, rules(((), 2), ((), 3)))], 
               findall(is_(x, f()), x, y), getvalue(y))), [2, 3])
      
class XTestRule:
  def test_abolish(self):
    from dao.builtins.rule import abolish
    eq_(eval(let([(f,function([[1], 1]))], abolish(f, 1))), {})
  def test_assert(self):
    from dao.builtins.rule import assert_
    eq_(eval(let([(f,function(([1], 1)))], 
               assert_(f, [2], [2]), f(2))), 2)
  def test_asserta(self):
    from dao.builtins.rule import asserta
    eq_(eval(let([(f,function(([1], 1)))], 
               asserta(f, [2], [2]), f(2))), 2)
  def test_replace(self):
    from dao.builtins.rule import replace
    eq_(eval(let([(f,function(([1], 1), ([2], 2)))], 
               replace(f, [2], [3]), f(2))), 3)
    
        
#from dao.builtins.container import concat, subsequence
#from dao.builtins.container import length, contain

class XTestStringConstruct:
  def test_string_length(self):
    eq_(eval(length("abc", x)), True)
  def test_string_length2(self):
    eq_(eval(begin(length("abc", x), x)), 3)
  def test_char_in(self):
    eq_(eval(begin(contain('abc', x), x)), 'a')
  def test_string_concat(self):
    eq_(eval(concat("abc", "def", "abcdef")), True)
    eq_(eval(begin(concat("abc", "def", x), x)), "abcdef")
    eq_(eval(begin(concat(y, "def", "abcdef"), y)), "abc")
  def test_string_concat2(self):
    eq_(eval(begin(concat(x, y, "abcdef"), y)), "bcdef")    
  def test_sub_string(self):
    eq_(eval(begin(findall(subsequence('ab', 0, y, 2, "ab"), y, z), z)), 
             [2])
  def test_sub_string2(self):
    eq_(eval(begin(findall(subsequence('ab', x, y, z, k), k, z), z)), 
             ['a', 'ab', 'b'])
  def test_findall_string_concat(self):
    eq_(eval(begin(findall(concat(x, y, "ab"), L(x, y), z), z)), 
             [L("a", "b")])
  def test_findall_string_concat2(self):
    eq_(eval(begin(findall(concat(x, y, "abc"), L(x, y), z), z)), 
             [L("a", "bc"), L("ab", "c")])    

from dao.builtins import getvalue, getvalue_default
from dao.builtins import isinteger, isfloat, isnumber, isstr, istuple, islist, isdict
from dao.solvebase import LogicVar as DaoLogicVar

class TestTerm:
  def test_getvalue(self):
    x = LogicVar('x')
    x1 = DaoLogicVar('x')
    eq_(eval(getvalue(x)), x1)
    eq_(eval(begin(unify(x, 1), getvalue(x))), 1)
    
  def test_getvalue_default(self):
    x = LogicVar('x')
    eq_(eval(getvalue_default(x)), None)
    eq_(eval(getvalue_default(x, 1)), 1)
    eq_(eval(begin(unify(x, 2), getvalue(x))), 2)
  
  def test_type(self):
    x = LogicVar('x')
    eq_(eval(isinteger(x)), False)
    eq_(eval(isinteger(1)), True)
    eq_(eval(isfloat(1)), False)
    eq_(eval(isfloat(1.0)), True)
    eq_(eval(isnumber(1.0)), True)
    eq_(eval(isnumber(1)), True)
    eq_(eval(istuple(())), True)
    eq_(eval(islist([])), True)
    eq_(eval(isdict({})), True)  
    
  def test_getvalue(self):
    from dao.solvebase import conslist
    eq_(eval(begin(getvalue(L("abc", 1)))), conslist("abc", 1))

from dao.builtins.quasiquote import DaoSyntaxError
from dao.builtins import quasiquote as qq, unquote as uq, unquote_splice as uqs

class TestQuasiquote:
  def test_simple1(self):
    eq_(eval(qq(1)), 1)
    
  def test_unquote1(self):
    eq_(eval(qq(uq(add(1,1)))), 2)
    
  def test_tuple1(self):
    eq_(eval(qq((1,))), (1,))
    eq_(eval(qq((1,2))), (1,2))
    eq_(eval(qq((add(1,1),2))), (add(1,1),2))
    
  def test_unquote_add(self):
    eq_(eval(qq(uq(add(1,1)))), 2)
    
  def test_unquote_slice(self):
    eq_(eval(qq(add(uqs(quote((3,4)))))), add(3, 4))
    
  def test_too_many_unquote(self):
    assert_raises(DaoSyntaxError, eval, qq(uq(uq(add(1,1)))))
   
  