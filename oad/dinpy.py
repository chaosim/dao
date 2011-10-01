# -*- coding: utf-8 -*-

'''dao grammar embeded in python list display by operator grammar'''

##__all__ = ['var', 'v', 'do', 'loop', 'put']

from oad.pysyntax import *

from oad.builtins import format
from oad.term import Var, DummyVar, Apply, conslist as L
from oad.builtins.matchterm import some, any, optional as opt
from oad.builtins.terminal import eof
from oad import special
from oad.builtins.type import pytuple, make_apply, head_list, index
from oad.builtins.term import getvalue, ground_value
from oad import builtin
from oad.builtins.arithpred import is_

def vars(names): return [Var(x.strip()) for x in names.split(',')]
def dummies(names): return [DummyVar(x.strip()) for x in names.split(',')]

builtins_dict = {
  'write': format.write}

_var_cache = {}
def varcache(name):
  try: return _var_cache[name]
  except: 
    _var_cache[name] = Var(name)
    return _var_cache[name]

@builtin.function('getvar')
def getvar(name): 
  return varcache(name)

def name2obj(name):
  try: return builtins_dict[name]
  except: return varcache(name)

_ = DummyVar('_')
x, y, z = Var('x'), Var('y'), Var('z')

## v.a
class VForm(object):
  def __getattr__(self, var): return varcache(var)
v = lead(VForm)

## var.a.b.c
class VarForm(object):
  def __init__(self):
    self.__vars__ = []
  def __getattr__(self, var):
    self.__vars__.append(varcache(var))
    return self
  def __len__(self): return len(self.__vars__)
  def __iter__(self): return iter(self.__vars__)
var = lead(VarForm)

# my.a, globl.a
## my = element(some(getattr(_), L('local', _), y)+eof)
## globl = element(some(getattr(_), L('globl', _), y)+eof)

##use_item = attr|div(var)|div(str)
##use = 'use'+some(use_item)+optional(use_block)|any(use_item)+use_block\
##          |some(use_item)+div+use_block

# put.a = 1
put = element(getattr(x)+eq(y)+eof+make_apply(special.set, getvar(getvalue(x)), y))
##put = element(some(getattr(_), varcache(_), x) + eq(y) + eof+assign(x, y))

do_word = word('do')
of = word('of')
at = word('at')

block = getitem(x)
stmts = some(call(_), _, x)
body = block #|stmts

@builtin.function('getvar')
def make_let(args, body): 
  if isinstance(body, tuple): return special.let(args[0], *body)
  else: return special.let(args[0], body)
let = element(call(x)+do_word+getitem(y)+eof+make_let(x, y))

@builtin.function('make_iff')
def make_iff(clauses, els_clause): 
  return special.iff(clauses, els_clause)

then, elsif, els = word('then'), word('elsif'), word('els')
test, clause, clauses, els_clause = vars('test, clause, clauses, els_clause')
_test, _test2, _body =  dummies('_test, _test2, _body')
iff = element(call(test)+then+getitem(clause)
              +any(elsif+call(_test)+then+getitem(_body)+is_(_test2, index(_test, 0)), 
                   (_test2, _body), clauses)
              +opt(els+getitem(els_clause))+eof
              +make_iff(head_list(pytuple(index(test, 0), clause), clauses), 
                        ground_value(els_clause)))

when_fun = funcall('when')
until_fun = funcall('until')

# do.write(1).eof, do[ write(1)], do.write(1).until(1), do.write(1).when(1)
do_condition = opt(when_fun(x, y)+opt(until_fun))
do = element(body+do_condition)

on = element(call(x)+do_word+body+eof) # with statements in dao
case = element(call(x)+some(of+call(x)+some(stmts)|block))|div(dict)
loop = element(call(x)+body)

##fun = element(getattr(x)+(eq+rule_dict|(at+rule_list)))
##
##decl = use|var|v|my|out|globl
##stmt = do|put|let|fun|macro|on|case|iff|loop|rule|rules|put
##form = decl|stmt

