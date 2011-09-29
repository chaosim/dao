# -*- coding: utf-8 -*-

'''dao grammar embeded in python list display by operator grammar'''

##__all__ = ['var', 'v', 'do', 'loop', 'put']

from oad.pysyntax import *

from oad.builtins import format
from oad.term import Var, DummyVar, Apply, conslist as L
from oad.builtins.matchterm import some, any, optional as opt
from oad.builtins.terminal import eof
from oad.special import assign

builtins_dict = {
  'write': format.write}

_var_cache = {}
def varcache(name):
  try: return _var_cache[name]
  except: 
    _var_cache[name] = Var(name)
    return _var_cache[name]

def name2obj(name):
  try: return builtins_dict[name]
  except: return varcache(name)

_ = DummyVar('_')
x, y, z = Var('x'), Var('y'), Var('z')

class VarForm(object):
  def __init__(self):
    self.__vars__ = []
  def __getattr__(self, var):
    self.__vars__.append(varcache(var))
    return self
  def __len__(self): return len(self.__vars__)
  def __iter__(self): return iter(self.__vars__)
var = lead(VarForm)

class VForm(object):
  def __getattr__(self, var): return varcache(var)
v = lead(VForm)

##use_item = attr|div(var)|div(str)
##use = 'use'+some(use_item)+optional(use_block)|any(use_item)+use_block\
##          |some(use_item)+div+use_block

## v.a, my.a, globl.a
## var.a.b.c
my = element(some(getattr(_), L('local', _), y)+eof)
globl = element(some(getattr(_), L('globl', _), y)+eof)

# put.a = 1
put = element(getattr(x)+eq(y)+eof+assign(varcache(x), y))
##put = element(some(getattr(_), varcache(_), x) + eq(y) + eof+assign(x, y))

do_word = word('do')
of = word('of')
at = word('at')
then = word('then')
elsif = word('elsif')
els = word('els')

block = getitem(x)
stmts = some(call(_), _, x)
body = block|stmts

def check_let_bindings(result, bindings): pass
let = element(call(check_let_bindings)+do_word+body)

when_fun = funcall('when')
until_fun = funcall('until')

# do.write(1).eof, do[ write(1)], do.write(1).until(1), do.write(1).when(1)
do_condition = opt(when_fun(x, y)+opt(until_fun))
do = element(body+do_condition)

on = element(call(x)+do_word+body+eof) # with statements in dao
case = element(call(x)+some(of+call(x)+some(stmts)|block))|div(dict)
iff = element(call(x)+then+body+any(elsif+body)+opt(els+body))
loop = element(call(x)+body)

##fun = element(getattr(x)+(eq+rule_dict|(at+rule_list)))
##
##decl = use|var|v|my|out|globl
##stmt = do|put|let|fun|macro|on|case|iff|loop|rule|rules|put
##form = decl|stmt

