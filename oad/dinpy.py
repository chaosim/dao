# -*- coding: utf-8 -*-

# What will happen when lisp meets prolog in python?

'''dao grammar embeded in python list display by operator grammar'''

__all__ = ['_', 'v', 'var', 'vars', 'dummies', 'put', 
  'iff', 'let', 'case', 'els', 'block', 
  'label', 'do', 'loop', 'each', 'when', 'exit', 'next',
  'fun', 'macro', 'at', 'parse',
  'py', 'some', 'any', 'may']

from oad.pysyntax import *
from oad.term import Var, DummyVar, Apply, conslist as L, vars, dummies
from oad import builtin
from oad import special
from oad.special import set as assign
from oad.builtins.matchterm import some, any, optional as opt
from oad.builtins.terminal import eof
from oad.builtins.type import pytuple, make_apply, head_list, list_tail#, to_list
from oad.builtins.type import items, first #, index
from oad.builtins.term import getvalue, ground_value
from oad.builtins.arithpred import is_, ne as pred_ne
from oad.term import dummies

class DinpySyntaxError(Exception): pass

## vv.a
def single_var(klass, varcahce):
  class VForm(object):
    def __getattr__(self, var): return varcache(var, klass)
  return lead(VForm)

_var_cache2 = {}
def varcache2(name, klass=Var):
  return _var_cache2.setdefault(klass, {}).setdefault(name, klass(name))

# used only in dinpy.py internally, do not export me.
vv = single_var(Var, varcache2)
__ = single_var(DummyVar, varcache2)

_var_cache = {}
def varcache(name, klass=Var):
  return _var_cache.setdefault(klass, {}).setdefault(name, klass(name))

# used in codes parsed by dinpy parser
v = single_var(Var, varcache)
_ = single_var(DummyVar, varcache)

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

@builtin.function('getvar')
def getvar(name, klass=Var): 
  return varcache(name, klass)

def _vars(text): return [varcache2(x.strip()) for x in text.split(',')]

class MayForm:
  def __div__(self, element):
    return opt(element)
may = MayForm()

def some_any(matcher):
  class Form:
    def __div__(self, item):
      try:
        self.template
        return matcher(self.element, self.template, item)
      except:
        try: 
          self.element
          self.template = item
        except: self.element = item
      return self
    def __add__(self, other):
      try: 
        self.template
        raise DinpySyntaxError
      except:
        try: return matcher(self.element)+other
        except: raise DinpySyntaxError
    def __or__(self, other):
      try: 
        self.template
        raise DinpySyntaxError
      except:
        try: return matcher(self.element)|other
        except: raise DinpySyntaxError
    def __parse_syntax__(self):
      try: 
        self.template
        raise DinpySyntaxError
      except:
        try: return matcher(self.element)
        except: raise DinpySyntaxError
  return Form

Some = lead(some_any(some))
Any = lead(some_any(any))

# my.a, globl.a
## my = element(some(getattr(__._), L('local', __._), y)+eof)
## globl = element(some(getattr(__._), L('globl', __._), y)+eof)

##use_item = attr|div(var)|div(str)
##use = 'use'+some(use_item)+optional(use_block)|any(use_item)+use_block\
##          |some(use_item)+div+use_block

# put.a = 1, put.i.j==(1,2)
put = element(
  # put.a == 1
    getattr(vv.var)+eq(vv.value)+eof
      +make_apply(special.set, getvar(vv.var), vv.value)
  # put.i.j==(1,2)
  | Some/(getattr(__._)+assign(vv.x, getvar(__._)))/vv.x/vv.vars+eq(vv.value)+eof
        +make_apply(special.set_list, vv.vars, vv.value)
  )

_do, _of, _at = words('do, of, at')

@builtin.function('getvar')
def make_let(args, body): 
  if not isinstance(args[0], dict): raise DinpySyntaxError
  for k, v in args[0].items():
    if not isinstance(k, Var): raise DinpySyntaxError
  if isinstance(body, tuple): return special.let(args[0], *body)
  else: return special.let(args[0], body)
  
#let({var:value}).do[...]
let = element(call(vv.bindings)+_do+getitem(vv.body)+eof
              +make_let(vv.bindings, vv.body))

@builtin.function('make_iff')
def make_iff(test, clause, clauses, els_clause):
  els_clause = els_clause if not isinstance(els_clause, Var) else None
  return special.iff([(test[0],clause)]+clauses, els_clause)

_then, _elsif, _els = words('then, elsif, els')
_test, _test2, _body =  dummies('_test, _test2, _body')

# iff(1).then[2], iff(1).then[2]  .elsif(3).then[4] .els[5]
iff = element(call(vv.test)+_then+getitem(vv.clause)
              +any(_elsif+call(_test)+_then+getitem(_body)+is_(_test2, first(_test)), 
                   (_test2, _body), vv.clauses)
              +opt(_els+getitem(vv.els_clause))+eof
              +make_iff(vv.test, vv.clause, vv.clauses, vv.els_clause))

class CASE_ELS: pass
CASE_ELS = CASE_ELS()

@builtin.function('make_case')
def make_case(test, cases): 
  case_dict = {}
  for case, clause in cases:
    if isinstance(clause, tuple): clause = list(clause)
    elif not isinstance(clause, list): clause = [clause]
    if isinstance(case, tuple):
      assert case!=()
      for x in case:
        case_dict[x] = clause
    elif case is CASE_ELS: 
      els_clause = clause
    else: case_dict[case] = clause
  return special.CaseForm(test[0], case_dict, els_clause)

of_fun = attr_call('of')

# case(x).of(1)[write(1)].of(2,3)[write(4)].els[write(5)]
case = element( call(vv.test)+(
  #.of(1)[write(1)].of(2,3)[write(4)].els[write(5)]
    (some(of_fun(__.values)+getitem(__.clause),(__.values,__.clause), vv.clauses)
     +_els+getitem_to_list(vv.els)+eof
    +make_case(vv.test, list_tail(vv.clauses, pytuple(CASE_ELS, vv.els))))
  #/{1:[write(1)],2:[write(4)],3:[write(4)], els:[write(5)]}
  | div(vv.clauses)+eof+make_case(vv.test, items(vv.clauses))
  ))
els = CASE_ELS

# when(x>1).do[write(1)
when = element(call(vv.test)+_do+getitem_to_list(vv.body)+eof+
  make_apply(special.LoopWhenForm, vv.body, first(vv.test)))

when_fun = attr_call('when')
until_fun = attr_call('until')

# do.write(1).until(1), do.write(1).when(1)
do = element(getitem_to_list(vv.body)+(
  # .when(1)
    when_fun(vv.test)+eof
    +make_apply(special.LoopWhenForm, vv.body, first(vv.test))
  #.until(1)
  | until_fun(vv.test)+eof
    +make_apply(special.LoopUntilForm, vv.body, first(vv.test))
  ))

# loop[write(1)], loop(1)[write(1)]
loop = element(
  # loop[write(1)]
    getitem_to_list(vv.body)+eof
    +make_apply(special.LoopForm, vv.body)
  # loop(1)[write(1)]
  | call(vv.times)+getitem_to_list(vv.body)+eof
    +make_apply(special.LoopTimesForm, first(vv.times), vv.body)
  )

@builtin.function('make_each1')
def make_each(vars, iterators, body):
  if len(iterators)==1: iterators = iterators[0]
  else:
    iterators1 = []
    for iterator in iterators:
      if isinstance(iterator, slice):
        iterators1.append(range(iterator.start, iterator.stop, 
                               1 if iterator.step is None else iterator.step))
      else: iterators1.append(iterator)
    iterators = zip(*iterators1)
  return special.EachForm(vars, iterators, body)

# each(i,j)[1:10][1:10]. do[write(i, j)]
# each(i,j)[zip(range(5), range(5))]. do [write(i,j)],
# each(i,j)[range(5)][range(5)]. do [write(i,j)],
each = element(call(vv.vars)+some(getitem(__.iterator), __.iterator, vv.iterators)
               +_do+getitem_to_list(vv.body)+eof
    +make_each(vv.vars, vv.iterators, vv.body))

exit = element(may/getattr(vv.type)+opt(div(vv.label))+opt(rshift(vv.value))+eof)
next = element(opt(getattr(vv.type))+opt(div(vv.label))+eof)
label = element(getattr(vv.type)+div(vv.body)+eof)

block = element(getattr(vv.name)+getitem(vv.block)+eof)

py = element(div(vv.func)+call(vv.args)+eof)

##on = element(call(x)+do_word+body+eof) # with statements in dao

class AtForm:
  def __init__(self, clauses):
    self.clauses = clauses
  def __eq__(self, other): 
    return self.clauses==other.clauses
  def __repr__(self): return 'AtForm(%s)'%repr(self.clauses)
    
# at(*args)[...](*args)[...][...]
# at[...][...]
at = element(
  some(opt(call(__.args))+assign(__.args, ground_value(__.args))
       +some(getitem_to_list(__.body), __.body, __.bodies), 
        (__.args, __.bodies), vv.args_bodies)+eof
        +make_apply(AtForm, vv.args_bodies))

from oad.builtins.rule import replace, remove, assert_, asserta, \
     abolish, retractall, retract

# fun. a(x)== [...]
@builtin.function('make_fun1')
def make_fun1(name, args, body): 
  fun = varcache(name)
  head = args
  if isinstance(body, AtForm): 
    body = body.clauses
    if len(body)>1: raise DinpySyntaxError
    if body[0][0] is not None: raise DinpySyntaxError
    if len(body[0][1])==1: return replace(fun, head, *body[0][1][0])
    return special.begin(*[remove(fun, head)]+[assert_(fun, head, x) for x in body[0][1]])
  else: return replace(fun, head, *body)
  
# fun. a(x) >= [...]
@builtin.function('make_fun2')
def make_fun2(name, args, body): 
  fun = varcache(name)
  head = args
  if isinstance(body, AtForm): 
    body = body.clauses
    if len(body)>1: raise DinpySyntaxError
    if body[0][0] is not None: raise DinpySyntaxError
    if len(body[0][1])==1: return assert_(fun, head, *body[0][1][0])
    return special.begin(*[assert_(fun, head, x) for x in body[0][1]])
  else: return assert_(fun, head, *body)
  
# fun. a(x) <= [...]
@builtin.function('make_fun3')
def make_fun3(name, args, body): 
  fun = varcache(name)
  head = args
  if isinstance(body, AtForm): 
    body = body.clauses
    if len(body)>1: raise DinpySyntaxError
    if body[0][0] is not None: raise DinpySyntaxError
    if len(body[0][1])==1: return asserta(fun, head, *body[0][1][0])
    return special.begin(*[asserta(fun, head, x) for x in reversed(body[0][1])])
  else: return asserta(fun, head, *body)
    
#  fun. a== [...]
@builtin.function('make_fun4')
def make_fun4(klass, name, rules): 
  fun = varcache(name)
  if isinstance(rules, AtForm): 
    rules1 = []
    for head, bodies in rules.clauses:
      if head is None: raise DinpySyntaxError
      for body in bodies:
        rules1.append((head, body))
    return assign(fun, klass(rules1))
  else: raise DinpySyntaxError
  
#  fun. a>= [...]
@builtin.function('make_fun5')
def make_fun5(name, rules): 
  fun = varcache(name)
  if isinstance(rules, AtForm): 
    rules1 = []
    for head, bodies in rules.clauses:
      if head is None: raise DinpySyntaxError
      for body in bodies:
        rules1.append((head, body))
    return special.begin(*[assert_(fun, head, body) for head, body in rules1])
  else: raise DinpySyntaxError
  
#  fun. a>= [...]
@builtin.function('make_fun6')
def make_fun6(name, rules): 
  fun = varcache(name)
  if isinstance(rules, AtForm): 
    rules1 = []
    for head, bodies in rules.clauses:
      if head is None: raise DinpySyntaxError
      for body in bodies: rules1.append((head, body))
    return special.begin(*[asserta(fun, head, body) for head, body in reversed(rules1)])
  else: raise DinpySyntaxError
  
#  fun[...]
@builtin.function('make_fun7')
def make_fun7(name, rules): 
  fun = varcache(name)
  if isinstance(rules, AtForm): 
    rules1 = []
    for head, bodies in rules.clauses:
      if head is None: raise DinpySyntaxError
      for body in bodies: rules1.append((head, body))
    return special.begin(*[asserta(fun, head, body) for head, body in reversed(rules1)])
  else: raise DinpySyntaxError
  
## fun. a(x)== [...],  fun. a(x) <= at[...][...], 覆盖与a(x)匹配的整个定义
## fun. a(x) >= [...],  fun. a(x) <= at[...][...], 对参数组附加定义
## fun. a(x) <= [...],  fun. a(x) <= at[...][...]，对参数组前补定义
## fun. a== at(..)[...](..)[...][...]，重定义
## fun. a>= at(..)[...](..)[...][...]，附加定义
## fun. a<= at(..)[...](..)[...][...]，前补定义
## fun(..)[...](..)[...][...]
def fun_macro_grammar(klass):
  return (
  # fun. a(x)== [...],  fun. a(x) <= at[...][...]
    (getattr(vv.name)+call(vv.args)+eq(vv.body)+eof
      +make_fun1(vv.name,vv.args, vv.body))
  # fun. a(x) >= [...],  fun. a(x) <= at[...][...]
  | (getattr(vv.name)+call(vv.args)+ge(vv.body)+eof
     +make_fun2(vv.name,vv.args, vv.body))
  # fun. a(x) <= [...],  fun. a(x) <= at[...][...]
  | (getattr(vv.name)+call(vv.args)+le(vv.body)+eof
     +make_fun3(vv.name,vv.args, vv.body))
  #  fun. a== at(..)[...]
  | (getattr(vv.name)+eq(vv.rules)+eof
     +make_fun4(klass, vv.name,vv.rules))
  #  fun. a>= at(..)[...]
  | (getattr(vv.name)+ge(vv.rules)+eof+make_fun5(vv.name,vv.rules))
  #  fun. a<= at(..)[...]
  | (getattr(vv.name)+le(vv.rules)+eof+make_fun6(vv.name,vv.rules))
  #  fun(args) [...](args)[...][...]
  | (some(call(__.head) + some(
      getitem_to_list(__.body), __.body, __.bodies), (__.head, __.bodies), vv.rules)
        +eof+make_fun7(vv.rules))
  #   - fun.a/3,
  | (getattr(vv.name)+neg+div(vv.arity)+eof
     +make_apply(abolish, getvar(vv.name), vv.arity))
  #- fun.a(x),
  | (getattr(vv.name)+call(vv.args)+neg+eof
     +make_apply(retractall, getvar(vv.name), vv.args))
  #- fun.a(x)[1],
##  | (getattr(vv.name)+call(vv.args)+getitem(vv.index)+neg+assign(result, retract(vv.name, vv.args)))
  )

fun = element(fun_macro_grammar(special.FunctionForm))
macro = element(fun_macro_grammar(special.MacroForm))

def lshift_apply(function):
  return element(some(lshift(vv.arg))+not_(follow_by(lshift(_))), make_apply(function, vv.arg))
