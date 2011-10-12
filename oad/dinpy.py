# -*- coding: utf-8 -*-

# What will happen when lisp meets prolog in python?

'''dao grammar embeded in python list display by operator grammar'''

# keyword must be short(don't be longer than 5 letters).
# keyword can not be the keyword of python.

# control: dao, do, loop, each, if_, iff, els, elsif, when, until, on, at, exit, next, goto
# variable: use, set, out, local, var, v, globl, my, dummy
# structure: rule, rules, lamda, let, letre, fun, macro, klass, block, label
# solve, eval

__all__ = [
  'dao', 'parse', 
  
  # declaration and variable
  '_', 'v', 'var', 'vars', 'dummies', 'put', 
  
  # control structure
  'iff', 'let', 'case', 'els',  'on', 'block','label',
  'do', 'loop', 'each', 'when', 'exit', 'next',
  
  # function and macro definition
  'fun', 'macro', 'at',
  
  # miscellaneous
  'py', 'some', 'any', 'may']

from oad.pysyntax import *
from oad import dexpr
from oad.dexpr import VarSymbol, DummyVarSymbol
from oad.term import Var, DummyVar, Apply, conslist as L, vars, dummies
from oad import builtin
from oad import special
from oad.special import set as assign
from oad.builtins.matcher import some, any, may
from oad.builtins.terminal import eos
from oad.builtins.term import pytuple, pycall, py_apply, head_list, list_tail
from oad.builtins.term import items, first, left
from oad.builtins.term import getvalue, ground_value, is_
from oad.builtins.arith import ne_p
from oad.solve import eval, solve, tag_loop_label, parse_block

class DinpySyntaxError(Exception): pass

class Dao(object): 
  def __init__(self): 
    self.code = []
  def __setattr__(self, attr, value):
    if attr=='version':
      self._version = tuple(int(x) for x in value.split('.'))
      self.code = []
    else: return object.__setattr__(self, attr, value)
  def eval(self):
    result = eval(special.begin(*self.code))
    self.code = []
    return result
  def solve(self):
    result = solve(special.begin(*self.code))
    self.code = []
    return result
  def __getitem__(self, code):
    code = parse(code)
    code = tag_loop_label(code)
    if isinstance(code, tuple): self.code +=  list(code)
    else: self.code += [code]
    return self
dao = Dao()

## vv.a v.a
def my_single_var(name, klass):
  class VForm(object):
    def __init__(self, name=name, grammar=None):
      self.__form_name__ = 'v'
      self.__form_grammar__ = None
    def __getattr__(self, var): return my_varcache(var, klass)
  return lead(VForm)

_my_varcache = {}
def my_varcache(name, klass=Var):
  return _my_varcache.setdefault(klass, {}).setdefault(name, klass(name))

# used only in dinpy.py internally, do not export me.
vv = my_single_var('vv', Var)
__ = my_single_var('__', DummyVar)

## vv.a v.a
def single_symbol(name, klass):
  class VForm(object):
    def __init__(self, name=name, grammar=None):
      self.__form_name__ = 'v'
      self.__form_grammar__ = None
    def __getattr__(self, name): return klass(name)
  return lead(VForm)

from oad.dexpr import varcache

# used in codes parsed by dinpy parser
v = single_symbol('v', VarSymbol)
_ = single_symbol('_', DummyVarSymbol)

## var.a.b.c
class VarForm(object):
  def __init__(self, name=None, grammar=None):
    self.__form_name__ = 'var'
    self.__form_grammar__ = None
    self.__vars__ = []
  def __getattr__(self, var):
    self.__vars__.append(VarSymbol(var))
    return self
  def __len__(self): return len(self.__vars__)
  def __iter__(self): return iter(self.__vars__)
var = lead(VarForm)

@builtin.function('getvar')
def getvar(name, klass=Var): 
  return varcache(name, klass)

def _vars(text): return [varcache2(x.strip()) for x in text.split(',')]

# my.a, globl.a
## my = element(some(getattr(__._), L('local', __._), y)+eos)
## globl = element(some(getattr(__._), L('globl', __._), y)+eos)

##use_item = attr|div(var)|div(str)
##use = 'use'+some(use_item)+mayional(use_block)|any(use_item)+use_block\
##          |some(use_item)+div+use_block

# put.a = 1, put.i.j==(1,2)
put = element('put',
  # put.a == 1
    getattr(vv.var)+eq(vv.value)+eos
      +pycall(special.set, getvar(vv.var), vv.value)
  # put.i.j==(1,2)
  | (getattr(__._)+assign(vv.x, getvar(__._)))[1:]%vv.x*vv.vars+eq(vv.value)+eos
        +pycall(special.set_list, vv.vars, vv.value)
  )

_do, _of, _at = words('do, of, at')

def get_eq_operands(binary):
  if not isinstance(binary.y, VarSymbol):
    raise DinpySyntaxError
  if isinstance(binary.x, dexpr.__eq__):
    return get_eq_operands(binary.x)+(binary.y,)
  elif isinstance(binary.x, VarSymbol):
    return (binary.x, binary.y)
  raise DinpySyntaxError

@builtin.function('getvar')
def make_let(args, body): 
  bindings = {}
  for b in args:
    if not isinstance(b, dexpr.__eq__): raise DinpySyntaxError
    k, v = parse(b.x), parse(b.y)
    if isinstance(k, Var): bindings[k] = v
    else: 
      if isinstance(k, dexper.__eq__): k =  get_eq_operands(k)
      elif not isinstance(k, list): raise DinpySyntaxError
      for x in k: 
        x = parse(x) 
        if isinstance(x, Var): bindings[x] = v
        else: raise DinpySyntaxError
  if isinstance(body, tuple): return special.let(bindings, *body)
  else: return special.let(bindings, body)
  
#let({var:value}).do[...]
let = element('let',
  call(vv.bindings)+_do+getitem(vv.body)+eos
    +make_let(vv.bindings, vv.body))

@builtin.function('make_iff')
def make_iff(test, clause, clauses, els_clause):
  els_clause = parse_block(els_clause) if not isinstance(els_clause, Var) else None
  test = parse(test[0])
  clause = parse_block(clause)
  clauses1 =  [(parse(t), parse_block(c)) for t, c in clauses]
  return special.iff([(test, clause)]+clauses1, els_clause)

_then, _elsif, _els = words('then, elsif, els')
_test, _test2, _body =  dummies('_test, _test2, _body')

# iff(1).then[2], iff(1).then[2]  .elsif(3).then[4] .els[5]
iff = element('iff',
              call(vv.test)+getitem(vv.clause)
              +any(_elsif+call(_test)+getitem(_body)+is_(_test2, first(_test)), 
                   (_test2, _body), vv.clauses)
              +may(_els+getitem(vv.els_clause))+eos
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
case = element('case',
  call(vv.test)+(
  #.of(1)[write(1)].of(2,3)[write(4)].els[write(5)]
    (some(of_fun(__.values)+getitem(__.clause),(__.values,__.clause), vv.clauses)
     +_els+getitem_to_list(vv.els)+eos
    +make_case(vv.test, list_tail(vv.clauses, pytuple(CASE_ELS, vv.els))))
  #/{1:[write(1)],2:[write(4)],3:[write(4)], els:[write(5)]}
  | div(vv.clauses)+eos+make_case(vv.test, items(vv.clauses))
  ))
els = CASE_ELS

# when(x>1).do[write(1)
when = element('when',
  call(vv.test)+_do+getitem_to_list(vv.body)+eos+
  pycall(special.LoopWhenForm, vv.body, first(vv.test)))

when_fun = attr_call('when')
until_fun = attr_call('until')

# do.write(1).until(1), do.write(1).when(1)
do = element('do',
  getitem_to_list(vv.body)+(
  # .when(1)
    when_fun(vv.test)+eos
    +pycall(special.LoopWhenForm, vv.body, first(vv.test))
  #.until(1)
  | until_fun(vv.test)+eos
    +pycall(special.LoopUntilForm, vv.body, first(vv.test))
  ))

# loop[write(1)], loop(1)[write(1)]
loop = element('loop',
  # loop[write(1)]
    getitem_to_list(vv.body)+eos
    +pycall(special.LoopForm, vv.body)
  # loop(1)[write(1)]
  | call(vv.times)+getitem_to_list(vv.body)+eos
    +pycall(special.LoopTimesForm, first(vv.times), vv.body)
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
each = element('each',
    call(vv.vars)+some(getitem(__.iterator), __.iterator, vv.iterators)
               +_do+getitem_to_list(vv.body)+eos
    +make_each(vv.vars, vv.iterators, vv.body))

@builtin.function('make_exit')
def make_exit(type, level, label, value):
  type = None if isinstance(type, Var) else type
  level = 0 if isinstance(level, Var) else level
  label = None if isinstance(label, Var) else label
  value = None if isinstance(value, Var) else value
  return special.exit(value, type, level, label)

# exit.loop^2 >> 3,  eixt/a>>3
exit = element('exit', 
         may((getattr(vv.type)+may(div(vv.level)))|may(div(vv.label)))
             +may(rshift(vv.value))+eos
             +make_exit(vv.type, vv.level, vv.label, vv.value))

@builtin.function('make_next')
def make_next(type, level, label):
  type = None if isinstance(type, Var) else type
  level = 0 if isinstance(level, Var) else level
  label = None if isinstance(label, Var) else label
  return special.next(type, level, label)

next = element('next', 
        may(getattr(vv.type)+may(div(vv.level))|may(div(vv.label)))+eos
        +make_next(vv.type, vv.level, vv.label))

@builtin.function('set_loop_label')
def set_loop_label(label, body):
  body = parse(body)
  if not isinstance(body, special.RepeatForm): raise DinpySyntaxError
  body.label = label
  return body

label = element('label', getattr(vv.name)+mod(vv.body)+eos
          +set_loop_label(vv.name, vv.body))

@builtin.function('make_block')
def make_block(name, body):
  return special.block(name, *body)

block = element('block', getattr(vv.name)+getitem_to_list(vv.body)+eos
                +make_block(vv.name, vv.body))

py = element('py', 
       ( getattr(vv.func)+assign(vv.func, getvar(vv.func))+eos
           +pycall(vv.func, vv.args))
       | +call(vv.args)+eos+pycall(first(vv.args), left(vv.args)))

# with statements in dao
on = element('on', call(vv.form)+_do+getitem(vv.body)+eos+
    pycall(special.OnForm, vv.form, vv.body)) 

class AtForm:
  def __init__(self, clauses):
    self.clauses = clauses
  def __eq__(self, other): 
    return self.clauses==other.clauses
  def __repr__(self): return 'AtForm(%s)'%repr(self.clauses)
    
# at(*args)[...](*args)[...][...]
# at[...][...]
at = element('at',
  some(may(call(__.args))+assign(__.args, ground_value(__.args))
       +some(getitem_to_list(__.body), __.body, __.bodies), 
        (__.args, __.bodies), vv.args_bodies)+eos
        +pycall(AtForm, vv.args_bodies))

from oad.builtins.rule import replace, remove, assert_, asserta, \
     abolish, retractall, retract

# fun. a(x)== [...]
@builtin.function('make_fun1')
def make_fun1(name, args, body): 
  fun = varcache(name)
  head = args
  if isinstance(body, AtForm): 
    body = body.clauses
    if len(body)>1: raise DinpySyntaxError # should not be (args)[body]...(args)[body]
    if body[0][0] is not None: raise DinpySyntaxError # should not be (args)[body]
    body = body[0][1][0]
    if len(body[0][1])==1: 
      return (special.if_(callp(fun), replace(fun, head, *body), 
                special.set(fun, function((head, body)))))
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
    (getattr(vv.name)+call(vv.args)+eq(vv.body)+eos
      +make_fun1(vv.name,vv.args, vv.body))
  # fun. a(x) >= [...],  fun. a(x) <= at[...][...]
  | (getattr(vv.name)+call(vv.args)+ge(vv.body)+eos
     +make_fun2(vv.name,vv.args, vv.body))
  # fun. a(x) <= [...],  fun. a(x) <= at[...][...]
  | (getattr(vv.name)+call(vv.args)+le(vv.body)+eos
     +make_fun3(vv.name,vv.args, vv.body))
  #  fun. a== at(..)[...]
  | (getattr(vv.name)+eq(vv.rules)+eos
     +make_fun4(klass, vv.name,vv.rules))
  #  fun. a>= at(..)[...]
  | (getattr(vv.name)+ge(vv.rules)+eos+make_fun5(vv.name,vv.rules))
  #  fun. a<= at(..)[...]
  | (getattr(vv.name)+le(vv.rules)+eos+make_fun6(vv.name,vv.rules))
  #  fun(args) [...](args)[...][...]
  | (some(call(__.head) + some(
      getitem_to_list(__.body), __.body, __.bodies), (__.head, __.bodies), vv.rules)
        +eos+make_fun7(vv.rules))
  #   - fun.a/3,
  | (getattr(vv.name)+neg+div(vv.arity)+eos
     +pycall(abolish, getvar(vv.name), vv.arity))
  #- fun.a(x),
  | (getattr(vv.name)+call(vv.args)+neg+eos
     +pycall(retractall, getvar(vv.name), vv.args))
  #- fun.a(x)[1],
##  | (getattr(vv.name)+call(vv.args)+getitem(vv.index)+neg+assign(result, retract(vv.name, vv.args)))
  )

fun = element('fun', fun_macro_grammar(special.FunctionForm))
macro = element('macro', fun_macro_grammar(special.MacroForm))
