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
  'dao', 'preparse', 'eval', 'solve',
  
  # declaration and variable
  '_', 'v', 'var', 'put', #, 'vars', 'dummies'
  
  # control structure
  'iff', 'let', 'letr', 'case', 'els',  'on', 'block','label',
  'do', 'loop', 'each', 'when', 'exit', 'next',
  'catch', 'throw', 'protect',
  
  # function and macro definition
  'fun', 'macro', 'at',
  
##  # miscellaneous
##  'py', 'some', 'any', 'may'
]

from dao.pysyntax import *
from dao.dinpy import dexpr
from dao.dinpy.dexpr import _VarSymbol, _DummyVarSymbol
from dao.term import Var, DummyVar, CommandCall, conslist as L, vars, dummies
from dao import builtin
from dao import special
from dao.special import set as assign
from dao.builtins.matcher import some, any, may
from dao.builtins.terminal import eos
from dao.builtins.term import pytuple, pycall, py_CommandCall, head_list, list_tail
from dao.builtins.term import items, first, left
from dao.builtins.term import getvalue, getvalue_default, is_
from dao.builtins.arith import ne_p
from dao.solve import eval as oad_eval, solve, tag_loop_label
from dao.solve import set_run_mode, noninteractive, DaoUncaughtThrow
from dao.solve import interactive_parser, interactive_tagger, interactive_solver


def eval(code):
  code = preparse(code)
  code = tag_loop_label(code)
  return oad_eval(code)

class DaoCodeFormater: 
  def __init__(self, indent_width=2):
    self.indent = 0
    self.indent_width = indent_width
    self.text = ''
    self.row = 0
    self.column = 0
    
  def indent(self): self.indent += self.indent_width
  def unindent(self): self.indent -= self.indent_width
  def pprint(self, code):
    try: code____pprint___ = code.___pprint___
    except:
      if isinstance(code, list) or isinstance(code, tuple): 
        for x in code:
          self.text += self.pprint(x)
      else: self.text += repr(code)
    code____pprint___(self)
  
class Dao(object): 
  def __init__(self): 
    self.code = []
  def __setattr__(self, attr, value):
    if attr=='version':
      self._version = tuple(int(x) for x in value.split('.'))
      self.code = []
      return self
    else: return object.__setattr__(self, attr, value)
  def __getitem__(self, code):
    set_run_mode(noninteractive)
    if isinstance(code, tuple): self.code +=  list(code)
    else: self.code += [code]
    return self
  def preparse(self):
    return tag_loop_label(preparse(self.code))
  def solve(self):
    result = solve(tag_loop_label(preparse(self.code)))
    self.code = []
    return result
  def eval(self):
    result = oad_eval(tag_loop_label(preparse(self.code)))
    self.code = []
    return result
  def pprint(self, formater=None):
    if formater is None: formater = DaoCodeFormater()
    formater.text += 'dao.version = %s' % self.version
    return formater.pprint(self.code)
    
dao = Dao()

def solve(exp):
  code = interactive_parser().parse(exp)
  code = interactive_tagger().tag_loop_label(code)
  return interactive_solver().solve(code)
    
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

def _vars(text): return [_my_varcache(x.strip()) for x in text.split(',')]

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

from dao.dinpy.dexpr import varcache

# used in codes parsed by dinpy parser
v = single_symbol('v', _VarSymbol)
_ = single_symbol('_', _DummyVarSymbol)

## var.a.b.c
class SymbolForm(object):
  def __init__(self, name=None, grammar=None):
    self.__form_name__ = 'var'
    self.__form_grammar__ = None
    self.__symbols__ = []
  def __getattr__(self, name):
    self.__symbols__.append(_VarSymbol(name))
    return self
  def __len__(self): return len(self.__symbols__)
  def __iter__(self): return iter(self.__symbols__)
var = lead(SymbolForm)

@builtin.function('getvar')
def getvar(name, klass=Var): 
  return varcache(name, klass)

# my.a, globl.a
## my = element(some(getattr(__._), L('local', __._), y)+eos)
## globl = element(some(getattr(__._), L('globl', __._), y)+eos)

## use_item = attr|div(var)|div(str)
## use = 'use'+some(use_item)+mayional(use_block)|any(use_item)+use_block\
##          |some(use_item)+div+use_block

# put.a = 1, put.i.j==(1,2)
put = element('put',
  # put.i.j<<(1,2)
  (getattr(__._)+assign(vv.x, getvar(__._)))[1:]%vv.x*vv.vars
        +lshift(vv.value)+eos
        +pycall(special.set_list, vv.vars, pycall(preparse,vv.value))
  )

@builtin.function('make_begin')
def make_begin(body):
  return special.begin(*preparse(body))

do = element('do',
  getitem_to_list(vv.body)+eos+make_begin(vv.body))

_do, _of, _at, _loop, _always = words('do, of, at, loop, always')

def get_let_vars(binary, klass):
  if not isinstance(binary.y, _VarSymbol): raise DinpySyntaxError()
  if isinstance(binary.x, _VarSymbol): return (binary.x, binary.y)
  if isinstance(binary.x, klass): 
    return get_let_vars(binary.x, klass)+(binary.y,)
  raise DinpySyntaxError()

@builtin.function('make_let')
def make_let(args, body, function): 
  bindings = []
  for b in args:
    if not isinstance(b, dexpr._lshift): raise DinpySyntaxError()
    vars, value = b.x, preparse(b.y)
    if isinstance(vars, _VarSymbol): bindings.append((preparse(vars), value))
    else: 
      if isinstance(vars, dexpr._lshift): 
        vars =  get_let_vars(vars, dexpr._lshift)
        i = len(vars)-1
        v2 = varcache(vars[i].name)
        bindings.append((v2, value))
        while i>0:
          v1 = varcache(vars[i-1].name)
          bindings.append((v1, v2))
          v2 = v1
          i -= 1
      elif isinstance(vars, dexpr._div): 
        bindings += [(varcache(v.name), value) for v, value 
                         in zip(get_let_vars(vars, dexpr._div), value)]
      else: raise DinpySyntaxError()
  body = preparse(body)
  if isinstance(body, tuple): return function(bindings, *body)
  else: return function(bindings, body)

def let_grammar(function):
  return (call(vv.bindings)+_do+getitem(vv.body)+eos
    +make_let(vv.bindings, vv.body, function))
    
# let (var==value).do[...]
# let (v==v==value).do[...]
# let ([v,v]==value).do[...]
let = element('let',let_grammar(special.let))

# letr (var << value).do[...]
# letr (v1 << v2 << value).do[...]
# letr ([v1,v2]<<value).do[...]
letr = element('letr',let_grammar(special.letr))

@builtin.function('make_iff')
def make_iff(test, clause, clauses, els_clause):
  els_clause = preparse(els_clause) if not isinstance(els_clause, Var) else None
  test = preparse(test[0])
  clause = preparse(clause)
  clauses1 =  [(preparse(t), preparse(c)) for t, c in clauses]
  return special.iff([(test, clause)]+clauses1, els_clause)

_then, _elsif, _els = words('then, elsif, els')
_test, _test2, _body =  dummies('_test, _test2, _body')

# iff(1).then[2], iff(1).then[2]  .elsif(3).then[4] .els[5]
iff = element('iff',
              (call(vv.test)+_do+getitem(vv.clause)
              +any(_elsif+call(_test)+_do+getitem(_body)+is_(_test2, first(_test)), 
                   (_test2, _body), vv.clauses)
              +may(_els+getitem(vv.els_clause))+eos
              +make_iff(vv.test, vv.clause, vv.clauses, vv.els_clause))
              )
              

class CASE_ELS: pass
CASE_ELS = CASE_ELS()

@builtin.function('make_case')
def make_case(test, cases): 
  case_dict = {}
  for case, clause in cases:
    case = preparse(case)
    if case is CASE_ELS: 
      els_clause = preparse(clause) if clause is not None else None
    else:
      clause = preparse(clause)
      for x in case: case_dict[x] = clause
  return special.CaseForm(preparse(test[0]), case_dict, els_clause)

of_fun = attr_call('of')

# case(x).of(1)[write(1)].of(2,3)[write(4)].els[write(5)]
case = element('case',
  call(vv.test)+(
  #.of(1)[write(1)].of(2,3)[write(4)].els[write(5)]
    (some(of_fun(__.values)+getitem_to_list(__.clause),(__.values,__.clause), vv.clauses)
     +may(_els+getitem_to_list(vv.els))+eos
    +make_case(vv.test, list_tail(vv.clauses, pytuple(CASE_ELS, getvalue_default(vv.els)))))
  #/{1:[write(1)],2:[write(4)],3:[write(4)], els:[write(5)]}
##  | div(vv.clauses)+eos+make_case(vv.test, items(vv.clauses))
  ))
els = CASE_ELS

@builtin.function('make_loop')
def make_loop(body): 
  return special.LoopForm(preparse(body))

@builtin.function('make_loop_times')
def make_loop_times(body, times):
  return special.LoopTimesForm(preparse(times[0]), preparse(body))

@builtin.function('make_loop_when')
def make_loop_when(body, test):
  return special.LoopWhenForm(preparse(body), preparse(test[0]))

@builtin.function('make_loop_until')
def make_loop_until(body, test):
  return special.LoopUntilForm(preparse(body), preparse(test[0]))

when_fun = attr_call('when')
until_fun = attr_call('until')

# loop[write(1)], loop(1)[write(1)]
# loop [write(1)].until(1), loop[write(1)].when(1)
loop = element('loop',
  (  # loop(1)[write(1)]
    (call(vv.times)+getitem_to_list(vv.body)+eos
      +make_loop_times(vv.body, getvalue_default(vv.times)))
    # loop[...], loop[...].when(), loop[...].until() 
  | ( getitem_to_list(vv.body)+
      ( # loop [...] # infinite loop
        ( eos+make_loop(vv.body))
        # loop[...].when(), loop[...].until() 
      | ( # .when(1)
          ( when_fun(vv.test)+eos
              +make_loop_when(vv.body, vv.test))
          #.until(1)
         |( until_fun(vv.test)+eos
              +make_loop_until(vv.body, vv.test) ) )  
  ) ) ) ) 

@builtin.function('make_when_loop')
def make_when_loop(test, body):
  return special.WhenLoopForm(preparse(test[0]), preparse(body))

# when(x>1).loop[write(1)]
when = element('when',
  call(vv.test)+_loop+getitem_to_list(vv.body)+eos+
  make_when_loop(vv.test, vv.body))

@builtin.function('make_each1')
def make_each(vars, iterators, body):
  def tran_iterator(iterator):
    if isinstance(iterator, slice): 
      start = preparse(iterator.start)
      stop = preparse(iterator.stop)
      step = preparse(iterator.step)
      iterator = range(start, stop, 1 if step is None else step)
    else: iterator = preparse(iterator)
    return iterator
  if len(vars)==0: raise DinpySyntaxError()
  for x in vars: 
    if not isinstance(x, _VarSymbol): raise DinpySyntaxError()
  vars = preparse(vars)
  if len(vars)==1: 
    if len(iterators)!=1: raise DinpySyntaxError()
    if not isinstance(iterators[0], slice) and len(iterators[0])!=1: 
      raise DinpySyntaxError()
    iterator = tran_iterator(iterators[0]) 
    return special.EachForm(vars[0], iterator, preparse(body))
  else:
    if len(iterators)==1:
      return special.EachForm(vars, iterators[0], preparse(body))
    iterators1 = []
    for iterator in iterators:
      if isinstance(iterator, slice):
        start = preparse(iterator.start)
        stop = preparse(iterator.stop)
        step = preparse(iterator.step)
        iterators1.append(range(start, stop, 1 if step is None else step))
      else: iterators1.append(preparse(iterator))
    return special.EachForm(vars, zip(*iterators1), preparse(body))

# each(i,j)[1:10][1:10]. do[write(i, j)]
# each(i,j)[zip(range(5), range(5))]. do [write(i,j)],
# each(i,j)[range(5)][range(5)]. do [write(i,j)],
each = element('each',
    call(vv.vars)+some(getitem(__.iterator), __.iterator, vv.iterators)
               +_loop+getitem_to_list(vv.body)+eos
    +make_each(vv.vars, vv.iterators, vv.body))

@builtin.function('make_exit')
def make_exit(type, level, label, value):
  type = None if isinstance(type, Var) else type
  level = 0 if isinstance(level, Var) else level
  label = None if isinstance(label, Var) else label
  return special.exit(preparse(value), type, level, label)

# exit.loop^2 >> 3,  eixt/a>>3
exit = element('exit', 
         ((may(div(vv.type))+may(mul(vv.level)))|may(getattr(vv.label)))
             +may(rshift(vv.value))+eos
             +make_exit(vv.type, vv.level, vv.label, getvalue_default(vv.value)))

@builtin.function('make_next')
def make_next(type, level, label):
  type = None if isinstance(type, Var) else type
  level = 0 if isinstance(level, Var) else level
  label = None if isinstance(label, Var) else label
  return special.next(type, level, label)

next = element('next', 
        ((may(div(vv.type))+may(mul(vv.level)))|may(getattr(vv.label)))+eos
        +make_next(vv.type, vv.level, vv.label))

@builtin.function('set_loop_label')
def set_loop_label(label, body):
  body = preparse(body)
  if not isinstance(body, special.RepeatForm): raise DinpySyntaxError()
  body.label = label
  return body

label = element('label', getattr(vv.name)+mod(vv.body)+eos
          +set_loop_label(vv.name, vv.body))

@builtin.function('make_block')
def make_block(name, body):
  body = tuple(preparse(x) for x in body)
  return special.block(name, *body)

block = element('block', getattr(vv.name)+getitem_to_list(vv.body)+eos
                +make_block(vv.name, vv.body))

@builtin.function('make_catch')
def make_catch(tag, body):
  if len(tag)!=1:  raise DinpySyntaxError()
  return special.catch(preparse(tag[0]), *preparse(body))

catch = element('catch', call(vv.tag)+_do+getitem_to_list(vv.body)+eos
                  +make_catch(vv.tag, vv.body))
                  
@builtin.function('make_throw')
def make_throw(tag, form):
  if len(tag)!=1:  raise DinpySyntaxError()
  if len(form)!=1:  form = special.begin(*preparse(form))
  else: form = preparse(form[0])
  return special.throw(preparse(tag[0]), form)

throw = element('throw', call(vv.tag)+_do+getitem_to_list(vv.form)+eos
                  +make_throw(vv.tag, vv.form))
                  
@builtin.function('make_protect')
def make_protect(form, cleanup):
  if len(form)!=1:  form = special.begin(*preparse(form))
  else: form = preparse(form[0])
  return special.unwind_protect(form, *preparse(cleanup))

protect = element('protect', getitem_to_list(vv.form)+
                  _always+getitem_to_list(vv.cleanup)+eos
                  +make_protect(vv.form, vv.cleanup))
                  
@builtin.function('make_pycall')
def make_pycall(args):
  args = tuple(preparse(x) for x in args)
  return pycall(args[0], args[1:])

py = element('py', 
       ( getattr(vv.func)+assign(vv.func, getvar(vv.func))+eos
           +pycall(vv.func, vv.args))
       | + call(vv.args)+eos+make_pycall(vv.args))

@builtin.function('make_on_form')
def make_on_form(form, body):
  form = preparse(form)
  body = tuple(preparse(x) for x in body)
  return special.OnForm(form, *body)

# with statements in dao
on = element('on', call(vv.form)+_do+getitem(vv.body)+eos+
    pycall(special.OnForm, vv.form, vv.body)) 

class AtForm:
  def __init__(self, clauses):
    self.clauses = clauses
  def __eq__(self, other): 
    return self.clauses==other.clauses
  def __repr__(self): return 'AtForm(%s)'%repr(self.clauses)
    
@builtin.function('make_AtForm')
def make_AtForm(args_bodies):
  return AtForm(preparse(args_bodies))

# at(*args)[...](*args)[...][...]
# at[...][...]
at = element('at',
  some(may(call(__.args))+assign(__.args, getvalue_default(__.args))
       +some(getitem_to_list(__.body), __.body, __.bodies), 
        (__.args, __.bodies), vv.args_bodies)+eos
        +make_AtForm(vv.args_bodies))

from dao.builtins.rule import replace_def, remove, append_def, insert_def, \
     abolish, retractall, retract

# fun. a(x)[...]
@builtin.function('make_fun1')
def make_fun1(name, rules, klass):
  fun = varcache(name)
  if len(rules)==0:
    return replace_def(fun, (), [])
  if len(rules)==1:
    return replace_def(fun, preparse(rules[0][0]), preparse(rules[0][1]), klass)
  replaces = []
  for head, bodies in rules:
    head = preparse(head)
    bodies = preparse(bodies)
    replaces .append(replace_def(fun, head, bodies, klass))
  return special.begin(*replaces)  

# fun. a(x) >= [...], fun.a(x) >= at[...]...
@builtin.function('make_fun2')
def make_fun2(name, args, body, klass): 
  fun = varcache(name)
  head = args
  body = preparse(body)
  if isinstance(body, AtForm): 
    body = body.clauses
    if len(body)>1: raise DinpySyntaxError()
    if body[0][0] is not None: raise DinpySyntaxError()
    return append_def(fun, head,  body[0][1], klass)
  else: return append_def(fun, head, [body], klass)
  
# fun. a(x) <= [...]
@builtin.function('make_fun3')
def make_fun3(name, args, body, klass): 
  fun = varcache(name)
  head = args
  body = preparse(body)
  if isinstance(body, AtForm): 
    body = body.clauses
    if len(body)>1: raise DinpySyntaxError()
    if body[0][0] is not None: raise DinpySyntaxError()
    return insert_def(fun, head,  body[0][1], klass)
  else: return insert_def(fun, head, [body], klass)
    
#  fun. a== [...], fun. a== at(..)[...][...](...)[...][...]
@builtin.function('make_fun4')
def make_fun4(name, rules, klass): 
  fun = varcache(name)
  rules = preparse(rules)
  if isinstance(rules, AtForm): 
    rules1 = []
    for head, bodies in rules.clauses:
      if head is None: head = ()
      for body in bodies:
        rules1.append((head,)+tuple(body))
    return assign(fun, klass(*rules1))
  elif isinstance(rules, list):
    return assign(fun, klass(((), rules)))
  else: raise DinpySyntaxError()
  
#  fun. a>= [...]
@builtin.function('make_fun5')
def make_fun5(name, rules, klass): 
  fun = varcache(name)
  rules = preparse(rules)
  if isinstance(rules, AtForm): 
    clauses = [(head if head is not None else (), bodies) 
              for head, bodies in rules.clauses]
    return special.begin(*[append_def(fun, head, bodies, klass) 
                         for head, bodies in clauses])
  elif isinstance(rules, list):
    return append_def(fun, head, [rules], klass) 
  else: raise DinpySyntaxError()
  
#  fun. a<= [...]
@builtin.function('make_fun6')
def make_fun6(name, rules, klass): 
  fun = varcache(name)
  rules = preparse(rules)
  if isinstance(rules, AtForm): 
    clauses = [(head if head is not None else (), bodies) 
              for head, bodies in rules.clauses]
    return special.begin(*[insert_def(fun, head, bodies, klass) 
                         for head, bodies in clauses])
  elif isinstance(rules, list):
    return insert_def(fun, head, [rules], klass) 
  else: raise DinpySyntaxError()
  
#  fun[...]
@builtin.function('make_fun7')
def make_fun7(clauses, klass): 
  rules = []
  for head, bodies in clauses:
    if head is None: head = ()
    for body in bodies: 
      body = preparse(body)
      rules.append((preparse(head),)+tuple(body))
  return klass(*rules)
  
#  - fun.a(x),
@builtin.function('make_fun8')
def make_fun8(name, args, klass): # remove
  fun = varcache(name)
  args = preparse(args)
  return remove(fun, args, klass)
  
## fun. a(x) [...][...], 覆盖与a(x)匹配的整个定义
## fun. a(x) >= [...],  fun. a(x) >= at[...][...], 对参数组附加定义
## fun. a(x) <= [...],  fun. a(x) <= at[...][...]，对参数组前补定义
## fun. a== at(..)[...](..)[...][...]，重定义
## fun. a>= at(..)[...](..)[...][...]，附加定义
## fun. a<= at(..)[...](..)[...][...]，前补定义
## fun(..)[...](..)[...][...]
def fun_macro_grammar(klass1, klass2):
  return (
  # fun. a(x)[...],  fun. a(x) <= at[...][...]
    (getattr(vv.name)+
     any(may(call(__.args)) +assign(__.args, getvalue_default(__.args, ()))
          + some(getitem_to_list(__.body), __.body, __.bodies), 
          (__.args, __.bodies), vv.rules)
        +eos+make_fun1(vv.name, vv.rules, klass2))
  # fun. a(x) >= [...],  fun. a(x) <= at[...][...]
  | (getattr(vv.name)+call(vv.args)+ge(vv.body)+eos
     +make_fun2(vv.name,vv.args, vv.body, klass2))
  # fun. a(x) <= [...],  fun. a(x) <= at[...][...]
  | (getattr(vv.name)+call(vv.args)+le(vv.body)+eos
     +make_fun3(vv.name,vv.args, vv.body, klass2))
  #  fun. a== at(..)[...]
  | (getattr(vv.name)+eq(vv.rules)+eos
     +make_fun4(vv.name, vv.rules, klass1))
  #  fun. a>= at(..)[...]
  | (getattr(vv.name)+ge(vv.rules)+eos+make_fun5(vv.name,vv.rules, klass2))
  #  fun. a<= at(..)[...]
  | (getattr(vv.name)+le(vv.rules)+eos+make_fun6(vv.name,vv.rules, klass2))
  #  fun(args) [...](args)[...][...]
  | (some(may(call(__.args)) +assign(__.args, getvalue_default(__.args))
          + some(getitem_to_list(__.body), __.body, __.bodies), 
          (__.args, __.bodies), vv.rules)
        +eos+make_fun7(vv.rules, klass1))
  #   - fun.a/3,
  | (getattr(vv.name)+neg+div(vv.arity)+eos
     +pycall(abolish, getvar(vv.name), vv.arity))
  #- fun.a(x),
  | (getattr(vv.name)+call(vv.args)+neg+eos
     +make_fun8(vv.name, vv.args, klass2)) #retractall
  #- fun.a(x)[1],
##  | (getattr(vv.name)+call(vv.args)+getitem(vv.index)+neg+assign(result, retract(vv.name, vv.args)))
  )

fun = element('fun', fun_macro_grammar(special.FunctionForm, special.UserFunction))
macro = element('macro', fun_macro_grammar(special.FunctionForm, special.UserMacro))
