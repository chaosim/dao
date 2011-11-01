# -*- coding: utf-8 -*-

from dao.term import deref, getvalue, unify, CommandCall, Symbol, Var
from dao.builtin import builtin, BuiltinMacro
from dao.solve import mycont
from dao.builtins.term import is_
from dao.builtins.control import or_p, and_p
from dao import special
from dao.builtin import builtin, BuiltinFunction2
from dao import base
from dao.base import get_first_set
from dao.builtin import first_set

from dao.solve import run_mode, interactive
from dao.solve import interactive_solver, interactive_tagger, interactive_parser

# TODO: longest: the longest matcher.

# parser predicate
# optional, any, some, times, seplist, ...

# lazy: match at least, except matchers that followed fail. 尽量少吃进，除非别人逼我多吃一点
# nongreedy, match at most, throw out if matchers followed fail.先尽量多吃，如果别人要再吐出来
# greedy, match at most, don,t throw out even matchers followed fail. 吃进去了就不会吐出来。
lazy, nongreedy, greedy = range(3)

def slice_stop_default():
  class A:
    def __getitem__(self, other): return other.stop
  return A()[:]
slice_stop_default = slice_stop_default()

def slice_step_default():
  class A:
    def __getitem__(self, other): return other.step
  return A()[:]
slice_step_default = slice_step_default()

class Matcher(Symbol):
  '''
  a+b: MatcherSequence;
  a|b: MatcherOr;
  ~a: optional(a, greedy)
  -a: optional(a, nongreedy)'''
  def __getitem__(self, index):
    if not isinstance(index, slice):
      if index==0: raise ValueError(index)
      if index==1: return self
      return Repeater(self, None, index, index)
    elif isinstance(index, slice): 
      if index.step!=slice_step_default: raise ValueError(index)
      if index.start>index.stop: raise ValueError(index)
      stop = None if index.stop==slice_stop_default else  index.stop
      return Repeater(self, None, index.start, stop)
    raise ValueError(index)
  def __add__(self, other):
    if isinstance(other, MatcherSequence):
      return MatcherSequence([self]+other.exps)
    return MatcherSequence([self, other])
  def __or__(self, other):
    if isinstance(other, MatcherOr):
      return MatcherOr([self]+other.exps)
    return MatcherOr(self, other)
  def __neg__(self): 
    if self is not optional: return optional(self, nongreedy)
    raise TypeError(self)
  def __invert__(self):
    if self.operator is not optional: return optional(self, greedy)
    raise TypeError(self)

class Repeater(Matcher):
  '''nongreedy: matcher[:]/separator%template*result
  greedy: ~matcher[:]/separator%template*result
  lazy: -matcher[:]/separator%template*result'''
  def __init__(self, item, separator=None, min=None, max=None, template=None, result=None, mode=nongreedy):
    self.item, self.separator, self.min, self.max = item, separator, min, max
    self.template, self.result, self.mode = template, result, mode
    
  def ___parse___(self, parser):
    item = parser.parse(self.item)
    separator = parser.parse(self.separator)
    min = parser.parse(self.min)
    max = parser.parse(self.max)
    template = parser.parse(self.template)
    result = parser.parse(self.result)
    mode = parser.parse(self.mode)    
    if separator is None:
      if min is not None:
        if max is not None:
          if min==max:
            return times(item, min, template, result, mode)
          else:
            return times_between(item, min, max, template, result, mode)
        else:
          if min==0: return any(item, template, result, mode)
          elif min==1: return some(item, template, result, mode)
          return times_more(item, min, template, result, mode)
      else:
        if max is not None:
          return times_less(item, max, template, result, mode)
        else: return any(item, template, result, mode)
    else:
      if min is not None:
        if max is not None:
          if min==max:
            return seplist(item, separator, template, result, min, mode)
          else:
            return seplist_times_between(item, separator, min, max, template, result, mode)
        else:
          if min==0: return seplist(item, separator, template, result, None, mode)
          return seplist_times_more(item, separator, min, template, result, mode)
      else:
        if max is not None:
          return seplist_times_less(item, separator, max, template, result, mode)
        else: return seplist(item, separator, template, result, None, mode)
  def __div__(self, separator):
    '''separator'''
    if self.separator is  None:
      self.separator = separator
      return self
    raise TypeError(self)
  def __mod__(self, template):
    '''template'''
    if self.template is None:
      self.template = template
      return self
    raise TypeError(self)
  def __mul__(self, result):
    '''result'''
    if self.result is None:
      self.result = result
      return self
    raise TypeError(self)
  def __invert__(self):
    '''greedy mode'''
    self.mode = greedy
    return self
  def __neg__(self):
    '''lazy mode'''
    self.mode = lazy
    return self
  def __repr__(self):
    if run_mode() is interactive:
      code = interactive_parser().parse(self)
      code = interactive_tagger().tag_loop_label(code)
      result = interactive_solver().eval(code)
      return repr(result) if result is not None else '' if result is not None else ''
    return ''.join([repr(x) for x in 
          [self.item, self.separator, self.min, self.max, 
          self.template, self.result, self.mode]])

class BuiltinMatcher(BuiltinMacro):
  def __call__(self, *exps): return BuiltinMatcherCall(self, *exps)

class BuiltinMatcherCall(Matcher, CommandCall): pass
  #def first_set(self, solver, memo):
    #try: self_first_func = self.first_set_func
    #except: return solver.universal_set # return set([base.null])
    #return self.first_set_func(self, solver, memo, *self.operand)


matcher = builtin(BuiltinMatcher)

def convert_to_matcher(builtin):
  builtin.__class__ = BuiltinMatcher
  
class MatcherSequence(Matcher):
  def __init__(self, exps):
    self.exps = exps
  def __add__(self, other):
    if isinstance(other, MatcherSequence):
      return MatcherSequence(self.exps+other.exps)
    return MatcherSequence(self.exps+[other])
  def ___parse___(self, parser):
    return special.begin(*[parser.parse(e) for e in self.exps])
    
class MatcherOr(Matcher):
  def __init__(self, call1, call2):
    self.call1 = call1
    self.call2 = call2
  def ___parse___(self, parser):
    return or_p(parser.parse(self.call1), parser.parse(self.call2))

@first_set(lambda self, solver, memo: set([base.null]))
@matcher()
def nullword(solver, cont): 
  yield cont,  True
null = nullword = nullword()

def optional_first_set(self, solver, memo, item, mode): 
  return first_set(item, solver, memo)|base.nullset

@first_set(lambda self, solver, memo, item,template=None, result=None, mode=nongreedy:
           get_first_set(item, solver, memo)|base.nullset)
@matcher()
def optional(solver, cont, item, mode=nongreedy):
  item_matched = [False]
  @mycont(cont)
  def optional_cont(value, solver):
    item_matched[0] = True
    yield cont, value
  yield solver.cont(item, optional_cont), True
  if mode!=greedy  or (mode==greedy and not item_matched[0]):
    yield cont, True
  
may = optional

@first_set(lambda self, solver, memo, call1, call2:
           get_first_set(call1, solver, memo)&get_first_set(call2, solver, memo))
@matcher()
def parallel(solver, cont, call1, call2):
  parse_state = solver.parse_state
  @mycont(cont)
  def pallel_cont(value1, solver):
    right = solver.parse_state[1]
    solver.parse_state = parse_state
    @mycont(cont)
    def pallel_finish_cont(value, solver):
      if solver.parse_state[1]==right: yield cont, value
    yield solver.cont(call2, pallel_finish_cont), value1
  yield solver.cont(call1, pallel_cont), True

##@matcher() # not necessary be a builtin.
##def longest(solver, cont, call):
##  parse_state = solver.parse_state
##  right = -1
##  for c, value in solver.exp_run_cont(call, cont):
##    if solver.parse_state[1]<=right: continue
##    right = solver.parse_state[1]
##    bindings = solver.env.binings.copy()
##    value1 = value
##  if right==-1: return
##  solver.parse_state = parse_state[0], right
##  solver.env.binings = bindings
##  yield cont,value1
##

def greedy_repeat_cont(item, cont):
  @mycont(cont)
  def repeat_cont(value, solver):
    matched[0] = False
    @mycont(cont)
    def tyr_repeat_cont(value, solver):
      matched[0] = True
      yield repeat_cont, value
    yield solver.cont(item, tyr_repeat_cont), value
    if matched[0]: return
    yield cont, value
  return repeat_cont

def nongreedy_repeat_cont(item, cont):
  @mycont(cont)
  def repeat_cont(value, solver):
    yield solver.cont(item, repeat_cont), True 
    yield cont, True
  return repeat_cont

def lazy_repeat_cont(item, cont):
  @mycont(cont)
  def repeat_cont(value, solver):
    yield cont, True
    yield solver.cont(item, repeat_cont), True 
  return repeat_cont

def greedy_repeat_result_cont(item, cont, matched_times, matched_list, template, result):
  @mycont(cont)
  def repeat_cont(value, solver):
    matched = [False]
    if matched_times>0: matched_list.append(getvalue(template, solver.env))
    next_cont = greedy_repeat_result_cont(item, cont, matched_times+1, 
                                          matched_list, template, result)
    @mycont(next_cont)
    def try_repeat_cont(value, solver):
      matched[0] = True
      yield next_cont, True
    yield solver.cont(item, try_repeat_cont), value
    if matched[0]: return
    for _ in unify(result, matched_list, solver.env): 
      yield cont, True
  return repeat_cont

def nongreedy_repeat_result_cont(item, cont, matched_times, matched_list, template, result):
  @mycont(cont)
  def repeat_cont(value, solver):
    if matched_times>0: 
      matched_list1 = matched_list+[getvalue(template, solver.env)]
    else: matched_list1 = matched_list
    next_cont = nongreedy_repeat_result_cont(item, cont, 
                  matched_times+1, matched_list1, template, result)
    yield solver.cont(item, next_cont), True
    for _ in unify(result, matched_list1, solver.env): 
      yield cont, True
  return repeat_cont

def lazy_repeat_result_cont(item, cont, matched_times, matched_list, template, result):
  @mycont(cont)
  def repeat_cont(value, solver):
    if matched_times>0: matched_list.append(getvalue(template, solver.env))
    for _ in unify(result, matched_list, solver.env): 
      yield cont, True
    next_cont = lazy_repeat_result_cont(item, cont, matched_times+1, matched_list, template, result)
    yield solver.cont(item, next_cont), True
  return repeat_cont
  

def any(item, template=None, result=None, mode=nongreedy):   
  if result is None:
    if mode==greedy:
      @first_set(lambda self, solver, memo, item:
           get_first_set(item, solver, memo)|base.nullset)
      @matcher('any')
      def any_bultin(solver, cont, item):  
        yield greedy_repeat_cont(deref(item, solver.env), cont), []
      return any_bultin(item)
    elif mode==nongreedy:
      @first_set(lambda self, solver, memo, item:
           get_first_set(item, solver, memo)|base.nullset)
      @matcher('any')
      def any_bultin(solver, cont, item):  
        yield nongreedy_repeat_cont(deref(item, solver.env), cont), []
      return any_bultin(item)
    else:# mode==lazy:
      @first_set(lambda self, solver, memo, item:
           get_first_set(item, solver, memo)|base.nullset)
      @matcher('any')
      def any_bultin(solver, cont, item):  
        yield lazy_repeat_cont(deref(item, solver.env), cont), []
      return any_bultin(item)
  else:
    if mode==greedy:
      @first_set(lambda self, solver, memo, item,template, result:
           get_first_set(item, solver, memo)|base.nullset)
      @matcher('any')
      def any_bultin(solver, cont, item, template, result):  
        yield greedy_repeat_result_cont(
          deref(item, solver.env), cont, 0, [], 
          deref(template, solver.env), deref(result, solver.env)), []
      return any_bultin(item, template, result)
    elif mode==nongreedy:
      @first_set(lambda self, solver, memo, item, template, result:
           get_first_set(item, solver, memo)|base.nullset)
      @matcher('any')
      def any_bultin(solver, cont, item, template, result):  
        yield nongreedy_repeat_result_cont(
          deref(item, solver.env), cont, 0, [], 
          deref(template, solver.env), deref(result, solver.env)), []
      return any_bultin(item, template, result)
    else: # mode==lazy
      @first_set(lambda self, solver, memo, item, template, result:
           get_first_set(item, solver, memo)|base.nullset)
      @matcher('any')
      def any_bultin(solver, cont, item, template, result):  
        yield lazy_repeat_result_cont(
          deref(item, solver.env), cont, 0, [], 
          deref(template, solver.env), deref(result, solver.env)), []
      return any_bultin(item, template, result)

def make_repeat_cont(solver, cont, item, matched_times, matched_list, template, result, mode):
  if result is None:
    if mode==greedy:
      return greedy_repeat_cont(deref(item, solver.env), cont)
    elif mode==nongreedy:
      return nongreedy_repeat_cont(deref(item, solver.env), cont)
    else:# mode==lazy:
      return lazy_repeat_cont(deref(item, solver.env), cont)
  else:
    if mode==greedy:
      return greedy_repeat_result_cont(
          deref(item, solver.env), cont, matched_times, matched_list, 
          deref(template, solver.env), deref(result, solver.env))
    elif mode==nongreedy:
      return nongreedy_repeat_result_cont(
          deref(item, solver.env), cont, matched_times, matched_list, 
          deref(template, solver.env), deref(result, solver.env))
    else: # mode==lazy
      return lazy_repeat_result_cont(
          deref(item, solver.env), cont, matched_times, matched_list, 
          deref(template, solver.env), deref(result, solver.env))
  
def some(item, template=None, result=None, mode=nongreedy):   
  if result is None:
    if mode==greedy:
      @first_set(lambda self, solver, memo, item:
           get_first_set(item, solver, memo))
      @matcher('some')
      def some_bultin(solver, cont, item):  
        yield solver.cont(item, greedy_repeat_cont(
          deref(item, solver.env), cont)), []
      return some_bultin(item)
    elif mode==nongreedy:
      @first_set(lambda self, solver, memo, item:
           get_first_set(item, solver, memo))
      @matcher('some')
      def some_bultin(solver, cont, item):  
        yield solver.cont(item, nongreedy_repeat_cont(
          deref(item, solver.env), cont)), []
      return some_bultin(item)
    else:# mode==lazy:
      @first_set(lambda self, solver, memo, item:
           get_first_set(item, solver, memo))
      @matcher('some')
      def some_bultin(solver, cont, item):  
        yield solver.cont(item, lazy_repeat_cont(
          deref(item, solver.env), cont)), []
      return some_bultin(item)
  else:
    if mode==greedy:
      @first_set(lambda self, solver, memo, item, template, result:
           get_first_set(item, solver, memo))
      @matcher('some')
      def some_bultin(solver, cont, item, template, result):  
        yield solver.cont(item, greedy_repeat_result_cont(
          deref(item, solver.env), cont, 1, [], 
          deref(template, solver.env), deref(result, solver.env))), []
      return some_bultin(item, template, result)
    elif mode==nongreedy:
      @first_set(lambda self, solver, memo, item, template, result:
           get_first_set(item, solver, memo))
      @matcher('some')
      def some_bultin(solver, cont, item, template, result):  
        yield solver.cont(item, nongreedy_repeat_result_cont(
          deref(item, solver.env), cont, 1, [], 
          deref(template, solver.env), deref(result, solver.env))), []
      return some_bultin(item, template, result)
    else: # mode==lazy
      @first_set(lambda self, solver, memo, item, template, result:
           get_first_set(item, solver, memo))
      @matcher('some')
      def some_bultin(solver, cont, item, template, result):  
        yield solver.cont(item, lazy_repeat_result_cont(
          deref(item, solver.env), cont, 1, [], 
          deref(template, solver.env), deref(result, solver.env))), []
      return some_bultin(item, template, result)

def greedy_times_cont(item, expect_times, cont, matched_times):
  if isinstance(expect_times, int):
    @mycont(cont)
    def times_cont(value, solver):
      if expect_times==matched_times: yield cont, True
      else: yield solver.cont(item, 
        greedy_times_cont(item, expect_times, cont, matched_times+1)), True
  else:
    @mycont(cont)
    def times_cont(value, solver):
      matched = [False]
      next_cont = nongreedy_times_cont(item, expect_times, cont, matched_times+1)
      @mycont(next_cont)
      def try_times_cont(value, solver):
        matched[0] = True
        yield next_cont, value
      yield solver.cont(item, try_times_cont), value
      if matched[0]: return
      for _ in unify(expect_times, matched_times, solver.env): 
        yield cont, True
  return times_cont

def nongreedy_times_cont(item, expect_times, cont, matched_times):
  if isinstance(expect_times, int):
    @mycont(cont)
    def times_cont(value, solver):
      if expect_times==matched_times: yield cont, True
      else: yield solver.cont(item, 
        nongreedy_times_cont(item, expect_times, cont, matched_times+1)), True
  else:
    @mycont(cont)
    def times_cont(value, solver):
      yield solver.cont(item, nongreedy_times_cont(item, expect_times, cont, matched_times+1)), True
      for _ in unify(expect_times, matched_times, solver.env): 
        yield cont, True
  return times_cont

def lazy_times_cont(item, expect_times, cont, matched_times):
  if isinstance(expect_times, int):
    @mycont(cont)
    def times_cont(value, solver):
      if expect_times==matched_times: yield cont, True
      else: yield solver.cont(item, 
        lazy_times_cont(item, expect_times, cont, matched_times+1)), True
  else:
    @mycont(cont)
    def times_cont(value, solver):
      for _ in unify(expect_times, matched_times, solver.env): 
        yield cont, True
      next_cont = lazy_times_cont(item, expect_times, cont, matched_times+1)
      for s, x in solver.exp_run_cont(item, next_cont):
        yield c, x 
  return times_cont

def greedy_times_result_cont(item, expect_times, cont, matched_times, 
      matched_list, template, result):
  if isinstance(expect_times, int):
    @mycont(cont)
    def times_cont(value, solver):
      if matched_times>0: matched_list.append(getvalue(template, solver.env))
      if expect_times==matched_times:
        for _ in unify(result, matched_list, solver.env):
          yield cont, True
          return
      else: yield solver.cont(item, greedy_times_result_cont(item, expect_times, cont, 
              matched_times+1, matched_list, template, result)), True
  else:
    @mycont(cont)
    def times_cont(value, solver):
      if matched_times>0: matched_list.append(getvalue(template, solver.env))
      matched = [False]
      next_cont = greedy_times_result_cont(item, expect_times, cont, 
                    matched_times+1, matched_list, template, result)
      @mycont(next_cont)
      def try_times_cont(value, solver):
        matched[0] = True
        yield next_cont, True
      yield solver.cont(item, try_times_cont), value
      if matched[0]: return
      for _ in unify(expect_times, matched_times, solver.env):
        for _ in unify(result, matched_list, solver.env): 
          yield cont, True
  return times_cont

def nongreedy_times_result_cont(item, expect_times, cont, 
      matched_times, matched_list, template, result):
  if isinstance(expect_times, int):
    @mycont(cont)
    def times_cont(value, solver):
      if matched_times>0: matched_list.append(getvalue(template, solver.env))
      if expect_times==matched_times:
        for _ in unify(result, matched_list, solver.env):
          yield cont, True
          return
      else: yield solver.cont(item, 
              nongreedy_times_result_cont(item, expect_times, cont, 
              matched_times+1, matched_list, template, result)), True
  else:
    @mycont(cont)
    def times_cont(value, solver):
      if matched_times>0: 
        matched_list1 = matched_list+[getvalue(template, solver.env)]
      else: matched_list1 = matched_list
      next_cont = nongreedy_times_result_cont(item, expect_times, cont, 
                    matched_times+1, matched_list1, template, result)
      yield solver.cont(item, next_cont), True
      for _ in unify(expect_times, matched_times, solver.env):
        for _ in unify(result, matched_list1, solver.env): 
          yield cont, True
  return times_cont

def lazy_times_result_cont(item, expect_times, cont, 
      matched_times, matched_list, template, result):
  if isinstance(expect_times, int):
    @mycont(cont)
    def times_cont(value, solver):
      if matched_times>0: matched_list.append(getvalue(template, solver.env))
      if expect_times==matched_times:
        for _ in unify(result, matched_list, solver.env):
          yield cont, True
          return
      else: yield solver.cont(item, greedy_times_result_cont(item, expect_times, cont, 
              matched_times+1, matched_list, template, result)), True
  else:
    @mycont(cont)
    def times_cont(value, solver):
      if matched_times>0: matched_list.append(getvalue(template, solver.env))
      for _ in unify(expect_times, matched_times, solver.env):
        for _ in unify(result, matched_list, solver.env): 
          yield cont, True
      next_cont = lazy_times_result_cont(item, cont, matched_times+1, matched_list, template, result)
      yield solver.cont(item, next_cont), True
  return times_cont

def times_first_set(self, solver, memo, item, expect_times, template=None, result=None):
  expectTimes1 = getvalue(expect_times, solver.env)
  if isinstance(expectTimes1, int):
    if expectTimes1<0: raise Error
    elif expectTimes1==0: return base.nullset
    else: return get_first_set(item, solver, memo)
  else:
    return get_first_set(item, solver, memo)|base.nullset
  
def times(item, expect_times, template=None, result=None, mode=nongreedy):   
  if result is None:
    if mode==greedy:
      @first_set(times_first_set)
      @matcher('times')
      def times_bultin(solver, cont, item, expect_times):
        expectTimes1 = getvalue(expect_times, solver.env)
        if isinstance(expectTimes1, int):
          if expectTimes1<0: raise Error
          elif expectTimes1==0: 
            yield cont, True
            return
        yield greedy_times_cont(item, expectTimes1, cont, 0), True
      return times_bultin(item, expect_times)
    elif mode==nongreedy:
      @first_set(times_first_set)
      @matcher('times')
      def times_bultin(solver, cont, item, expect_times):
        expectTimes1 = getvalue(expect_times, solver.env)
        if isinstance(expectTimes1, int):
          if expectTimes1<0: raise Error
          elif expectTimes1==0: 
            yield cont, True
            return
        yield nongreedy_times_cont(item, expectTimes1, cont, 0), True
      return times_bultin(item, expect_times)
    else:# mode==lazy:
      @first_set(times_first_set)
      @matcher('times')
      def times_bultin(solver, cont, item, expect_times): 
        expectTimes1 = getvalue(expect_times, solver.env)
        if isinstance(expectTimes1, int):
          if expectTimes1<0: raise Error
          elif expectTimes1==0: 
            yield cont, True
            return
        yield lazy_times_cont(item, expectTimes1, cont, 0), True
      return times_bultin(item, expect_times)
  else:
    if mode==greedy:
      @first_set(times_first_set)
      @matcher('times')
      def times_bultin(solver, cont, item, expect_times, template, result):  
        expectTimes1 = getvalue(expect_times, solver.env)
        if isinstance(expectTimes1, int):
          if expectTimes1<0: raise Error
          elif expectTimes1==0: 
            for _ in unify(result, [], solver.env): yield cont, True
            return
        yield greedy_times_result_cont(
          deref(item, solver.env), expectTimes1, cont, 0, [], 
          deref(template, solver.env), deref(result, solver.env)), []
      return times(item, expect_times, expect_times, template, result)
    elif mode==nongreedy:
      @first_set(times_first_set)
      @matcher('times')
      def times_bultin(solver, cont, item, expect_times, template, result): 
        expectTimes1 = getvalue(expect_times, solver.env)
        if isinstance(expectTimes1, int):
          if expectTimes1<0: raise Error
          elif expectTimes1==0: 
            for _ in unify(result, [], solver.env): yield cont, True
            return
        yield nongreedy_times_result_cont(
          deref(item, solver.env), expectTimes1, cont, 0, [], 
          deref(template, solver.env), deref(result, solver.env)), []
      return times_bultin(item, expect_times, template, result)
    else: # mode==lazy
      @first_set(times_first_set)
      @matcher('times')
      def times_bultin(solver, cont, item, expect_times, template, result): 
        expectTimes1 = getvalue(expect_times, solver.env)
        if isinstance(expectTimes1, int):
          if expectTimes1<0: raise Error
          elif expectTimes1==0: 
            for _ in unify(result, [], solver.env): yield cont, True
            return
        yield lazy_times_result_cont(
          deref(item, solver.env), expectTimes1, cont, 0, [], 
          deref(template, solver.env), deref(result, solver.env)), []
      return times_bultin(item, expect_times, template, result)

def times_more_first_set(self, solver, memo, item, expect_times, template=None, result=None):
  if expectTimes1<0: raise Error
  elif expectTimes1==0: return get_first_set(item, solver)|base.nullset
  else: return get_first_set(item, solver)
  
@first_set(times_more_first_set)
@matcher()
def times_more(solver, cont, item, expect_times, template=None, result=None, mode=nongreedy): 
  item = deref(item, solver.env)
  expect_times = getvalue(expect_times, solver.env)
  template = deref(template, solver.env)
  result = deref(result, solver.env)
  if not isinstance(expect_times, int): raise ValueError(expect_times)
  if expect_times<0: raise ValueError(self)
  elif expect_times==0: 
    yield solver.cont(any(item, template, result, mode), cont), True
  elif  expect_times==1: 
    yield solver.cont(some(item, template, result, mode)), True
  else:
    if result is not None: temp_result = Var('temp_result')
    else: temp_result = None
    @mycont(cont)
    def times_more_cont(value, solver):
      matched_list = getvalue(temp_result, solver.env) if result is not None else None
      yield make_repeat_cont(solver, cont, item, 0, matched_list, template, result, mode), value
    yield solver.cont(times(item, expect_times, template, temp_result), times_more_cont), True

def greedy_times_less_cont(item, expect_times, cont, matched_times):
  @mycont(cont)
  def times_cont(value, solver):
    if expect_times==matched_times: yield cont, True
    else:
      next_cont = greedy_times_less_cont(item, expect_times, cont, matched_times+1)
      matched = [False]
      @mycont(next_cont)
      def try_times_cont(value, solver):  
        matched[0] = True
        yield next_cont, value
      yield solver.item(item, try_times_cont)  
      if not matched: yield cont, True
  return times_cont

def nongreedy_times_less_cont(item, expect_times, cont, matched_times):
  @mycont(cont)
  def times_cont(value, solver):
    if expect_times==matched_times: yield cont, True
    else:
      next_cont = nongreedy_times_less_cont(item, expect_times, cont, matched_times+1)
      for c, x in solver.exp_run_cont(item, next_cont):
        yield c, x
      yield cont, True
  return times_cont

def lazy_times_less_cont(item, expect_times, cont, matched_times):
  @mycont(cont)
  def times_cont(value, solver):
    if expect_times==matched_times: yield cont, True
    else:
      yield cont, True
      next_cont = nongreedy_times_less_cont(item, expect_times, cont, matched_times+1)
      for c, x in solver.exp_run_cont(item, next_cont):
        yield c, x      
  return times_cont

def greedy_times_less_result_cont(item, expect_times, cont, matched_times, 
      matched_list, template, result):
  @mycont(cont)
  def times_cont(value, solver):
    if matched_times>0: matched_list.append(getvalue(template, solver.env))
    if expect_times==matched_times:
      for _ in unify(result, matched_list, solver.env):
        yield cont, True
    else: 
      matched = [False]
      next_cont = greedy_times_less_result_cont(item, expect_times, cont, 
                        matched_times+1, matched_list, template, result)
      @mycont(next_cont)
      def try_times_cont(value, solver):
        matched[0] = True
        yield next_cont, value
      yield solver.cont(item, try_times_cont), value
      if not matched:
        for _ in unify(result, matched_list, solver.env):
          yield cont, True
  return times_cont

def nongreedy_times_less_result_cont(item, expect_times, cont, 
      matched_times, matched_list, template, result):
  @mycont(cont)
  def times_cont(value, solver):
    if matched_times>0: matched_list1 = matched_list+[getvalue(template, solver.env)]
    else: matched_list1 = matched_list
    if expect_times==matched_times:
      for _ in unify(result, matched_list1, solver.env):
        yield cont, True
    else: 
      next_cont = nongreedy_times_less_result_cont(item, expect_times, cont, 
                        matched_times+1, matched_list1, template, result)
      for c, x in solver.exp_run_cont(item, next_cont):
        yield c, x
      for _ in unify(result, matched_list1, solver.env):
        yield cont, True
  return times_cont

def lazy_times_less_result_cont(item, expect_times, cont, 
      matched_times, matched_list, template, result):
  @mycont(cont)
  def times_cont(value, solver):
    if matched_times>0: matched_list.append(getvalue(template, solver.env))
    if expect_times==matched_times:
      for _ in unify(result, matched_list, solver.env):
        yield cont, True
    else: 
      for _ in unify(result, matched_list, solver.env):
        yield cont, True
      next_cont = lazy_times_less_result_cont(item, expect_times, cont, 
                        matched_times+1, matched_list, template, result)
      for c, x in solver.exp_run_cont(item, next_cont):
        yield next_cont, x
  return times_cont

def make_times_less_cont(solver, cont, item, expect_times, 
      matched_times, matched_list, template, result, mode):
  if result is None:
    if mode==greedy:
      return greedy_times_less_cont(item, expect_times, cont, 0)
    elif mode==nongreedy:
      return nongreedy_times_less_cont(item, expect_times, cont, 0)
    else:# mode==lazy:
      return lazy_times_less_cont(item, expect_times, cont, 0)
  else:
    if mode==greedy:
      return greedy_times_less_result_cont(
          item, expect_times, cont, matched_times, matched_list, template, result)
    elif mode==nongreedy:
      return nongreedy_times_less_result_cont(
          item, expect_times, cont, matched_times, matched_list, template, result)
    else: # mode==lazy
      return lazy_times_less_result_cont(
          item, expect_times, cont, matched_times, matched_list, template, result)
  
@matcher()
def times_less(solver, cont, item, expect_times, template=None, result=None, mode=nongreedy): 
  item = deref(item, solver.env)
  expect_times = getvalue(expect_times, solver.env)
  template = deref(template, solver.env)
  result = deref(result, solver.env)
  if not isinstance(expect_times, int): raise ValueError(expect_times)
  if expect_times<0: raise ValueError(self)
  elif expect_times==0: 
    for c, _ in solver.exp_run_cont(nullword, cont):
      if result is not None:
        for _ in unify(result, [], solver.env):
          yield c, True
      else: yield cont, True
  else:
    if result is None:
      if mode==greedy:
        yield greedy_times_less_cont(item, expect_times, cont, 0), True
      elif mode==nongreedy:
        yield nongreedy_times_less_cont(item, expect_times, cont, 0), True
      else:# mode==lazy:
        yield lazy_times_less_cont(item, expect_times, cont, 0), True
    else:
      if mode==greedy:
        yield greedy_times_less_result_cont(
            item, expect_times, cont, 0, [], template, result), []
      elif mode==nongreedy:
        yield nongreedy_times_less_result_cont(
            item, expect_times, cont, 0, [], template, result), []
      else: # mode==lazy
        yield lazy_times_less_result_cont(
            item, expect_times, cont, 0, [], template, result), []
        
def times_between(item, min, max, template=None, result=None, mode=nongreedy):
  @matcher('times_between')
  def times_between_builtin(solver, cont, item, template, result):
    min1 = deref(min, solver.env)
    max1 = deref(max, solver.env)
    if not isinstance(min1, int): raise ValueError(min1)
    if not isinstance(max1, int): raise ValueError(max1)
    if min1==0:
      yield solver.cont(time_less(item, max1, template, result, mode), cont), True
    if result is not None:
      temp1 = Var('temp1')
      temp2 = Var('temp2')
    else: 
      temp1 = None
      temp2 = None
    for c, _ in solver.exp_run_cont(times(item, min, template, temp1, mode), cont):
      for c, _ in solver.exp_run_cont(times_less(item, max-min, template, temp2, mode), cont):
        if result is not None:
          matched_list = getvalue(temp1, solver.env)+getvalue(temp2, solver.env)
          for _ in unify(result, matched_list, solver.env):
            yield c, True
        else: yield cont, True
  return times_between_builtin(item, template, result)

def seplist(item, separator, template=None, result=None, 
            expect_times=None, mode=nongreedy):
  if expect_times==0: return nullword
  elif expect_times==1: return item
  if expect_times is None:
    if result is None:
      if mode==greedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont, item, separator):  
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          yield solver.cont(item1, greedy_repeat_cont(
            and_p(separator1, item1), cont)), []
        return seplist_bultin(item, separator)
      elif mode==nongreedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont, item, separator):  
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          yield solver.cont(item1, nongreedy_repeat_cont(
            and_p(separator1, item1), cont)), []
      else:# mode==lazy:
        @matcher('seplist')
        def seplist_bultin(solver, cont, item, separator):  
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          yield solver.cont(item1, lazy_repeat_cont(
            and_p(separator1, item1), cont)), []
        return seplist_bultin(item, separator)
    else:
      if mode==greedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont, item, separator, template, result):  
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          for c, _ in solver.exp_run_cont(item1, cont):
            yield greedy_repeat_result_cont(
            and_p(separator1, item1), c, 1, [], 
            deref(template, solver.env), deref(result, solver.env)), []
            return
          for _ in unify(result, [], solver.env):
            yield cont, True
        return seplist_bultin(item, separator, template, result)
      elif mode==nongreedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont, item, separator, template, result):  
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          yield solver.cont(item1, nongreedy_repeat_result_cont(
            and_p(separator1, item1), cont, 1, [], 
            deref(template, solver.env), deref(result, solver.env))), []
          for _ in unify(result, [], solver.env):
            yield cont, True
        return seplist_bultin(item, separator, template, result)
      else: # mode==lazy
        @matcher('seplist')
        def seplist_bultin(solver, cont, item, separator):  
          for _ in unify(result, [], solver.env):
            yield cont, True
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          yield solver.cont(item1, lazy_repeat_result_cont(
            and_p(separator1, item1), cont, 1, [], 
            deref(template, solver.env), deref(result, solver.env))), []
        return seplist_bultin(item, separator, template, result)
  else:
    if result is None:
      if mode==greedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont, item, separator):
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          expectTimes1 = getvalue(expect_times, solver.env)
          if isinstance(expectTimes1, int):
            if expectTimes1<0: raise Error
            elif expectTimes1==0: 
              yield cont, True
              return
          yield solver.cont(item1, greedy_times_cont(and_p(separator1, item1), 
                            expectTimes1, cont, 1, [])), True
        return seplist_bultin(item, separator)
      elif mode==nongreedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont, item, separator):
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          expectTimes1 = getvalue(expect_times, solver.env)
          if isinstance(expectTimes1, int):
            if expectTimes1<0: raise Error
            elif expectTimes1==0: 
              yield cont, True
              return
          yield solver.cont(item1, nongreedy_times_cont(and_p(separator1, item1), 
                            expectTimes1, cont, 1, [])), True
        return seplist_bultin(item, separator)
      else:# mode==lazy:
        @matcher('seplist')
        def seplist_bultin(solver, cont, item, separator): 
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          expectTimes1 = getvalue(expect_times, solver.env)
          if isinstance(expectTimes1, int):
            if expectTimes1<0: raise Error
            elif expectTimes1==0: 
              yield cont, True
              return
          yield solver.cont(item1, lazy_times_cont(and_p(separator1, item1), 
                            expectTimes1, cont, 1, [])), True
        return seplist_bultin(item, separator)
    else:
      if mode==greedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont, item, separator, template, result):  
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          expectTimes1 = getvalue(expect_times, solver.env)
          if isinstance(expectTimes1, int):
            if expectTimes1<0: raise Error
            elif expectTimes1==0: 
              for _ in unify(result, [], solver.env): yield cont, True
              return
          yield solver.cont(item1, greedy_times_result_cont(
            deref(item, solver.env), expectTimes1, cont, 1, [], 
            deref(template, solver.env), deref(result, solver.env))), []
        return seplist_bultin(item, separator, template, result)
      elif mode==nongreedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont, item, separator, template, result): 
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          expectTimes1 = getvalue(expect_times, solver.env)
          if isinstance(expectTimes1, int):
            if expectTimes1<0: raise Error
            elif expectTimes1==0: 
              for _ in unify(result, [], solver.env): yield cont, True
              return
          yield solver.cont(item1, nongreedy_times_result_cont(
            and_p(separator1, item1), expectTimes1, cont, 1, [], 
            deref(template, solver.env), deref(result, solver.env))), []
        return seplist_bultin(item, separator, template, result)
      else: # mode==lazy
        @matcher('seplist')
        def seplist_bultin(solver, cont, item, separator, template, result): 
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          expectTimes1 = getvalue(expect_times, solver.env)
          if isinstance(expectTimes1, int):
            if expectTimes1<0: raise Error
            elif expectTimes1==0: 
              for _ in unify(result, [], solver.env): yield cont, True
              return
          yield solver.cont(item1, lazy_times_result_cont(
            and_p(separator1, item1), expectTimes1, cont, 1, [], 
            deref(template, solver.env), deref(result, solver.env))), []
        return seplist_bultin(item, separator, template, result)
  return seplist_bultin()
    
def seplist_times_more(item, separator, expect_times, template=None, result=None, mode=nongreedy):
  @matcher('seplist_times_more')
  def seplist_bultin(solver, cont, item, separator, template, result):
    expect_times = deref(expect_times, solver.env)
    if not isinstance(expect_times, int): raise ValueError(expect_times)
    result1 = deref(result, solver.env)
    if result1 is not None:
      template1 = deref(template, solver.env)
      temp_result = Var('temp_result')
    else: 
      template1 = None
      temp_result = None
    prefix_list = seplist(item, separator, template1, temp_result1, expect_times, mode)
    for c, v in solver.exp_run_cont(prefix_list, cont):
      if result1 is not None:
        matched_list = getvalue(temp_result1, solver.env)
      else: matched_list = None
      next_cont = make_repeat_cont(solver, cont, and_p(separator1, item1), 
                    0, matched_list, template1, result1, mode)
      yield next_cont, v
  return seplist_bultin(item, separator, template, result)

def seplist_times_less(item, separator, expect_times, template=None, result=None, mode=nongreedy):
  @matcher('seplist_times_more')
  def seplist_bultin(solver, cont, item, separator, template, result):
    expect_times = deref(expect_times, solver.env)
    if not isinstance(expect_times, int): raise ValueError(expect_times)
    if expect_times==0: 
      if result is None: yield cont, True
      else:
        for _ in unify(result, []): 
          yield cont, True
    result1 = deref(result, solver.env)
    if result1 is not None:
      template1 = deref(template, solver.env)
      temp_result = Var('temp_result')
    else: 
      template1 = None
      temp_result = None
    if mode==greedy:
      matched = False
      for c, v in solver.exp_run_cont(item, cont):
        matched = True
        next_cont = make_times_less_cont(solver, cont, item1, separator1, 
                      expect_times1-1, 1, [], template1, result1, mode)
        yield c, v
      if not matched:
        if result1 is None:
          yield cont, True
        else:
          for _ in unify(result1, []): 
            yield cont, True
    elif mode==nongreedy:
      for c, v in solver.exp_run_cont(item, cont):
        next_cont = make_times_less_cont(solver, cont, item1, separator1, 
                      expect_times1-1, 1, [], template1, result1, mode)
        yield c, v
      if result1 is None:
        yield cont, True
      else:
        for _ in unify(result1, []): 
          yield cont, True
    else: #mode==lazy
      if result1 is None:
        yield cont, True
      else:
        for _ in unify(result1, []): 
          yield cont, True
      for c, v in solver.exp_run_cont(item, cont):
        next_cont = make_times_less_cont(solver, cont, item1, separator1, 
                      expect_times1-1, 1, [], template1, result1, mode)
        yield c, v
  return seplist_bultin(item, separator, template, result)

def seplist_times_between(item, separator, min, max, template=None, result=None, mode=nongreedy):
  @matcher('seplist_times_between')
  def seplist_times_between_builtin(solver, cont, item, separator, template, result):
    min1 = deref(min, solver.env)
    max1 = deref(max, solver.env)
    if not isinstance(min1, int): raise ValueError(min1)
    if not isinstance(max1, int): raise ValueError(max1)
    if min1==0:
      yield solver.cont(seplist_time_less(item, separator, max1, template, result, mode), cont), True
    if result is not None:
      temp1 = Var('temp1')
      temp2 = Var('temp2')
    else: 
      temp1 = None
      temp2 = None
    for c, _ in solver.exp_run_cont(seplist_times(item, separator, min, template, temp1, mode), cont):
      for c, _ in solver.exp_run_cont(times_less(item+separator, max-min, template, temp2, mode), cont):
        if result is not None:
          matched_list = getvalue(temp1, solver.env)+getvalue(temp2, solver.env)
          for _ in unify(result, matched_list, solver.env):
            yield cont, True
        else: yield cont, True
  return seplist_times_between_builtin(item, separator, template, result)

@matcher()
def follow(solver, cont, item):
  parse_state = solver.parse_state
  @mycont(cont)
  def follow_cont(value, solver):
    solver.parse_state = parse_state
    yield cont, value
  yield solver.cont(item, follow_cont), True
