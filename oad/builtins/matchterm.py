# -*- coding: utf-8 -*-

from oad.term import deref, getvalue, unify, Apply
from oad.error import throw_type_error
from oad.builtin import builtin, BuiltinMacro
from oad.solve import mycont

# parser predicate
# optional, any, some, times, seplist, ...

class BuiltinMatcher(BuiltinMacro):
  def __call__(self, *exps):
    return MatcherApply(self, *exps)

class MatcherApply(Apply):
  def __getitem__(self, index):
    if not isinstance(index, slice):
      if index==0: raise ValueError(index)
      if index==1: return self
      return times(self, index)
    elif isinstance(index, slice): 
      if index.step is not None: raise ValueError(index)
      if index.start>=index.stop: raise ValueError(index)
      if index.start is None and index.stop is None:
        return any(self)
      if index.start==1 and index.stop is None:
        return some(self)
      if index.start==index.stop-1:
        return times(self, index.start)
      if index.start==None and index.stop==1:
        return optional(self)
      if index.start is not None and index.stop is None:
        return times(self, index.start)+any(self)
      raise ValueError(index)
  def __neg__(self):
    return optional(self)
  def __pos__(self):
    return some(self)
  def __div__(self, other):
    if self.operator not in [some, any, times, seplist]:
      raise ValueError(self)
    if self.result is not None:
      raise ValueError(self)
    self.template = other
    return self
  def __rshift__(self, other):
    if self.operator in [some, any, times, seplist]:
      if self.result is not None:
        raise ValueError(self)
      self.result = other
      return self
    raise ValueError(self)
  
matcher = builtin(BuiltinMatcher)

@matcher()
def nullword(solver, cont): 
  yield cont,  True
null = nullword = nullword()

# lazy: match at least, except matchers followed fail. 尽量少吃进，除非别人逼我多吃一点
# nongreedy, match at most, throw out if matchers followed fail.先尽量多吃，如果别人要再吐出来
# greedy, match at most, don,t throw out even matchers followed fail. 吃进去了就不会吐出来。
lazy, nongreedy, greedy = range(3)

@matcher()
def optional(solver, cont, item, mode=nongreedy):
  item = deref(item, solver.env)
  stream = solver.stream
  for x in solver.solve(item, cont):
    yield cont, x
    if greedy: return
  solver.stream = stream
  yield cont, True

@matcher()
def parallel(solver, cont, *calls):
  call0 = deref(calls[0], solver.env)
  if len(calls)==1: 
    yield solver.cont(call0, cont), True
    return
  stream = solver.stream
  @mycont(cont)
  def pallel_cont(value, solver):
    if pallel_cont.right==-1: pallel_cont.right = solver.stream[1]
    elif pallel_cont.right!=solver.stream[1]: return
    if len(pallel_cont.calls)==0: 
      yield cont, True
      return
    solver.stream = stream
    call0 = pallel_cont.calls[0]
    pallel_cont.calls =pallel_cont.calls[1:]
    yield solver.cont(call0, pallel_cont), True
  pallel_cont.right = -1
  pallel_cont.calls = calls[1:]
  yield solver.cont(call0, pallel_cont), True

def greedy_repeat_cont(item, cont):
  @mycont(cont)
  def repeat_cont(value, solver):
    matched = False
    stream = solver.stream
    for x in solver.solve(item, repeat_cont):
      matched = True
      yield repeat_cont, x 
    if matched: return
    solver.stream = stream
    yield cont, value
  return repeat_cont

def nongreedy_repeat_cont(item, cont):
  @mycont(cont)
  def repeat_cont(value, solver):
    stream = solver.stream
    yield solver.cont(item, repeat_cont), True 
    solver.stream = stream
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
    matched = False
    stream = solver.stream
    if matched_times>0: matched_list.append(getvalue(template, solver.env))
    next_cont = greedy_repeat_result_cont(item, cont, matched_times+1, matched_list, template, result)
    for x in solver.solve(item, next_cont):
      matched = True
      yield next_cont, True
    if matched: return
    for _ in unify(result, matched_list, solver.env): 
      solver.stream = stream
      yield cont, True
  return repeat_cont

def nongreedy_repeat_result_cont(item, cont, matched_times, matched_list, template, result):
  @mycont(cont)
  def repeat_cont(value, solver):
    stream = solver.stream
    if matched_times>0: matched_list.append(getvalue(template, solver.env))
    next_cont = nongreedy_repeat_result_cont(item, cont, matched_times+1, matched_list, template, result)
    yield solver.cont(item, next_cont), True
    for _ in unify(result, matched_list, solver.env): 
      solver.stream = stream
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
      @matcher('any')
      def any_bultin(solver, cont):  
        yield greedy_repeat_cont(deref(item, solver.env), cont), []
    elif mode==nongreedy:
      @matcher('any')
      def any_bultin(solver, cont):  
        yield nongreedy_repeat_cont(deref(item, solver.env), cont), []
    else:# mode==lazy:
      @matcher('any')
      def any_bultin(solver, cont):  
        yield lazy_repeat_cont(deref(item, solver.env), cont), []
  else:
    if mode==greedy:
      @matcher('any')
      def any_bultin(solver, cont):  
        yield greedy_repeat_result_cont(
          deref(item, solver.env), cont, 0, [], 
          deref(template, solver.env), deref(result, solver.env)), []
    elif mode==nongreedy:
      @matcher('any')
      def any_bultin(solver, cont):  
        yield nongreedy_repeat_result_cont(
          deref(item, solver.env), cont, 0, [], 
          deref(template, solver.env), deref(result, solver.env)), []
    else: # mode==lazy
      @matcher('any')
      def any_bultin(solver, cont):  
        yield lazy_repeat_result_cont(
          deref(item, solver.env), cont, 0, [], 
          deref(template, solver.env), deref(result, solver.env)), []
  return any_bultin()

def some(item, template=None, result=None, mode=nongreedy):   
  if result is None:
    if mode==greedy:
      @matcher('some')
      def some_bultin(solver, cont):  
        yield solver.cont(item, greedy_repeat_cont(
          deref(item, solver.env), cont)), []
    elif mode==nongreedy:
      @matcher('some')
      def some_bultin(solver, cont):  
        yield solver.cont(item, nongreedy_repeat_cont(
          deref(item, solver.env), cont)), []
    else:# mode==lazy:
      @matcher('some')
      def some_bultin(solver, cont):  
        yield solver.cont(item, lazy_repeat_cont(
          deref(item, solver.env), cont)), []
  else:
    if mode==greedy:
      @matcher('some')
      def some_bultin(solver, cont):  
        yield solver.cont(item, greedy_repeat_result_cont(
          deref(item, solver.env), cont, 1, [], 
          deref(template, solver.env), deref(result, solver.env))), []
    elif mode==nongreedy:
      @matcher('some')
      def some_bultin(solver, cont):  
        yield solver.cont(item, nongreedy_repeat_result_cont(
          deref(item, solver.env), cont, 1, [], 
          deref(template, solver.env), deref(result, solver.env))), []
    else: # mode==lazy
      @matcher('some')
      def some_bultin(solver, cont):  
        yield solver.cont(item, lazy_repeat_result_cont(
          deref(item, solver.env), cont, 1, [], 
          deref(template, solver.env), deref(result, solver.env))), []
  return some_bultin()

def greedy_times_cont(item, cont):
  if isinstance(expectTimes, int):
    @mycont(cont)
    def times_cont(value, solver):
      if expectTimes==matched_times: yield cont, True
      else: yield solver.cont(item, 
        greedy_times_cont(item, expectTimes, cont, matched_times+1)), True
  else:
    @mycont(cont)
    def times_cont(value, solver):
      matched = False
      stream = solver.stream
      next_cont = nongreedy_times_cont(item, expectTimes, cont, matched_times+1)
      for x in solver.solve(item, next_cont):
        matched = True
        yield next_cont, x 
      if matched: return
      for _ in unify(expectTimes, matched_times, solver.env): 
        solver.stream = stream
        yield cont, True
  return times_cont

def nongreedy_times_cont(item, expectTimes, cont, matched_times):
  if isinstance(expectTimes, int):
    @mycont(cont)
    def times_cont(value, solver):
      if expectTimes==matched_times: yield cont, True
      else: yield solver.cont(item, 
        nongreedy_times_cont(item, expectTimes, cont, matched_times+1)), True
  else:
    @mycont(cont)
    def times_cont(value, solver):
      stream = solver.stream
      yield solver.cont(item, nongreedy_times_cont(item, expectTimes, cont, matched_times+1)), True
      for _ in unify(expectTimes, matched_times, solver.env): 
        solver.stream = stream
        yield cont, True
  return times_cont

def lazy_times_cont(item, expectTimes, cont, matched_times):
  if isinstance(expectTimes, int):
    @mycont(cont)
    def times_cont(value, solver):
      if expectTimes==matched_times: yield cont, True
      else: yield solver.cont(item, 
        lazy_times_cont(item, expectTimes, cont, matched_times+1)), True
  else:
    @mycont(cont)
    def times_cont(value, solver):
      for _ in unify(expectTimes, matched_times, solver.env): 
        solver.stream = stream
        yield cont, True
      next_cont = lazy_times_cont(item, expectTimes, cont, matched_times+1)
      for x in solver.solve(item, next_cont):
        yield next_cont, x 
  return times_cont

def greedy_times_result_cont(item, expectTimes, cont, matched_times, 
      matched_list, template, result):
  if isinstance(expectTimes, int):
    @mycont(cont)
    def times_cont(value, solver):
      if matched_times>0: matched_list.append(getvalue(template, solver.env))
      if expectTimes==matched_times:
        for _ in unify(result, matched_list, solver.env):
          yield cont, True
          return
      else: yield solver.cont(item, greedy_times_result_cont(item, expectTimes, cont, 
              matched_times+1, matched_list, template, result)), True
  else:
    @mycont(cont)
    def times_cont(value, solver):
      matched = False
      stream = solver.stream
      if matched_times>0: matched_list.append(getvalue(template, solver.env))
      next_cont = greedy_times_result_cont(item, expectTimes, cont, 
                    matched_times+1, matched_list, template, result)
      for x in solver.solve(item, next_cont):
        matched = True
        yield next_cont, True
      if matched: return
      for _ in unify(expectTimes, matched_times, solver.env):
        for _ in unify(result, matched_list, solver.env): 
          solver.stream = stream
          yield cont, True
  return times_cont

def nongreedy_times_result_cont(item, expectTimes, cont, 
      matched_times, matched_list, template, result):
  if isinstance(expectTimes, int):
    @mycont(cont)
    def times_cont(value, solver):
      if matched_times>0: matched_list.append(getvalue(template, solver.env))
      if expectTimes==matched_times:
        for _ in unify(result, matched_list, solver.env):
          yield cont, True
          return
      else: yield solver.cont(item, 
              greedy_times_result_cont(item, expectTimes, cont, 
              matched_times+1, matched_list, template, result)), True
  else:
    @mycont(cont)
    def times_cont(value, solver):
      stream = solver.stream
      if matched_times>0: matched_list.append(getvalue(template, solver.env))
      next_cont = nongreedy_times_result_cont(item, expectTimes, cont, 
                    matched_times+1, matched_list, template, result)
      yield solver.cont(item, next_cont), True
      for _ in unify(expectTimes, matched_times, solver.env):
        for _ in unify(result, matched_list, solver.env): 
          solver.stream = stream
          yield cont, True
  return times_cont

def lazy_times_result_cont(item, expectTimes, cont, 
      matched_times, matched_list, template, result):
  @mycont(cont)
  def times_cont(value, solver):
    if matched_times>0: matched_list.append(getvalue(template, solver.env))
    for _ in unify(result, matched_list, solver.env): 
      yield cont, True
    next_cont = lazy_times_result_cont(item, cont, matched_times+1, matched_list, template, result)
    yield solver.cont(item, next_cont), True
  return times_cont
  if isinstance(expectTimes, int):
    @mycont(cont)
    def times_cont(value, solver):
      if matched_times>0: matched_list.append(getvalue(template, solver.env))
      if expectTimes==matched_times:
        for _ in unify(result, matched_list, solver.env):
          yield cont, True
          return
      else: yield solver.cont(item, greedy_times_result_cont(item, expectTimes, cont, 
              matched_times+1, matched_list, template, result)), True
  else:
    @mycont(cont)
    def times_cont(value, solver):
      if matched_times>0: matched_list.append(getvalue(template, solver.env))
      for _ in unify(expectTimes, matched_times, solver.env):
        for _ in unify(result, matched_list, solver.env): 
          yield cont, True
      next_cont = lazy_times_result_cont(item, cont, matched_times+1, matched_list, template, result)
      yield solver.cont(item, next_cont), True
  return times_cont
  
def times(item, expectTimes, template=None, result=None, mode=nongreedy):   
  if result is None:
    if mode==greedy:
      @matcher('times')
      def times_bultin(solver, cont):
        expectTimes1 = getvalue(expectTimes, solver.env)
        if isinstance(expectTimes1, int):
          if expectTimes1<0: raise Error
          elif expectTimes1==0: 
            yield cont, True
            return
        yield greedy_times_cont(item, expectTimes1, cont, 0), True
    elif mode==nongreedy:
      @matcher('times')
      def times_bultin(solver, cont):
        expectTimes1 = getvalue(expectTimes, solver.env)
        if isinstance(expectTimes1, int):
          if expectTimes1<0: raise Error
          elif expectTimes1==0: 
            yield cont, True
            return
        yield nongreedy_times_cont(item, expectTimes1, cont, 0), True
    else:# mode==lazy:
      @matcher('times')
      def times_bultin(solver, cont): 
        expectTimes1 = getvalue(expectTimes, solver.env)
        if isinstance(expectTimes1, int):
          if expectTimes1<0: raise Error
          elif expectTimes1==0: 
            yield cont, True
            return
        yield lazy_times_cont(item, expectTimes1, cont, 0), True
  else:
    if mode==greedy:
      @matcher('times')
      def times_bultin(solver, cont):  
        expectTimes1 = getvalue(expectTimes, solver.env)
        if isinstance(expectTimes1, int):
          if expectTimes1<0: raise Error
          elif expectTimes1==0: 
            for _ in unify(result, [], solver.env): yield cont, True
            return
        yield greedy_times_result_cont(
          deref(item, solver.env), expectTimes1, cont, 0, [], 
          deref(template, solver.env), deref(result, solver.env)), []
    elif mode==nongreedy:
      @matcher('times')
      def times_bultin(solver, cont): 
        expectTimes1 = getvalue(expectTimes, solver.env)
        if isinstance(expectTimes1, int):
          if expectTimes1<0: raise Error
          elif expectTimes1==0: 
            for _ in unify(result, [], solver.env): yield cont, True
            return
        yield nongreedy_times_result_cont(
          deref(item, solver.env), expectTimes1, cont, 0, [], 
          deref(template, solver.env), deref(result, solver.env)), []
    else: # mode==lazy
      @matcher('times')
      def times_bultin(solver, cont): 
        expectTimes1 = getvalue(expectTimes, solver.env)
        if isinstance(expectTimes1, int):
          if expectTimes1<0: raise Error
          elif expectTimes1==0: 
            for _ in unify(result, [], solver.env): yield cont, True
            return
        yield lazy_times_result_cont(
          deref(item, solver.env), expectTimes1, cont, 0, [], 
          deref(template, solver.env), deref(result, solver.env)), []
  return times_bultin()

@matcher()
def times_more(solver, cont, item, expectTimes, template=None, result=None):   
  item = deref(item, solver.env)
  expectTimes = getvalue(expectTimes, solver.env)
  template = deref(template, solver.env)
  result = deref(result, solver.env)
  if isinstance(expectTimes, int):
    if expectTimes<0: raise ValueError(self)
    elif expectTimes==0: 
      yield solver.cont(any(item, template, result), cont), True
      return
    elif  expectTimes==1: 
      yield solver.cont(some(item, template, result)), True
      return
  item = times(item, expectTimes, template, result)+any(item, template, result)
  yield solver.cont(item, cont), True

def seplist(item, separator, template=None, result=None, 
            expect_times=None, mode=nongreedy):
  if expect_times==0: return nullword
  elif expect_times==1: return item
  if expect_times is None:
    if result is None:
      if mode==greedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont):  
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          yield solver.cont(item1, greedy_repeat_cont(
            separator1+item1, cont)), []
      elif mode==nongreedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont):  
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          yield solver.cont(item1, nongreedy_repeat_cont(
            separator1+item1, cont)), []
      else:# mode==lazy:
        @matcher('some')
        def seplist_bultin(solver, cont):  
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          yield solver.cont(item1, lazy_repeat_cont(
            separator1+item1, cont)), []
    else:
      if mode==greedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont):  
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          yield solver.cont(item1, greedy_repeat_result_cont(
            separator1+item1, cont, 1, [], 
            deref(template, solver.env), deref(result, solver.env))), []
      elif mode==nongreedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont):  
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          yield solver.cont(item1, nongreedy_repeat_result_cont(
            separator1+item1, cont, 1, [], 
            deref(template, solver.env), deref(result, solver.env))), []
      else: # mode==lazy
        @matcher('seplist')
        def seplist_bultin(solver, cont):  
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          yield solver.cont(item1, lazy_repeat_result_cont(
            separator1+item1, cont, 1, [], 
            deref(template, solver.env), deref(result, solver.env))), []
  else:
    if result is None:
      if mode==greedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont):
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          expectTimes1 = getvalue(expectTimes, solver.env)
          if isinstance(expectTimes1, int):
            if expectTimes1<0: raise Error
            elif expectTimes1==0: 
              yield cont, True
              return
          yield solver.cont(item1, greedy_times_cont(separator1+item1, 
                            expectTimes1, cont, 1, [])), True
      elif mode==nongreedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont):
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          expectTimes1 = getvalue(expectTimes, solver.env)
          if isinstance(expectTimes1, int):
            if expectTimes1<0: raise Error
            elif expectTimes1==0: 
              yield cont, True
              return
          yield solver.cont(item1, nongreedy_times_cont(separator1+item1, 
                            expectTimes1, cont, 1, [])), True
      else:# mode==lazy:
        @matcher('seplist')
        def seplist_bultin(solver, cont): 
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          expectTimes1 = getvalue(expectTimes, solver.env)
          if isinstance(expectTimes1, int):
            if expectTimes1<0: raise Error
            elif expectTimes1==0: 
              yield cont, True
              return
          yield solver.cont(item1, lazy_times_cont(separator1+item1, 
                            expectTimes1, cont, 1, [])), True
    else:
      if mode==greedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont):  
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          expectTimes1 = getvalue(expectTimes, solver.env)
          if isinstance(expectTimes1, int):
            if expectTimes1<0: raise Error
            elif expectTimes1==0: 
              for _ in unify(result, [], solver.env): yield cont, True
              return
          yield solver.cont(item1, greedy_times_result_cont(
            deref(item, solver.env), expectTimes1, cont, 1, [], 
            deref(template, solver.env), deref(result, solver.env))), []
      elif mode==nongreedy:
        @matcher('seplist')
        def seplist_bultin(solver, cont): 
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          expectTimes1 = getvalue(expectTimes, solver.env)
          if isinstance(expectTimes1, int):
            if expectTimes1<0: raise Error
            elif expectTimes1==0: 
              for _ in unify(result, [], solver.env): yield cont, True
              return
          yield solver.cont(item1, nongreedy_times_result_cont(
            deref(item, solver.env), expectTimes1, cont, 1, [], 
            deref(template, solver.env), deref(result, solver.env))), []
      else: # mode==lazy
        @matcher('seplist')
        def seplist_bultin(solver, cont): 
          separator1 = deref(separator, solver.env) 
          item1 = deref(item, solver.env) 
          expectTimes1 = getvalue(expectTimes, solver.env)
          if isinstance(expectTimes1, int):
            if expectTimes1<0: raise Error
            elif expectTimes1==0: 
              for _ in unify(result, [], solver.env): yield cont, True
              return
          yield solver.cont(item1, lazy_times_result_cont(
            deref(item, solver.env), expectTimes1, cont, 1, [], 
            deref(template, solver.env), deref(result, solver.env))), []
  return seplist_bultin()
    
@matcher()
def follow(solver, cont, item):
  arg0 = self.elements[0].deref(solver.env)
  oldStreamPosition = solver.streamer.position
  solver.streamer.position = oldStreamPosition
  return scont, fcont, solver.env

@matcher()
def followby(solver):
  arg0 = self.elements[0].deref(solver.env)
  call2 = self.elements[1].deref(solver.env)
  oldStreamPosition = solver.streamer.position
  solver.call(arg0, DoneContinuation(solver), fcont, solver.env)
  solver.streamer.position = oldStreamPosition
  return scont, fcont, solver.env

@matcher()
def notfollowby(solver):
  arg0 = self.elements[0].deref(solver.env)
  call2 = self.elements[1].deref(solver.env)
  oldStreamPosition = solver.streamer.position
  try: 
    solver.call(arg0, DoneContinuation(solver), fcont, solver.env)
    raise UnifyFail()
  except: solver.streamer.position = oldStreamPosition
  return scont, fcont, solver.env
