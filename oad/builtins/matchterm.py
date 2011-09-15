from oad.term import deref, getvalue, unify
from oad.error import throw_type_error
from oad import builtin
from oad.solve import mycont

# parser predicate
# optional, any, some, times, seplist, ...

@builtin.macro()
def optional(solver, cont, item):
  item = deref(item, solver.env)
  stream = solver.stream
  yield solver.cont(item, cont), True
  solver.stream = stream
  yield cont, True

@builtin.macro()
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

def make_repeat_cont(item, cont, matched_times=0, matched_list=None, template=None, result=None):
  if template is None:
    @mycont(cont)
    def repeat_cont(value, solver):
      stream = solver.stream
      yield solver.cont(item, make_repeat_cont(item, cont)), True
      solver.stream = stream
      yield cont, True
    return repeat_cont
  else: 
    @mycont(cont)
    def repeat_cont(value, solver):
      stream = solver.stream
      if matched_times>0: matched_list.append(getvalue(template, solver.env))
      yield solver.cont(item, make_repeat_cont(item, cont, 
                matched_times+1, matched_list, template, result)), True
      for _ in unify(result, matched_list, solver.env): 
        solver.stream = stream
        yield cont, True
    return repeat_cont
  
@builtin.macro()
def any(solver, cont, item, template=None, result=None):   
  item = deref(item, solver.env)
  stream = solver.stream
  template = deref(template, solver.env)
  result = deref(result, solver.env)
  yield make_repeat_cont(item, cont, 0, [], template, result), []

@builtin.macro()
def some(solver, cont, item, template=None, result=None):   
  item = deref(item, solver.env)
  stream = solver.stream
  if template is not None: 
    template = deref(template, solver.env)
    result = deref(result, solver.env)
    yield solver.cont(item, make_repeat_cont(item, cont, 1, [], template, result)), []
  else: yield solver.cont(item, make_repeat_cont(item, cont)), True

def make_times_cont(item, expectTimes, cont, matched_times, matched_list=None, template=None, result=None):
  if isinstance(expectTimes, int):
    if result is None:
      @mycont(cont)
      def times_cont(value, solver):
        for _ in unify(expectTimes, matched_times+1, solver.env): # matched: matched times
          yield cont, True
          return
        yield solver.cont(item, make_times_cont(item, expectTimes, cont, matched_times+1)), True
      return times_cont
    else: 
      @mycont(cont)
      def times_cont(value, solver):
        if matched_times>0:
          matched_list.append(getvalue(template, solver.env))
        for _ in unify(expectTimes, matched_times, solver.env):
          for _ in unify(result, matched_list, solver.env):
            yield cont, True
            return
        yield solver.cont(item, make_times_cont(item, expectTimes, cont, 
                matched_times+1, matched_list, template, result)), True
  else:
    if result is None:
      @mycont(cont)
      def times_cont(value, solver):
        stream = solver.stream
        yield solver.cont(item, make_times_cont(item, expectTimes, cont, matched_times+1)), True
        for _ in unify(expectTimes, matched_times+1, solver.env): # matched: matched times
          solver.stream = stream
          yield cont, True      
    else: 
      @mycont(cont)
      def times_cont(value, solver):
        stream = solver.stream
        if matched_times>0:
          matched_list.append(getvalue(template, solver.env))
        yield solver.cont(item, make_times_cont(item, expectTimes, cont, 
              matched_times+1, matched_list, template, result)), True
        for _ in unify(expectTimes, matched_times+1, solver.env): # matched: matched times
          for _ in unify(result, matched_list, solver.env): 
            solver.stream = stream
            yield cont, True
  return times_cont
    
@builtin.macro()
def times(solver, cont, item, expectTimes, template=None, result=None):   
  item = deref(item, solver.env)
  expectTimes = getvalue(expectTimes, solver.env)
  template = deref(template, solver.env)
  result = deref(result, solver.env)
  if isinstance(expectTimes, int) and expectTimes<0: raise Error
  yield make_times_cont(item, expectTimes, cont, 0, [], template, result), True

from oad.builtins.control import and_
@builtin.macro()
def seplist(solver, cont, item, separator=' ', template=None, result=None, expect_times=None):
  result = deref(result, solver.env)
  if expect_times==0: 
    if result is not None:
      for _ in unify(result, [], solver.env): yield cont, True
    else: yield cont, True
  item = deref(item, solver.env)
  if separator==' ': separator = (any, (char, ' '))
  else: separator = deref(separator, solver.env)
  template = deref(template, solver.env)
  repeat_item = (and_, separator, item)
  if expect_times is None:
    repeat_cont = make_repeat_cont(repeat_item, cont, 1, [], template, result)
    yield solver.cont(item, repeat_cont), True
  else:  
    times_cont = make_times_cont(repeat_item, expect_times, cont, 1, [], template, result)   
    yield solver.cont(item, times_cont), True
    
@builtin.macro()
def follow(solver, cont, item):
  arg0 = self.elements[0].deref(solver.env)
  oldStreamPosition = solver.streamer.position
  solver.streamer.position = oldStreamPosition
  return scont, fcont, solver.env

@builtin.macro()
def followby(solver):
  arg0 = self.elements[0].deref(solver.env)
  call2 = self.elements[1].deref(solver.env)
  oldStreamPosition = solver.streamer.position
  solver.call(arg0, DoneContinuation(solver), fcont, solver.env)
  solver.streamer.position = oldStreamPosition
  return scont, fcont, solver.env

@builtin.macro()
def notfollowby(solver):
  arg0 = self.elements[0].deref(solver.env)
  call2 = self.elements[1].deref(solver.env)
  oldStreamPosition = solver.streamer.position
  try: 
    solver.call(arg0, DoneContinuation(solver), fcont, solver.env)
    raise UnifyFail()
  except: solver.streamer.position = oldStreamPosition
  return scont, fcont, solver.env
