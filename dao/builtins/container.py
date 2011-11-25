from dao.term import Var, deref, unify, getvalue
from dao import builtin

# analysing and construction sequences

@builtin.macro()
def contain(solver, container, member): 
  container = getvalue(container, solver.env, {})
  member = getvalue(member, solver.env, {})
  if isinstance(member, Var):
    for x in container:
      for _ in unify(member, x, solver.env):
        yield cont, True
  elif member in container: 
    yield cont, True

@builtin.macro()
def length(solver, sequence, leng):
  sequence = getvalue(sequence, solver.env, {})
  if isinstance(sequence, Var): error.throw_instantiation_error()
  leng = getvalue(leng, solver.env, {})
  for _ in unify(len(sequence), leng, solver.env): 
    yield cont, True


def starstwith(x, y):
  try: x_startswith = x.startswith
  except: return x[:len(y)]== y
  return x_startswith(y)

def endswith(x, y):
  try: x_endswith = x.endswith
  except: return x[len(x)-len(y):]== y
  return x_endswith(y)

@builtin.macro()
def concat(solver, sequence1, sequence2, result):
  sequence1 = getvalue(sequence1, solver.env, {})
  sequence2 = getvalue(sequence2, solver.env, {})
  result = getvalue(result, solver.env, {})
  if isinstance(sequence1, Var):
    index = 0
    if isinstance(sequence2, Var):
      for index in range(1, len(result)):
        for _ in sequence1.unify(result[:index], solver.env):
          for __ in sequence2.unify(result[index:], solver.env):
            yield cont, True
    else:
      if endswith(result, sequence2):
        for _ in sequence1.unify(result[:len(sequence2)], solver.env): yield cont, True
  else:
    if isinstance(sequence2, Var):
      if startswith(result, sequence1):
        for _ in sequence2.unify(result[len(sequence1):], solver.env): yield cont, True
    else:
      for _ in unify(result, sequence1+sequence2, solver.env): yield cont, True

class SubSequenceUEntitySubContinuation:
  def __init__(self, sequence, before, length, after, sub, solver):
    SubSequenceContinuation.__init__(self, sequence, before, length, after, sub, solver)
    self.s1 = helper.unwrap_sequence(sub)
    if len(self.s1)>=self.stoplength or len(self.s1)<self.startlength:
      raise error.UnifyFail()
    self.start = self.startbefore
  def activate(self, solver):
    start = self.start
    assert start >= 0
    end = self.stopbefore + len(self.s1)
    assert end>=0
    b = self.sequence.find(self.s1, start, end) # XXX -1?
    if b<0: raise error.UnifyFail()
    self.prepare_more_solutions(solver)
    self.start = b + 1
    try:
      self.before.unify(Integer(b), solver.env)
      self.after.unify(Integer(len(self.sequence) - len(self.s1) - b), solver.env)
      self.length.unify(Integer(len(self.s1)), solver.env)
    except error.UnifyFail: pass
    return solver.set(self.cont)

  def __repr__(self): return "<SubSequenceUEntitySubContinuation(%r)>" % self.__dict__

class SubSequenceVarAfterContinuation:#(SubSequenceContinuation):
  def __init__(self, sequence, before, length, after, sub, solver):
    SubSequenceContinuation.__init__(self, sequence, before, length, after, sub, solver)
    self.b = self.startbefore
    self.l = self.startlength
  def activate(self, solver):
    if self.b < self.stopbefore:
      if self.l < self.stoplength:
        if self.l + self.b > len(self.sequence):
          self.b += 1
          self.l = self.startlength
          return self.activate(fcont, trail)
        self.prepare_more_solutions(solver)

        self.before.unify(Integer(self.b), solver.env)
        self.after.unify(Integer(
          len(self.sequence) - self.l - self.b), solver.env)
        self.length.unify(Integer(self.l), solver.env)
        b = self.b
        l = self.l
        assert b >= 0
        assert l >= 0
        self.sub.unify(sequence(self.sequence[b:b + l]), trail)
        self.l += 1
        return solver.set(self.cont)
      else:
        self.b += 1
        self.l = self.startlength
        return self.activate(solver)
    raise error.UnifyFail()

class SubSequenceElseContinuation:#(SubSequenceContinuation):
  def __init__(self, sequence, before, length, after, sub, solver):
    SubSequenceContinuation.__init__(solver, sequence, before, length, after, sub)
    self.a = helper.unwrap_int(after)
    self.l = self.startlength
  def activate(self, solver):
    if self.l < self.stoplength:
      b = len(self.sequence) - self.l - self.a
      assert b >= 0
      if self.l + b > len(self.sequence):
        self.l += 1
        return self.activate(solver)
      self.prepare_more_solutions(solver)
      self.before.unify(Integer(b), solver.env)
      self.after.unify(Integer(self.a), solver.env)
      self.length.unify(Integer(self.l), solver.env)
      l = self.l
      assert l >= 0
      self.sub.unify(sequence(self.sequence[b:b + l], cache=False), solver.env)
      self.l += 1
      return solver.set(self.nextcont)
    raise error.UnifyFail()

@builtin.macro()
def subsequence(solver, sequence, before, length, after, sub):
  sequence = deref(sequence, solver.env)
  before = deref(before, solver.env)
  length = deref(length, solver.env)
  after = deref(after, solver.env)
  sub = deref(sub, solver.env)
  if not isinstance(before, Var):
    if before<0 or before>=len(sequence): return
  if not isinstance(length, Var):  
    if length<=0 or length>len(sequence): return
  if not isinstance(after, Var):
    if after<0 or after>len(sequence): return
  cont = solver.scont
  @mycont(cont)
  def sub_sequence_cont(value, solver):
    if not isinstance(sub, Var):
      if sub=='': return
      if isinstance(before, Var): startbefore, stopbefore = 0, len(sequence)+1
      else: startbefore, stopbefore = before, before+1
      for _ in unify(length, len(sub), solver.env):
        start  = startbefore
        while start<stopbefore:
          start = sequence.find(sub, start)
          if start<0: return
          for _ in unify(before, start, solver.env):
            for _ in unify(after, start+len(sub), solver.env):
              yield cont, True
          start += 1
    else:
      if not isinstance(before, Var) \
         and not isinstance(length, Var)\
         and not isinstance(after, Var):
        if start+length!=after: return
        for _ in sub.unify(sequence[before:after], solver.env):
          yield cont, sequence[before:after]
      elif not isinstance(before, Var) and  not isinstance(length, Var):
        if before+length>len(sequence): return
        for _ in sub.unify(sequence[before:after], solver.env):
          for _ in after.unify(before+length, solver.env):
            yield cont, sequence[before:before+length]
      elif not isinstance(length, Var) and  not isinstance(after, Var):
        if after-length<0: return
        for _ in sub.unify(sequence[after-length:after], solver.env):
          for _ in length.unify(length, solver.env):
            yield cont, sequence[after-length:after:after]
      elif not isinstance(before, Var):
        for leng in range(1, len(sequence)-before+1):
          for _ in sub.unify(sequence[before:before+leng], solver.env):
            for _ in length.unify(leng, solver.env):
              for _ in after.unify(before+leng, solver.env):
                yield cont, sequence[before:before+leng]
      elif not isinstance(after, Var):
        for leng in range(1, after):
          for _ in sub.unify(sequence[after-leng+1:after], solver.env):
            for _ in length.unify(leng, solver.env):
              for _ in before.unify(after-leng+1, solver.env):
                yield cont, sequence[before:after]
      elif not isinstance(length, Var):
        for start in range(len(sequence)-length):
          for _ in sub.unify(sequence[start:start+length], solver.env):
            for _ in before.unify(start, solver.env):
              for _ in after.unify(start+length, solver.env):
                yield cont, sequence[start:start+length]
      else:
        for start in range(len(sequence)):
          for leng in range(1, len(sequence)-start+1):
            for _ in sub.unify(sequence[start:start+leng], solver.env):
              for _ in before.unify(start, solver.env):
                for _ in length.unify(leng, solver.env):
                  for _ in after.unify(start+leng, solver.env):
                    yield cont, sequence[start:start+leng]
  yield sub_sequence_cont, True

@builtin.function('conslist')
def conslist(*arguments): return conslist(arguments)

@builtin.function('pylist')
def pylist(*arguments): return list(arguments)

@builtin.function('pytuple')
def pytuple(*arguments): 
  return tuple(arguments)

@builtin.function('head_list')
def head_list(head, tail): 
  if isinstance(tail, list): return [head]+tail
  else: return (head,)+tuple(tail)

@builtin.function('list_tail')
def list_tail(head, tail):
  if isinstance(head, list): return head+[tail]
  else: return head+(tail,)

@builtin.function('index')
def index(sequence, index): 
  return sequence[index]

@builtin.function('first')
def first(sequence): 
  return sequence[0]

@builtin.function('left')
def left(sequence): 
  return sequence[1:]

@builtin.function('second')
def second(sequence): 
  return sequence[1]

from dao.solve import DaoStopIteration

@builtin.function('iter_next')
def iter_next(iterator): 
  try: return iterator.next()
  except StopIteration:
##    iterator.close()
    raise DaoStopIteration

@builtin.function('make_iter')
def make_iter(iterator): 
  try: 
    iterator.next
    return iterator
  except: return iter(iterator)
  
@builtin.function('to_list')
def to_list(item): 
  if isinstance(item, list) or isinstance(item, tuple): 
    return item
  return [item]

@builtin.function('items')
def items(dict):
  return dict.items()
