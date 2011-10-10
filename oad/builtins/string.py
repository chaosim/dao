from oad.term import Var, deref, unify, getvalue
from oad import builtin

# analysing and construction strings

@builtin.macro()
def char_in(solver, cont, in_, whole): 
  in_ = getvalue(in_, solver.env)
  whole = getvalue(whole, solver.env)
  if isinstance(in_, Var):
    for x in whole:
      for _ in unify(in_, x, solver.env):
        yield cont, True
  elif in_ in whole: 
    yield cont, True

@builtin.macro()
def length(solver, cont, string, leng):
  string = deref(string, solver.env)
  if isinstance(string, Var): error.throw_instantiation_error()
  leng = deref(leng, solver.env)
  if not (isinstance(leng, Var) or isinstance(leng, int)):
    error.throw_type_error("integer", leng)
  for _ in unify(len(string), leng, solver.env): 
    yield cont, True

@builtin.macro()
def concat(solver, cont, string1, string2, result):
  string1 = deref(string1, solver.env)
  string2 = deref(string2, solver.env)
  result = deref(result, solver.env)
  if isinstance(string1, Var):
    index = 0
    if isinstance(string2, Var):
      for index in range(1, len(result)):
        for _ in string1.unify(result[:index], solver.env):
          for __ in string2.unify(result[index:], solver.env):
            yield cont, True
    else:
      if result.endswith(string2):
        for _ in string1.unify(result[:len(string2)], solver.env): yield cont, True
  else:
    if isinstance(string2, Var):
      if result.startswith(string1):
        for _ in string2.unify(result[len(string1):], solver.env): yield cont, True
    else:
      for _ in unify(result, string1+string2, solver.env): yield cont, True

class SubStringUEntitySubContinuation:
  def __init__(self, string, before, length, after, sub, solver):
    SubStringContinuation.__init__(self, string, before, length, after, sub, solver)
    self.s1 = helper.unwrap_string(sub)
    if len(self.s1)>=self.stoplength or len(self.s1)<self.startlength:
      raise error.UnifyFail()
    self.start = self.startbefore
  def activate(self, solver):
    start = self.start
    assert start >= 0
    end = self.stopbefore + len(self.s1)
    assert end>=0
    b = self.string.find(self.s1, start, end) # XXX -1?
    if b<0: raise error.UnifyFail()
    self.prepare_more_solutions(solver)
    self.start = b + 1
    try:
      self.before.unify(Integer(b), solver.env)
      self.after.unify(Integer(len(self.string) - len(self.s1) - b), solver.env)
      self.length.unify(Integer(len(self.s1)), solver.env)
    except error.UnifyFail: pass
    return solver.set(self.cont)

  def __repr__(self): return "<SubStringUEntitySubContinuation(%r)>" % self.__dict__

class SubStringVarAfterContinuation:#(SubStringContinuation):
  def __init__(self, string, before, length, after, sub, solver):
    SubStringContinuation.__init__(self, string, before, length, after, sub, solver)
    self.b = self.startbefore
    self.l = self.startlength
  def activate(self, solver):
    if self.b < self.stopbefore:
      if self.l < self.stoplength:
        if self.l + self.b > len(self.string):
          self.b += 1
          self.l = self.startlength
          return self.activate(fcont, trail)
        self.prepare_more_solutions(solver)

        self.before.unify(Integer(self.b), solver.env)
        self.after.unify(Integer(
          len(self.string) - self.l - self.b), solver.env)
        self.length.unify(Integer(self.l), solver.env)
        b = self.b
        l = self.l
        assert b >= 0
        assert l >= 0
        self.sub.unify(string(self.string[b:b + l]), trail)
        self.l += 1
        return solver.set(self.cont)
      else:
        self.b += 1
        self.l = self.startlength
        return self.activate(solver)
    raise error.UnifyFail()

class SubStringElseContinuation:#(SubStringContinuation):
  def __init__(self, string, before, length, after, sub, solver):
    SubStringContinuation.__init__(solver, string, before, length, after, sub)
    self.a = helper.unwrap_int(after)
    self.l = self.startlength
  def activate(self, solver):
    if self.l < self.stoplength:
      b = len(self.string) - self.l - self.a
      assert b >= 0
      if self.l + b > len(self.string):
        self.l += 1
        return self.activate(solver)
      self.prepare_more_solutions(solver)
      self.before.unify(Integer(b), solver.env)
      self.after.unify(Integer(self.a), solver.env)
      self.length.unify(Integer(self.l), solver.env)
      l = self.l
      assert l >= 0
      self.sub.unify(string(self.string[b:b + l], cache=False), solver.env)
      self.l += 1
      return solver.set(self.nextcont)
    raise error.UnifyFail()

@builtin.macro()
def substring(solver, cont, string, before, length, after, sub):
  string = deref(string, solver.env)
  before = deref(before, solver.env)
  length = deref(length, solver.env)
  after = deref(after, solver.env)
  sub = deref(sub, solver.env)
  if not isinstance(before, Var):
    if before<0 or before>=len(string): return
  if not isinstance(length, Var):  
    if length<=0 or length>len(string): return
  if not isinstance(after, Var):
    if after<0 or after>len(string): return
  def sub_string_cont(value, solver):
    if not isinstance(sub, Var):
      if sub=='': return
      if isinstance(before, Var): startbefore, stopbefore = 0, len(string)+1
      else: startbefore, stopbefore = before, before+1
      for _ in unify(length, len(sub), solver.env):
        start  = startbefore
        while start<stopbefore:
          start = string.find(sub, start)
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
        for _ in sub.unify(string[before:after], solver.env):
          yield cont, string[before:after]
      elif not isinstance(before, Var) and  not isinstance(length, Var):
        if before+length>len(string): return
        for _ in sub.unify(string[before:after], solver.env):
          for _ in after.unify(before+length, solver.env):
            yield cont, string[before:before+length]
      elif not isinstance(length, Var) and  not isinstance(after, Var):
        if after-length<0: return
        for _ in sub.unify(string[after-length:after], solver.env):
          for _ in length.unify(length, solver.env):
            yield cont, string[after-length:after:after]
      elif not isinstance(before, Var):
        for leng in range(1, len(string)-before+1):
          for _ in sub.unify(string[before:before+leng], solver.env):
            for _ in length.unify(leng, solver.env):
              for _ in after.unify(before+leng, solver.env):
                yield cont, string[before:before+leng]
      elif not isinstance(after, Var):
        for leng in range(1, after):
          for _ in sub.unify(string[after-leng+1:after], solver.env):
            for _ in length.unify(leng, solver.env):
              for _ in before.unify(after-leng+1, solver.env):
                yield cont, string[before:after]
      elif not isinstance(length, Var):
        for start in range(len(string)-length):
          for _ in sub.unify(string[start:start+length], solver.env):
            for _ in before.unify(start, solver.env):
              for _ in after.unify(start+length, solver.env):
                yield cont, string[start:start+length]
      else:
        for start in range(len(string)):
          for leng in range(1, len(string)-start+1):
            for _ in sub.unify(string[start:start+leng], solver.env):
              for _ in before.unify(start, solver.env):
                for _ in length.unify(leng, solver.env):
                  for _ in after.unify(start+leng, solver.env):
                    yield cont, string[start:start+leng]
  yield sub_string_cont, True

