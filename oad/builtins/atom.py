from oad import error
from oad.term import Var, deref, unify, getvalue
from oad import builtin

# analysing and construction atoms

@builtin.macro()
def charin(solver, cont, in_, whole): 
  in_ = getvalue(in_, solver.env)
  whole = getvalue(whole, solver.env)
  if isinstance(in_, Var):
    for x in whole:
      for _ in unify(in_, x, solver.env):
        yield cont, True
  elif in_ in whole: 
    yield cont, True

@builtin.macro()
def atom_length(solver, cont, atom, length):
  atom = deref(atom, solver.env)
  if isinstance(atom, Var): error.throw_instantiation_error()
  length = deref(length, solver.env)
  if not (isinstance(length, Var) or isinstance(length, int)):
    error.throw_type_error("integer", length)
  for _ in unify(len(atom), length, solver.env): 
    yield cont, True

@builtin.macro()
def atom_concat(solver, cont, atom1, atom2, result):
  atom1 = deref(atom1, solver.env)
  atom2 = deref(atom2, solver.env)
  result = deref(result, solver.env)
  if isinstance(atom1, Var):
    index = 0
    if isinstance(atom2, Var):
      for index in range(1, len(result)):
        for _ in atom1.unify(result[:index], solver.env):
          for __ in atom2.unify(result[index:], solver.env):
            yield cont, True
    else:
      if result.endswith(atom2):
        for _ in atom1.unify(result[:len(atom2)], solver.env): yield cont, True
  else:
    if isinstance(atom2, Var):
      if result.startswith(atom1):
        for _ in atom2.unify(result[len(atom1):], solver.env): yield cont, True
    else:
      for _ in unify(result, atom1+atom2, solver.env): yield cont, True

class SubAtomUEntitySubContinuation:#(SubAtomContinuation):
  def __init__(self, atom, before, length, after, sub, solver):
    SubAtomContinuation.__init__(self, atom, before, length, after, sub, solver)
    self.s1 = helper.unwrap_atom(sub)
    if len(self.s1)>=self.stoplength or len(self.s1)<self.startlength:
      raise error.UnifyFail()
    self.start = self.startbefore
  def activate(self, solver):
    start = self.start
    assert start >= 0
    end = self.stopbefore + len(self.s1)
    assert end>=0
    b = self.atom.find(self.s1, start, end) # XXX -1?
    if b<0: raise error.UnifyFail()
    self.prepare_more_solutions(solver)
    self.start = b + 1
    try:
      self.before.unify(Integer(b), solver.env)
      self.after.unify(Integer(len(self.atom) - len(self.s1) - b), solver.env)
      self.length.unify(Integer(len(self.s1)), solver.env)
    except error.UnifyFail: pass
    return solver.set(self.cont)

  def __repr__(self): return "<SubAtomUEntitySubContinuation(%r)>" % self.__dict__

class SubAtomVarAfterContinuation:#(SubAtomContinuation):
  def __init__(self, atom, before, length, after, sub, solver):
    SubAtomContinuation.__init__(self, atom, before, length, after, sub, solver)
    self.b = self.startbefore
    self.l = self.startlength
  def activate(self, solver):
    if self.b < self.stopbefore:
      if self.l < self.stoplength:
        if self.l + self.b > len(self.atom):
          self.b += 1
          self.l = self.startlength
          return self.activate(fcont, trail)
        self.prepare_more_solutions(solver)

        self.before.unify(Integer(self.b), solver.env)
        self.after.unify(Integer(
          len(self.atom) - self.l - self.b), solver.env)
        self.length.unify(Integer(self.l), solver.env)
        b = self.b
        l = self.l
        assert b >= 0
        assert l >= 0
        self.sub.unify(atom(self.atom[b:b + l]), trail)
        self.l += 1
        return solver.set(self.cont)
      else:
        self.b += 1
        self.l = self.startlength
        return self.activate(solver)
    raise error.UnifyFail()

class SubAtomElseContinuation:#(SubAtomContinuation):
  def __init__(self, atom, before, length, after, sub, solver):
    SubAtomContinuation.__init__(solver, atom, before, length, after, sub)
    self.a = helper.unwrap_int(after)
    self.l = self.startlength
  def activate(self, solver):
    if self.l < self.stoplength:
      b = len(self.atom) - self.l - self.a
      assert b >= 0
      if self.l + b > len(self.atom):
        self.l += 1
        return self.activate(solver)
      self.prepare_more_solutions(solver)
      self.before.unify(Integer(b), solver.env)
      self.after.unify(Integer(self.a), solver.env)
      self.length.unify(Integer(self.l), solver.env)
      l = self.l
      assert l >= 0
      self.sub.unify(atom(self.atom[b:b + l], cache=False), solver.env)
      self.l += 1
      return solver.set(self.nextcont)
    raise error.UnifyFail()

@builtin.macro()
def sub_atom(solver, cont, atom, before, length, after, sub):
  atom = deref(atom, solver.env)
  before = deref(before, solver.env)
  length = deref(length, solver.env)
  after = deref(after, solver.env)
  sub = deref(sub, solver.env)
  if not isinstance(before, Var):
    if before<0 or before>=len(atom): return
  if not isinstance(length, Var):  
    if length<=0 or length>len(atom): return
  if not isinstance(after, Var):
    if after<0 or after>len(atom): return
  def sub_atom_cont(value, solver):
    if not isinstance(sub, Var):
      if sub=='': return
      if isinstance(before, Var): startbefore, stopbefore = 0, len(atom)+1
      else: startbefore, stopbefore = before, before+1
      for _ in unify(length, len(sub), solver.env):
        start  = startbefore
        while start<stopbefore:
          start = atom.find(sub, start)
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
        for _ in sub.unify(atom[before:after], solver.env):
          yield cont, atom[before:after]
      elif not isinstance(before, Var) and  not isinstance(length, Var):
        if before+length>len(atom): return
        for _ in sub.unify(atom[before:after], solver.env):
          for _ in after.unify(before+length, solver.env):
            yield cont, atom[before:before+length]
      elif not isinstance(length, Var) and  not isinstance(after, Var):
        if after-length<0: return
        for _ in sub.unify(atom[after-length:after], solver.env):
          for _ in length.unify(length, solver.env):
            yield cont, atom[after-length:after:after]
      elif not isinstance(before, Var):
        for leng in range(1, len(atom)-before+1):
          for _ in sub.unify(atom[before:before+leng], solver.env):
            for _ in length.unify(leng, solver.env):
              for _ in after.unify(before+leng, solver.env):
                yield cont, atom[before:before+leng]
      elif not isinstance(after, Var):
        for leng in range(1, after):
          for _ in sub.unify(atom[after-leng+1:after], solver.env):
            for _ in length.unify(leng, solver.env):
              for _ in before.unify(after-leng+1, solver.env):
                yield cont, atom[before:after]
      elif not isinstance(length, Var):
        for start in range(len(atom)-length):
          for _ in sub.unify(atom[start:start+length], solver.env):
            for _ in before.unify(start, solver.env):
              for _ in after.unify(start+length, solver.env):
                yield cont, atom[start:start+length]
      else:
        for start in range(len(atom)):
          for leng in range(1, len(atom)-start+1):
            for _ in sub.unify(atom[start:start+leng], solver.env):
              for _ in before.unify(start, solver.env):
                for _ in length.unify(leng, solver.env):
                  for _ in after.unify(start+leng, solver.env):
                    yield cont, atom[start:start+leng]
  yield sub_atom_cont, True

