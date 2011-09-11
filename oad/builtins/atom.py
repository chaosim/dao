from oad import error
from oad.term import Var
from oad import builtin

# analysing and construction atoms

@builtin.macro()
def charin(solver, in_, whole): 
  in_ = deref(in_, solver.env)
  whole = deref(whole, solver.env)
  if isinstance(in_, Var):
    for x in whole:
      for _ in unify(in_, x, solver.env):
        yield True
  elif in_ in whole: 
    yield True

@builtin.macro()
def atom_length(solver, atom, length):
  atom = deref(atom, solver.env)
  if isinstance(atom, Var): error.throw_instantiation_error()
  length = deref(length, solver.env)
  if not (isinstance(length, Var) or isinstance(length, int)):
    error.throw_type_error("integer", length)
  for _ in unify(len(atom), length, solver.env): 
    yield True

@builtin.macro()
def atom_concat(solver, atom1, atom2, result):
  atom1 = deref(atom1, solver.env)
  atom2 = deref(atom2, solver.env)
  result = result.deref(solver.env)
  if isinstance(atom1, Var):
    index = 0
    if isinstance(atom2, Var):
      for index in range(len(result)):
        for _ in atom1.unify(result[:self.index+1], solver.env):
          for __ in atom2.unify(result[self.index+1:], solver.env):
            yield True
    else:
      if result.endswith(atom2):
        for _ in atom1.unify(result[:len(atom2)+1], solver.env):
          yield True
  else:
    if isinstance(atom2, Var):
      if r.startswith(s1):
        a2.unify(atom(r[len(s1):]), solver.env)
      else: raise error.UnifyFail()
    else:
      s2 = str(a2)
      result.unify(atom(s1 + s2), solver.env)
  solver.value = True

class SubAtomContinuation:#(ChoiceContinuation):
  def __init__(self, atom, before, length, after, sub, solver):
    ChoiceContinuation.__init__(self, solver)
    self.undotrail = trail
    self.orig_fcont = solver.fcont
    self.atom = atom
    self.before = before
    self.length = length
    self.after = after
    self.sub = sub
    self.setup()

  def setup(self):
    if isinstance(self.length, Var):
      self.startlength = 0
      self.stoplength = len(self.atom) + 1
    else:
      self.startlength = helper.unwrap_int(self.length)
      self.stoplength = self.startlength + 1
      if self.startlength < 0:
        self.startlength = 0
        self.stoplength = len(self.atom) + 1
    if isinstance(self.before, Var):
      self.startbefore = 0
      self.stopbefore = len(self.atom) + 1
    else:
      self.startbefore = helper.unwrap_int(self.before)
      if self.startbefore < 0:
        self.startbefore = 0
        self.stopbefore = len(self.atom) + 1
      else:
        self.stopbefore = self.startbefore + 1

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
def sub_atom(solver, atom, before, length, after, sub):
  atom = deref(atom1, solver.env)
  before = deref(before, solver.env)
  length = deref(length, solver.env)
  after = deref(after, solver.env)
  sub = deref(sub, solver.env)
  if not isinstance(sub, Var): cls = SubAtomUEntitySubContinuation
  elif isinstance(after, Var): cls = SubAtomVarAfterContinuation
  else: cls = SubAtomElseContinuation
  solver.set(cls(atom1, before, length, after, sub, solver))

