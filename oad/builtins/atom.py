from oad import error
from oad.term import Var#, atom, SUCCESS, Integer
##from oad.cont import ChoiceContinuation
from oad import builtin
from oad import helper

# analysing and construction atoms

@builtin.macro()
def charin(evaluator, in_, whole): 
  in_ = in_.getvalue(evaluator.trail)
  whole = whole.getvalue(evaluator.trail)
  if in_.name not in whole.name: 
    raise error.UnifyFail()
  evaluator.value = SUCCESS

@builtin.macro()
def atom_length(evaluator, atom, length):
  atom = atom.deref(evaluator.trail)
  if isinstance(atom, Var): error.throw_instantiation_error()
  atom = helper.unwrap_atom(atom)
  length = length.deref(evaluator.trail)
  if not (isinstance(length, Var) or isinstance(length, Integer)):
    error.throw_type_error("integer", length)
  Integer(len(atom)).unify(length, evaluator.trail)
  evaluator.value = SUCCESS

class AtomConcatContinuation:#(ChoiceContinuation):
  def __init__(self, atom1, atom2, result, evaluator):
    ChoiceContinuation.__init__(self, evaluator)
    self.undotrail = evaluator.trail
    self.orig_fcont = evaluator.fcont
    self.atom1, self.atom2 = atom1, atom2
    self.result = repr(result)
    self.index = 0

  def activate(self, evaluator):
    # nondeterministic splitting of result
    if self.index<len(self.result)+1:
      self.prepare_more_solutions(evaluator)
      self.atom1.unify(atom(self.result[:self.index]), evaluator.trail)
      self.atom2.unify(atom(self.result[self.index:]), evaluator.trail)
      self.index += 1
      return evaluator.set(self.cont)
    raise error.UnifyFail()

@builtin.macro()
def atom_concat(evaluator, atom1, atom2, result):
  a1 = atom1.deref(evaluator.trail)
  a2 = atom2.deref(evaluator.trail)
  result = result.deref(evaluator.trail)
  if isinstance(a1, Var):
    if isinstance(a2, Var):
      return evaluator.set(AtomConcatContinuation(a1, a2, result, evaluator))
    else:
      s2 = str(a2)
      r = str(result)
      if r.endswith(s2):
        stop = len(r) - len(s2)
        assert stop>0
        a1.unify(atom(r[:stop]), evaluator.trail)
      else: raise error.UnifyFail()
  else:
    s1 = repr(a1)
    if isinstance(a2, Var):
      r = helper.convert_to_str(result)
      if r.startswith(s1):
        a2.unify(atom(r[len(s1):]), evaluator.trail)
      else: raise error.UnifyFail()
    else:
      s2 = str(a2)
      result.unify(atom(s1 + s2), evaluator.trail)
  evaluator.value = SUCCESS

class SubAtomContinuation:#(ChoiceContinuation):
  def __init__(self, atom, before, length, after, sub, evaluator):
    ChoiceContinuation.__init__(self, evaluator)
    self.undotrail = trail
    self.orig_fcont = evaluator.fcont
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
  def __init__(self, atom, before, length, after, sub, evaluator):
    SubAtomContinuation.__init__(self, atom, before, length, after, sub, evaluator)
    self.s1 = helper.unwrap_atom(sub)
    if len(self.s1)>=self.stoplength or len(self.s1)<self.startlength:
      raise error.UnifyFail()
    self.start = self.startbefore
  def activate(self, evaluator):
    start = self.start
    assert start >= 0
    end = self.stopbefore + len(self.s1)
    assert end>=0
    b = self.atom.find(self.s1, start, end) # XXX -1?
    if b<0: raise error.UnifyFail()
    self.prepare_more_solutions(evaluator)
    self.start = b + 1
    try:
      self.before.unify(Integer(b), evaluator.trail)
      self.after.unify(Integer(len(self.atom) - len(self.s1) - b), evaluator.trail)
      self.length.unify(Integer(len(self.s1)), evaluator.trail)
    except error.UnifyFail: pass
    return evaluator.set(self.cont)

  def __repr__(self): return "<SubAtomUEntitySubContinuation(%r)>" % self.__dict__

class SubAtomVarAfterContinuation:#(SubAtomContinuation):
  def __init__(self, atom, before, length, after, sub, evaluator):
    SubAtomContinuation.__init__(self, atom, before, length, after, sub, evaluator)
    self.b = self.startbefore
    self.l = self.startlength
  def activate(self, evaluator):
    if self.b < self.stopbefore:
      if self.l < self.stoplength:
        if self.l + self.b > len(self.atom):
          self.b += 1
          self.l = self.startlength
          return self.activate(fcont, trail)
        self.prepare_more_solutions(evaluator)

        self.before.unify(Integer(self.b), evaluator.trail)
        self.after.unify(Integer(
          len(self.atom) - self.l - self.b), evaluator.trail)
        self.length.unify(Integer(self.l), evaluator.trail)
        b = self.b
        l = self.l
        assert b >= 0
        assert l >= 0
        self.sub.unify(atom(self.atom[b:b + l]), trail)
        self.l += 1
        return evaluator.set(self.cont)
      else:
        self.b += 1
        self.l = self.startlength
        return self.activate(evaluator)
    raise error.UnifyFail()

class SubAtomElseContinuation:#(SubAtomContinuation):
  def __init__(self, atom, before, length, after, sub, evaluator):
    SubAtomContinuation.__init__(evaluator, atom, before, length, after, sub)
    self.a = helper.unwrap_int(after)
    self.l = self.startlength
  def activate(self, evaluator):
    if self.l < self.stoplength:
      b = len(self.atom) - self.l - self.a
      assert b >= 0
      if self.l + b > len(self.atom):
        self.l += 1
        return self.activate(evaluator)
      self.prepare_more_solutions(evaluator)
      self.before.unify(Integer(b), evaluator.trail)
      self.after.unify(Integer(self.a), evaluator.trail)
      self.length.unify(Integer(self.l), evaluator.trail)
      l = self.l
      assert l >= 0
      self.sub.unify(atom(self.atom[b:b + l], cache=False), evaluator.trail)
      self.l += 1
      return evaluator.set(self.nextcont)
    raise error.UnifyFail()

@builtin.macro()
def sub_atom(evaluator, atom1, before, length, after, sub):
  atom1 = atom1.deref(trail)
  if isinstance(atom1, Var): error.throw_instantiation_error()
  atom1 = helper.unwrap_atom(atom1)
  before = before.deref(evaluator.trail)
  length = length.deref(evaluator.trail)
  after = after.deref(evaluator.trail)
  sub = sub.deref(evaluator.trail)
  if not isinstance(sub, Var): cls = SubAtomUEntitySubContinuation
  elif isinstance(after, Var): cls = SubAtomVarAfterContinuation
  else: cls = SubAtomElseContinuation
  evaluator.set(cls(atom1, before, length, after, sub, evaluator))

