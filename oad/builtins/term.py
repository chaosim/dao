from oad import helper 
from oad import error
from oad.term import Var, unify, deref
import oad.term as term
from oad import builtin

# analysing and construction terms

@builtin.macro()
def getvalue(solver, cont, item):
  yield cont, term.getvalue(item, solver.env)
  
@builtin.macro()
def ground_value(solver, cont, item, default=None):
  v = term.getvalue(item, solver.env)
  if isinstance(v, Var): v = default #deref(default, solver.env)
  yield cont, v
  
@builtin.macro()
def setvalue(solver, var, value):
  var = deref(var, solver.env)
  value = deref(value, solver.env)
  def setvalue_cont(value, solver):
    var = var.var
    if var in solver.env.bindings:
      v = solver.env.bindings[var]
      if isinstance(v, Var): var = v
    var.setvalue(value, solver.env)
    yield cont, True
  yield solver.cont(value, set_cont), True

@builtin.macro()
def functor(solver):
  assert isinstance(self, term.Term)
  t = self.elements[0].deref(trail)
  functor = self.elements[1].deref(trail)
  arity = self.elements[2].deref(trail)
  if helper.is_atomic(t):
    functor.unify(t, trail)
    arity.unify(term.Integer(0), trail)
  elif isinstance(t, term.Term):
    functor.unify(term.Atom(t.name), trail)
    arity.unify(term.Integer(len(t.elements)), trail)
  elif isinstance(t, term.Var):
    if isinstance(functor, term.Var): error.throw_instantiation_error()
    a = helper.unwrap_int(arity)
    if a<0: error.throw_domain_error("not_less_than_zero", arity)
    else:
      functor = helper.ensure_atomic(functor)
      if a==0: t.unify(helper.ensure_atomic(functor), trail)
      else:
        name = helper.unwrap_atom(functor)
        t.unify(term.Term(name, [term.Var() for i in range(a)]), trail)
  return scont, fcont, trail

class ArgContinuation:#(ChoiceContinuation):
  def __init__(solver, first, second, third):
    ChoiceContinuation.__init__(self, engine, scont)
    self.undotrail = trail
    self.orig_fcont = fcont
    self.first = first
    self.second = second
    self.third = third
    self.i = 0

  def activate(self, fcont, trail):
    if self.i < self.second.argument_count():
      fcont, trail = self.prepare_more_solutions(fcont, trail)
      arg = self.second.argument_at(self.i)
      self.i += 1
      try:
        self.third.unify(arg, trail)
        self.first.unify(term.Number(self.i), trail)
      except error.UnifyFail, e:
        return fcont, self.orig_fcont, trail
      return self.nextcont, fcont, trail
    raise error.UnifyFail()

@builtin.macro()
def argument(solver):
  assert isinstance(self, term.Term)
  first = self.elements[0].deref(trail)
  second = self.elements[1].deref(trail)
  third = self.elements[2].deref(trail)
  if isinstance(second, term.Var): error.throw_instantiation_error()
  if helper.is_atomic(second): raise error.UnificationFailed()
  if not helper.is_term(second): error.throw_type_error("compound", second)
  assert isinstance(second, term.Callable)
  if isinstance(first, term.Var):
    a = ArgContinuation(engine, scont, fcont, trail, first, second, third)
    return a, fcont, trail
  elif isinstance(first, term.Number):
    num = first.num
    if num == 0: raise error.UnifyFail
    if num < 0: error.throw_domain_error("not_less_than_zero", first)
    if num > second.argument_count(): raise error.UnifyFail()
    arg = second.argument_at(num - 1)
    third.unify(arg, trail)
  else: error.throw_type_error("integer", first)
  return scont, fcont, trail
      
@builtin.macro()
def univ(solver):
  assert isinstance(self, term.Term)
  first = self.elements[0].deref(trail)
  second = self.elements[1].deref(trail)
  if not isinstance(first, term.Var):
    if isinstance(first, term.Term):
      l = [term.Atom(first.name)] + list(first.elements)
    else: l = [first]
    u1 = helper.wrap_list(l)
    u1.unify(second, trail)
  else:
    if isinstance(second, term.Var): error.throw_instantiation_error()
    else:
      l = helper.unwrap_list(second)
      head = l[0]
      if not isinstance(head, term.Atom):
        error.throw_type_error("atom", head)
      t = term0(*l[1:])
      t.name = head.name
      t.unify(first, trail)
  return scont, fcont, trail

@builtin.macro()
def copy_term(solver, cont, item, copy):
  for _ in unify(copy, term.copy(item, {}), solver.env):
    yield cont, True
    