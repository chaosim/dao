from dao.term import Var, deref, unify, getvalue
from dao import builtin
from dao.solve import mycont

# analysing and construction sequences

@builtin.macro()
def contain(solver, container, member): 
  container = getvalue(container, solver.env, {})
  member = getvalue(member, solver.env, {})
  if isinstance(member, Var):
    for x in container:
      return unify(member, x, solver)
  elif member in container: 
    return True

@builtin.macro()
def length(solver, sequence, leng):
  sequence = getvalue(sequence, solver.env, {})
  if isinstance(sequence, Var): error.throw_instantiation_error()
  leng = getvalue(leng, solver.env, {})
  return unify(len(sequence), leng, solver)

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
      cont = solver.scont
      old_fcont = solver.fcont
      index_list =  (index for index in range(1, len(result)))
      @mycont(cont)
      def concat_cont(value, solver):
        try: index = index_list.next()
        except StopIteration: 
          solver.scont = old_fcont
          return
        if sequence1.unify(result[:index], solver) and\
               sequence2.unify(result[index:], solver):
          solver.scont = cont
          return result
      solver.scont = solver.fcont = concat_cont
      return True
    else:
      if endswith(result, sequence2):
        return sequence1.unify(result[:len(sequence2)], solver)
  else:
    if isinstance(sequence2, Var):
      if startswith(result, sequence1):
        return sequence2.unify(result[len(sequence1):], solver)
    else:
      return unify(result, sequence1+sequence2, solver)

@builtin.macro()
def subsequence(solver, sequence, before, length, after, sub):
  # sequence should be grounded.
  assert not isinstance(sequence, Var)
  sequence = deref(sequence, solver.env)
  before = deref(before, solver.env)
  length = deref(length, solver.env)
  after = deref(after, solver.env)
  sub = deref(sub, solver.env)
  if not isinstance(before, Var):
    if before<0 or before>=len(sequence): 
      solver.scont = solver.fcont
      return
  if not isinstance(length, Var):  
    if length<=0 or length>len(sequence):
      solver.scont = solver.fcont
      return
  if not isinstance(after, Var):
    if after<0 or after>len(sequence):
      solver.scont = solver.fcont
      return
  cont = solver.scont
  old_fcont = solver.fcont
  if not isinstance(sub, Var):
    if isinstance(before, Var): startbefore, stopbefore = 0, len(sequence)+1
    else: startbefore, stopbefore = before, before+1
    if unify(length, len(sub), solver):
      start  = [startbefore]
      @mycont(cont)
      def fcont(value, solver):
        if start[0]<stopbefore:
          start[0] = sequence.find(sub, start[0])
          if start[0]<0: 
            solver.scont = old_fcont
            return
          start[0] += 1
          if unify(before, start[0]-1, solver) and\
               unify(after, start[0]-1+len(sub), solver):
            solver.scont = cont
            return length
        else:
          solver.scont = old_fcont
          return
      solver.scont = solver.fcont = fcont
      return start[0]
  else:
    if not isinstance(before, Var) \
       and not isinstance(length, Var)\
       and not isinstance(after, Var):
      if start+length!=after: 
        solver.scont = old_fcont
        return
      if sub.unify(sequence[before:after], solver):
        return sequence[before:after]
    elif not isinstance(before, Var) and  not isinstance(length, Var):
      if before+length>len(sequence): 
        solver.scont = old_fcont
        return
      if sub.unify(sequence[before:after], solver) and\
         after.unify(before+length, solver):
          return sequence[before:before+length]
    elif not isinstance(length, Var) and  not isinstance(after, Var):
      if after-length<0: 
        solver.scont = old_fcont
        return
      if sub.unify(sequence[after-length:after], solver) and\
         length.unify(length, solver):
        return sequence[after-length:after:after]
    elif not isinstance(before, Var) and  not isinstance(after, Var):
      if sub.unify(sequence[before:after], solver) and\
         length.unify(length, solver):
        return sequence[after-length:after:after]
    elif not isinstance(before, Var):
      leng_list = (leng for leng in range(1, len(sequence)-before+1))
      @mycont(old_fcont)
      def cont1(value, solver):
        try:
          leng = leng_list.next()
          if sub.unify(sequence[before:before+leng], solver) and\
             length.unify(leng, solver) and\
             after.unify(before+leng, solver):
            solver.scont = cont
            return sequence[before:before+leng]
        except StopIteration:
          solver.scont = old_fcont
      solver.scont = cont1
    elif not isinstance(after, Var):
      leng_list = (leng for leng in range(1, after))
      @mycont(old_fcont)
      def cont1(value, solver):
        try:
          leng = leng_list.next()
          if sub.unify(sequence[after-leng+1:after], solver) and\
           length.unify(leng, solver) and\
           before.unify(after-leng+1, solver):
            solver.scont = cont
            return sequence[before:after]
        except StopIteration:
          solver.scont = old_fcont
      solver.scont = cont1
    elif not isinstance(length, Var):
      start_list = (start for start in range(len(sequence)-length))
      @mycont(old_fcont)
      def cont1(value, solver):
        try:
          start = start_list.next()
          if sub.unify(sequence[start:start+length], solver) and\
             before.unify(start, solver) and\
             after.unify(start+length, solver):
            solver.scont = cont
            return sequence[start:start+length]
        except StopIteration:
          solver.scont = old_fcont
      solver.scont = cont1
    else:
      start_leng_list = ((start, leng) for start in range(len(sequence))
                                   for leng in range(1, len(sequence)-start+1))
      @mycont(old_fcont)
      def cont1(value, solver):
        try:
          start, leng = start_leng_list.next()
          if sub.unify(sequence[start:start+leng], solver) and\
             before.unify(start, solver) and\
             length.unify(leng, solver) and\
             after.unify(start+leng, solver):
            solver.scont = cont
            return sequence[start:start+leng]
        except StopIteration:
          solver.scont = old_fcont
      solver.scont = solver.fcont = cont1

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
