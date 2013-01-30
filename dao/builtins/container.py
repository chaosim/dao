# analysing and construction sequences

from dao.compilebase import CompileTypeError
from dao.command import special, Command, SpecialCall, BuiltinFunction
import dao.interlang as il
from dao.interlang import TRUE, FALSE, NONE

#@special
#def contain(compiler, cont, container, member): 
  #return container.cps_convert(compiler, il.clamda(container1,
    #il.Assign(container2, il.GetValue(container1)), 
    #member.cps_convert(compiler, il.clamda(member1, 
      #il.Assign(member2, mil.GetValue(container2)),
      #il.If(isLogicVar(member2),
            #il.begin(
              #il.Assign(iter1, il.Iter(container1)),
              #il.Next(iter1)),
            
  #if isinstance(member, Var):
    #for x in container:
      #return unify(member, x, solver)
  #elif member in container: 
    #return True

@special
def length(compiler, cont, sequence):
  return sequence.cps_convert(compiler, il.clamda(sequence1, 
    cont(il.Len(il.GetValue(sequence1)))))

def starstwith(x, y):
  try: x_startswith = x.startswith
  except: return x[:len(y)]== y
  return x_startswith(y)

def endswith(x, y):
  try: x_endswith = x.endswith
  except: return x[len(x)-len(y):]== y
  return x_endswith(y)

@special
def concat(compiler, cont, sequence1, sequence2, result):
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

@special
def subsequence(compiler, cont, sequence, before, length, after, sub):
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

@special
def conslist(*arguments): 
  return conslist(arguments)

@special
def pylist(*arguments): return list(arguments)

@special
def pytuple(*arguments): 
  return tuple(arguments)

@special
def head_list(head, tail): 
  if isinstance(tail, list): return [head]+tail
  else: return (head,)+tuple(tail)

@special
def list_tail(head, tail):
  if isinstance(head, list): return head+[tail]
  else: return head+(tail,)

@special
def index(sequence, index): 
  return sequence[index]

@special
def first(sequence): 
  return sequence[0]

@special
def left(sequence): 
  return sequence[1:]

@special
def second(sequence): 
  return sequence[1]

from dao.solve import DaoStopIteration

@special
def iter_next(iterator): 
  try: return iterator.next()
  except StopIteration:
##    iterator.close()
    raise DaoStopIteration

@special
def make_iter(iterator): 
  try: 
    iterator.next
    return iterator
  except: return iter(iterator)
  
@special
def to_list(item): 
  if isinstance(item, list) or isinstance(item, tuple): 
    return item
  return [item]

@special
def items(dict):
  return dict.items()
