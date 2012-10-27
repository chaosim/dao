# too complicated to inline these code, except knowing the type of x, y in compile time.
def unify(x, y):
  try: x_unify = x.unify
  except AttributeError: # given AttributeError to make GeneratorExit happy.
    try: y_unify = y.unify
    except AttributeError: 
      return (True, ) if x==y else ()
    return y_unify(x)
  return x_unify(y)
    
class Var:
  def __init__(self, name=''):
    self.name = name
    self.binding = self

  def unify(self, other):
    self_ref = self.deref()
    other_ref = other.deref() if isinstance(other, Var) else other
    if isinstance(self_ref, Var):
      if self_ref is other_ref: 
        # shorten the chain of binding
        self.binding = other_ref 
        other.binding = other_ref
        yield True
      else:
        other.binding = other_ref # shorten the chain
        # link the two chain of binding, need restore when backtracking
        self.binding = other_ref
        self_ref.binding = other_ref
        yield True
        self.binding = self_ref
        self_ref.binding = self_ref
    elif isinstance(other_ref, Var):
      self.binding = self_ref
      other.binding = self_ref
      other_ref.binding = self_ref
      yield True
      other.binding = other_ref
      other_ref.binding = other_ref
    else:
      return unify(self_ref, other_ref, solver, occurs_check)
    
  # def unify_rule_head(self, head, solver, subst): # no unify_rule_head method, is compiled to target py code
  

  # def take_value(self, env)
  # def setvalue(self, value, env)
  # def copy(self, memo)
  # def free(self, env) 
  # def closure(self, env)
  # def match(self, other)
  #def cont(self, cont, solver)
  
  # OMG, who need to compile the compiled target element?!
  # It is impossible to have this in any target element!!!
  #def compile_to_cont(self, cont, compiler): # target element is impossible have this
  
  # no shorten the ref chains, to do it in builtin deref, unify.
  def deref(self):
    result = self
    while isinstance(result, Var) and result.binding is not result:
      result = result.binding
    return result 
  
  # no shorten the ref chains, to do it in builtin getvalue, unify.
  def getvalue(self, memo):
    try: return memo[self]
    except:
      result = self.deref()
      if isinstance(result, Var): return result
      try: result_getvalue = result.getvalue
      except: return result
      return result_getvalue(memo)
    
  def __repr__(self):  return '%s'%self.name
  def __eq__(self, other): return self is other
  def __hash__(self): return hash(id(self))
 
# It's necessary in target?
# Can't I eliminate it while compiling? Of course I can.
# class DummyVar

# same as DummyVar 
# class NullVar

# how about ClosureVar?
# class ClosureVar

# how about compile Cons?
# Cons become tuple? No?!
# how to compile Cons.unify?
class Cons: 
  def __init__(self, head, tail):
    self.head, self.tail = head, tail
    
  # how about compiled this?
  # yield should be used in target code? absolutely!!! 
  # so solver should disappear.
  # how to implement it? 
  def unify(self, other): # , solver, use yield, no solver!!!
    if isinstance(other, Var):
      ref = other.deref()
      if isinstance(ref, Var): 
        ref.binding = self
        other.binding = self
        yield True
        other.binding = ref.binding = ref
      else:
        for _ in unify(self, ref): yield True
    else:
      if not isinstance(other, Cons): return
      for _ in unify(self.head, other.head):
        for _ in unify(self.tail, other.tail):
          return True

  def match(self, other):
    if not isinstance(other, Cons): return False
    return match(self.head, other.head) and match(self.tail, other.tail)

  def unify_rule_head(self, other, solver, subst):
    if not isinstance(other, Cons): return
    for _ in unify_rule_head(self.head, other.head, solver, subst):
      for _ in unify_rule_head(self.tail, other.tail, solver, subst):
        yield True
          
  def copy_rule_head(self, env):
    head = copy_rule_head(self.head, env)
    tail = copy_rule_head(self.tail, env)
    if head==self.head and tail==self.tail: return self
    return Cons(head, tail)

  def getvalue(self, env, memo):
    head = getvalue(self.head, env, memo)
    tail = getvalue(self.tail, env, memo)
    if head==self.head and tail==self.tail:
      return self
    return Cons(head, tail)
  
  def take_value(self, env):
    head = take_value(self.head, env)
    tail = take_value(self.tail, env)
    if head==self.head and tail==self.tail:
      return self
    return Cons(head, tail)

  def copy(self, memo): 
    return Cons(copy(self.head, memo), copy(self.tail, memo))
  def closure(self, env): 
    head = closure(self.head, env)
    tail = closure(self.tail, env)
    if head==self.head and tail==self.tail:
      return self
    return Cons(head, tail)
  
  def __eq__(self, other): 
    return self.__class__==other.__class__ and self.head==other.head and self.tail==other.tail
     
  def __iter__(self):
    tail = self 
    while 1:
      yield tail.head
      if tail.tail is nil: return
      elif isinstance(tail.tail, Cons): 
        tail = tail.tail
      else: 
        yield tail.tail
        return
  def __len__(self): return len([e for e in self])
  def __repr__(self): return 'L(%s)'%' '.join([repr(e) for e in self])

cons = Cons

class Nil: 
  def __len__(self): return 0
  def __iter__(self): 
    if 0: yield
  def __repr__(self): return 'nil'

nil = Nil()

def conslist(*elements): 
  result = nil
  for term in reversed(elements): result = Cons(term, result)
  return result

def cons2tuple(item):
  if not isinstance(item, Cons) and not isinstance(item, list) \
     and not isinstance(item, tuple): 
    return item
  return tuple(cons2tuple(x) for x in item)
