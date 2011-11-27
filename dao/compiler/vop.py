
# virtual operations

# has side effect on variable, environment, parser_state?
# pure functions? 
# depend global varialbes?
# io

'''
root item: module exports, globals; io item

reachable items from root item
unreachable items from root item

dead variables

dead code

constant propagatation

loop

'''

class VirtualOperation: 
  def __init__(self):
    pass

class Deref(VirtualOperation):
  def __init__(self, exp):
    self.exp = exp
  def __eq__(self, other):
    return isinstance(other, GetValue) and other.exp==self.exp
  def __repr__(self): return 'GetValue(%s)'%self.exp
  
class GetValue(VirtualOperation):
  def __init__(self, exp):
    self.exp = exp
  def __eq__(self, other):
    return isinstance(other, GetValue) and other.exp==self.exp
  def __repr__(self): return 'GetValue(%s)'%self.exp
  
class GetClosure(VirtualOperation):
  def __init__(self, exp):
    self.exp = exp
  def __eq__(self, other):
    return isinstance(other, GetValue) and other.exp==self.exp
  def __repr__(self): return 'GetClosure(%s)'%self.exp
  
class ContVal(VirtualOperation):
  def __init__(self):
    pass
  def __eq__(self, other):
    return isinstance(other, ContVal)
  def __repr__(self): return 'getval'
  
class SetVal(VirtualOperation):
  def __init__(self, value):
    self.value = value
  def __eq__(self, other):
    return isinstance(other, SetVal)
  def __repr__(self): return 'getval'
  
class Concat(VirtualOperation):
  def __init__(self, head, tail):
    self.head, self.tail = head, tail
  def __eq__(self, other):
    return isinstance(other, SetVal)
  def __repr__(self): return 'Concat(%s, %s)'%self.head, self.tail

class SaveParseState(VirtualOperation):
  def __init__(self):
    pass
  def __eq__(self, other):
    return isinstance(other, SaveParseState)
  def __repr__(self): return 'SaveParseState'
  
class RestoreParseState(VirtualOperation):
  def __init__(self):
    pass
  def __eq__(self, other):
    return isinstance(other, SaveParseState)
  def __repr__(self): return 'RestoreParseState'

class Fail(VirtualOperation):
  #solver.scont = solver.fcont
  def __init__(self):
    pass
  def __eq__(self, other):
    return isinstance(other, Fail)
  def __repr__(self): return 'Fail'
fail = Fail()
  
class VirtualOperationInstance:
  def __init__(self, owner_cont, vop):
    self.owner_cont, vop = owner_cont, self.vop
  def get_data_dependent_cont(self):
    pass