
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

loop

'''

class VirtualOperation: 
  def __init__(self):
    pass

class GetVarValue(VirtualOperation):
  def __init__(self, var):
    self.var = var
  def __eq__(self, other):
    return isinstance(other, GetVarValue) and other.var==self.var
  def __repr__(self): return 'GetVarValue(%s)'%self.var
  
class GetClosure(VirtualOperation):
  def __init__(self, exp):
    self.exp = exp
  def __eq__(self, other):
    return isinstance(other, GetVarValue) and other.exp==self.exp
  def __repr__(self): return 'GetClosure(%s)'%self.exp
  
class GetVal(VirtualOperation):
  def __init__(self):
    pass
  def __eq__(self, other):
    return isinstance(other, GetVal)
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
  
class VirtualOperationInstance:
  def __init__(self, owner_cont, vop):
    self.owner_cont, vop = owner_cont, self.vop
  def get_data_dependent_cont(self):
    pass
  