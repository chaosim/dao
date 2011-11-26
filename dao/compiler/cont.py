# control flow, control dependency
# data flow, data dependency
# optimization based on continuations

class ContGraph:
  def __init__(self): 
    self.succ_dict = {}
    self.prev_dict = {}

class DataFlowGraph:
  def __init__(self): 
    self.depend_dict = {}
    self.flow_dict = {}
    
class Cont:
  def __init__(self, cont):
    self.succ = cont
    cont.prev = self
    self.prev = None
    self.data_dependent = set([])
    self.vops = []
    self.label = self.__class__.__name__[:3]
    self.depend_prev_value = True
  def depend_on(self, other):
    if other is self.prev:
      return self.depend_prev_value
    else:
      return self.depend_prev_value and self.prev.depend_on(other)
        
  def __hash__(self): return 1
  def __eq__(self, other):
    return isinstance(other, self.__class__) and self.succ==other.succ
  def set_succ(self, cont):
    self.succ = cont
    cont.prev = self
  def set_prev(self, cont):
    self.prev = cont
    cont.succ = self
  def __repr__(self):
    return '%s(%s)'%(self.__class__.__name__, self.succ.__class__.__name__)

class DoneCont(Cont):
  def __init__(self):
    self.label = 'done'
    self.depend_prev_value = True

  def __repr__(self): return 'done'
  
  def __eq__(self, other): 
    return isinstance(other, DoneCont)
  
done = DoneCont()

class ValueCont(Cont):
  def __init__(self, exp, cont):
    Cont.__init__(self, cont)
    self.exp = exp
    self.data_dependent = set()
    self.vops = []
    self.label = 'V'
    self.depend_prev_value = False
  def code(self):
    return code(self.exp)
  def __eq__(self, other):
    return Cont.__eq__(self, other) and self.exp==other.exp
  def __repr__(self):
    return 'V(%s, %s)'%(repr(self.exp), self.succ.label)
V = ValueCont

class SetCont(Cont):
  def __init__(self, var, cont):
    Cont.__init__(self, cont)
    self.var = var
    self.label = 'Set'
  def code(self):
    return '%s = %s'%(code(self.var), code(self.cont))
  def __eq__(self, other): 
    return Cont.__eq__(self, other) and self.var==other.var
  def __repr__(self):
    return 'Set(%s, %s)'%(self.var, self.cont.label)
Set = SetCont

class IfCont(Cont):
  def __init__(self, then_cont, else_cont):
    self.then_cont, self.else_cont = then_cont, else_cont
    #self.data_dependent = self.prev
    self.label = 'If'
  def __eq__(self, other):
    return isinstance(other, IfCont) and self.then_cont==other.then_cont and self.else_cont==other.else_cont
  def __repr__(self):
    return 'If(%s, %s)'%(self.then_cont.__class__.__name__, self.else_cont.label)
If = IfCont

class ApplyCont(Cont):
  def __init__(self, operator, cont):
    Cont.__init__(self, cont)
    self.operator = operator
    self.label = 'App'
  def __eq__(self, other):
    return Cont.__eq__(self, other) and self.operator==other.operator
  def __repr__(self): 
    return 'App(%r, %s)'%(self.operator, self.succ.label)
App = ApplyCont

class ArgumentCont:
  def __init__(self):
    self.label = 'Arg'
  def __hash__(self): return 1
  def __eq__(self, other):
    return isinstance(other, ArgumentCont)
  def __repr__(self): 
    return 'arg'
Arg = ArgumentCont

class GatherCont(Cont):
  def __init__(self, arg, cont):
    Cont.__init__(self, cont)
    self.arg = arg
    self.label = 'Gat'
    #self.vop = SetVal(Concat(GetAttr('arg'), GetVal()))
  def __eq__(self, other):
    return Cont.__eq__(self, other) and self.arg==other.arg
  def __repr__(self):
    return 'Gat(%s, %s)'%(self.arg, self.succ.label)
    
Gat = GatherCont
  