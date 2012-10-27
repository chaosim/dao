# control flow, control dependency
# data flow, data dependency
# optimization based on continuations

from dao.compiler.compilebase import code

class ContGraph:
  def __init__(self): 
    self.succ_dict = {}
    self.prev_dict = {}

class DataFlowGraph:
  def __init__(self): 
    self.depend_dict = {}
    self.flow_dict = {}

class ContCall:
  def __init__(self, caller, argument):
    self.caller, self.argument = caller, argument
  def __eq__(self, other):
    return isinstance(other, ContCall) and self.caller==other.caller and self.argument==other.argument
  def __call__(self, argument):
    return ContCall(self, argument)    
  def __repr__(self):
    return '%r(%r)'%(self.caller, self.argument)
  
class Cont:
  def __init__(self, succ, prev):
    self.succ = succ
    if isinstance(succ, Cont):
      succ.prev = self
    self.prev = prev
    if isinstance(prev, Cont):
      prev.succ = self
    self.data_dependent = set([])
    self.vops = []
    self.depend_prev_value = True
  
  def __call__(self, argument):
    return ContCall(self, argument)
  
  def depend_on(self, other):
    if other is self.prev:
      return self.depend_prev_value
    else:
      return self.depend_prev_value and self.prev.depend_on(other)
        
  def __hash__(self): return 1
  def __eq__(self, other):
    if self is other: return True
    if not isinstance(other, self.__class__): return False
    if not self.succ==other.succ: return False
    return self.__class__._eq(self, other)
  def _eq(self, other): 
    return True
  
  def set_succ(self, cont):
    self.succ = cont
    cont.prev = self
  def set_prev(self, cont):
    self.prev = cont
    cont.succ = self
    
  def __repr__(self):
    return '%s'%(self.label)

class DoneCont(Cont):
  label = 'done'
  def __init__(self):
    Cont.__init__(self, None, None)
  
  def code(self):
    if self.prev is not None:
      return self.prev.code()
    else: return ''
  
  def __repr__(self): return 'done'

done = DoneCont()

class FailDoneCont(Cont):
  label = 'fail'
  def __init__(self):
    Cont.__init__(self, None, None)

  def code(self):
    if self.prev is not None:
      return self.prev.code()
    else: return ''
    
  def __repr__(self): return 'fail'
  
fail_done = FailDoneCont() 

class ValueCont(Cont):
  label = 'V'
  def __init__(self, exp, cont):
    Cont.__init__(self, cont, None)
    self.exp = exp
    self.data_dependent = set()
    self.vops = []
    self.depend_prev_value = False
  def code(self):
    return repr(self.exp)
    #try: self_prev = self.prev
    #except: return code(self.exp)
    #return self_prev.code()
  def _eq(self, other):
    return self.exp==other.exp
  def __repr__(self):
    return 'V(%s, %s)'%(repr(self.exp), self.succ.label)
V = ValueCont

class SetCont(Cont):
  label = 'Set'
  def __init__(self, var, cont):
    Cont.__init__(self, cont, None)
    self.var = var
  def code(self):
    return '%s = %s'%(code(self.var), code(self.prev))
  def _eq(self, other): 
    return self.var==other.var
  def __repr__(self):
    return 'Set(%s, %s)'%(self.var, self.succ.label)
Set = SetCont

class IfCont(Cont):
  label = 'If'
  def __init__(self, then_cont, else_cont):
    Cont.__init__(self, None, None)
    self.then_cont, self.else_cont = then_cont, else_cont
    #self.data_dependent = self.prev
  def _eq(self, other):
    return self.then_cont==other.then_cont and self.else_cont==other.else_cont
  
  def code(self):
    return 'if value: %s\nelse:%s'%(self.then_cont.code(), self.else_cont.code())
  
  def __repr__(self):
    return 'If(%s, %s)'%(self.then_cont.label, self.else_cont.label)
If = IfCont

class ApplyCont(Cont):
  label = 'App'
  def __init__(self, operator, cont):
    Cont.__init__(self, cont, None)
    self.operator = operator
  def code(self):
    return '%s(%s, %s)'%code(self.operator)
  def _eq(self, other):
    return self.operator==other.operator
  def __repr__(self): 
    return 'App(%r, %s)'%(self.operator, self.succ.label)
App = ApplyCont

class ArgumentCont(Cont):
  label = 'Arg'
  def __init__(self):
    Cont.__init__(self, None, None)
  def __repr__(self): 
    return 'arg'
Arg = ArgumentCont

class GatherCont(Cont):
  label = 'Gat'
  def __init__(self, arg, cont):
    Cont.__init__(self, cont, None)
    self.arg = arg
    #self.vop = SetVal(Concat(GetAttr('arg'), ContVal()))
  def _eq(self, other):
    return self.arg==other.arg
  def __repr__(self):
    return 'Gat(%s, %s)'%(self.arg, self.succ.label)
    
Gat = GatherCont
  
class SelectFunMacroCont(Cont):
  label = 'Gat'
  def __init__(self, operator, fun_cont, builtin_fun_cont, macro_cont):
    Cont.__init__(self, None, None)
    self.operator = operator
    self.fun_cont, self.builtin_fun_cont = fun_cont, builtin_fun_cont
    self.macro_cont = macro_cont
  def _eq(self, other):
    return self.arg==other.arg
  def __repr__(self):
    return 'Sel/F/M(%s: %s, %s, %s)'%(self.operator.label, self.fun_cont.label, 
                                  self.builtin_fun_cont.label, self.macro_cont.label)
  
Sel = SelectFunMacroCont

class BlockCont(Cont):
  label = 'Blk'
  def __init__(self, succ):
    Cont.__init__(self, succ, None) 
  def __repr__(self):
    return 'Blk(%s)'%(self.succ)
Blk = BlockCont

class CatchCont(Cont):
  label = 'Catch'
  def __init__(self, succ, label_cont):
    Cont.__init__(self, succ, None)
    self.label_cont = label_cont
    # vop: label_cont.tag, label_cont.env = tag, compiler.env
  def _eq(self, other):
    return self.label_cont==other.label_cont
  def __repr__(self):
    return 'Catch(%s)'%(self.succ)
Catch = CatchCont

class LabelCont(Cont):
  label = 'Lbl'
  def __init__(self, succ):
    Cont.__init__(self, succ, None) 
  def __repr__(self):
    return 'Lbl(%s)'%(self.succ)
Lbl = LabelCont

class ThrowCont(Cont):
  label = 'Throw'
  def __init__(self, succ):
    Cont.__init__(self, succ, None)
    #solver.scont = lookup(throw_cont, tag, throw_cont, solver)
    #throw_cont.form, throw_cont.env = self.form, solver.env
  def __repr__(self):
    return 'Throw(%s)'%(self.succ)
Throw = ThrowCont

class LetVarCont(Cont):
  label = 'LetVar'
  def __init__(self, env, var, next_cont):
    Cont.__init__(self, next_cont, None)
    self.env, self.var = env, var
    #solver.scont = lookup(throw_cont, tag, throw_cont, solver)
    #throw_cont.form, throw_cont.env = self.form, solver.env
  def __repr__(self):
    return 'LetVar(%s)'%(self.var)
  