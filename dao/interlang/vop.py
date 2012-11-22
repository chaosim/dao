from dao.base import classeq

from dao.compilebase import optimize, MAX_EXTEND_CODE_SIZE, to_code_list
from dao.compilebase import VariableNotBound, CompileTypeError
from element import Element, Begin, Assign#, element
from lamda import Apply, optimize_once_args, Var, LocalVar, clamda
from element import pythonize_args, FALSE

#from element import Integer

class BinaryOperation(Element):
  def __init__(self, name, operator, have_side_effects=True):
    self.name, self.operator = name, operator
    self.have_side_effects = have_side_effects
  
  def alpha_convert(self, env, compiler):
    return self
  
  def assign_convert(self, env, compiler):
    return self
  
  def optimization_analisys(self, data):  
    return self
  
  def subst(self, bindings):  
    return self

  def optimize_once(self, data):
    return self, False
  
  def code_size(self): 
    return 1
  
  def pythonize_exp(self, env, compiler):
    return (self,), False
    
  def to_code(self, coder):
    return self.operator
      
  def __call__(self, *args):
    return BinaryOperationApply(self, args)
  
  def __eq__(x, y):
      return classeq(x, y) and x.operator==y.operator
    
  def __hash__(self): return hash(self.operator)
  
  def __repr__(self):
    return 'il.%s'%self.name

add = BinaryOperation('add', '+', False)
sub = BinaryOperation('sub', '-', False)
mul = BinaryOperation('mul', '*', False)
div = BinaryOperation('div', '/', False)
in_ = BinaryOperation('in', 'in', False)

class BinaryOperationApply(Apply):
  is_statement = False
  
  def __init__(self, caller, args):
    self.caller, self.args = caller, args

  def alpha_convert(self, env, compiler):
    return self.__class__(self.caller.alpha_convert(env, compiler), 
                 tuple(arg.alpha_convert(env, compiler) for arg in self.args))
  
  def assign_convert(self, env, compiler):
    return self.__class__(self.caller.assign_convert(env, compiler), 
                 tuple(arg.assign_convert(env, compiler) for arg in self.args))    
    
  def optimization_analisys(self, data):  
    data.called_count[self.caller] = data.called_count.setdefault(self.caller, 0)+1
    self.caller.optimization_analisys(data)
    for arg in self.args:
      arg.optimization_analisys(data)
        
  def code_size(self):
    return self.caller.code_size()+sum([x.code_size() for x in self.args])
              
  def side_effects(self):
    if isinstance(self.caller, Var): return True
    elif self.caller.have_side_effects: return True
    else: return False # after cps, all of value have been solved before called, 
                       # so have no side effects.
          
  def subst(self, bindings):  
    return self.__class__(self.caller.subst(bindings), 
                 tuple(arg.subst(bindings) for arg in self.args))
      
  def optimize_once(self, data):    
    changed = False
    caller, changed1 = self.caller.optimize_once(data)
    args, changed2 = optimize_once_args(self.args, data)
    return self.__class__(caller, args), changed1 or changed2

  def insert_return_yield(self, klass):
    return klass(self)
  
  def pythonize_exp(self, env, compiler):
    exps, args, has_statement = pythonize_args(self.args, env, compiler)
    return exps+(self.__class__(self.caller, args),), has_statement
  
  def to_code(self, coder):
    if not self.caller.operator[0].isalpha():
      return '%s%s%s'%(self.args[0].to_code(coder), 
                        self.caller.to_code(coder), 
                        self.args[1].to_code(coder))
    else:
      return '%s %s %s'%(self.args[0].to_code(coder), 
                              self.caller.to_code(coder), 
                              self.args[1].to_code(coder))      
    
  def __repr__(self):
    return '%r(%r)'%(self.caller, self.args)
  
  def __repr__(self):
    return '%r%s%r'%(self.args[0], self.caller.operator, self.args[1])
  

class VirtualOperation(Element):
  def __init__(self, *args):
    if self.arity>=0:
      assert len(args)==self.arity, \
           '%s should have %s arguments.'%(
             self.__class__.__name__, self.__class__.arity)
    self.args = args
  
  def __call__(self, *args):
    return Apply(self, args)

  def alpha_convert(self, env, compiler):
    return self.__class__(*tuple(arg.alpha_convert(env, compiler) for arg in self.args))
  
  def assign_convert(self, env, compiler):
    return self.__class__(*tuple(arg.assign_convert(env, compiler) for arg in self.args))
  
  def find_assign_lefts(self):
    return set()
  
  def side_effects(self):
    return True

  def optimization_analisys(self, data):
    for arg in self.args:
      arg.optimization_analisys(data)
  
  def subst(self, bindings):  
    return self.__class__(*tuple(x.subst(bindings) for x in self.args))
  
  def code_size(self):
    return 1
  
  def optimize_once(self, data):
    return self, False
  
  def insert_return_yield(self, klass):
    return klass(self)
  
  def replace_return_yield(self, klass):
    return self
  
  def pythonize_exp(self, env, compiler):
    exps, args, has_statement = pythonize_args(self.args, env, compiler)
    try: self_is_statement = self.is_statement
    except: self_is_statement = False
    return exps+(self.__class__(*args),), self_is_statement or has_statement
  
  def to_code(self, coder):
    if isinstance(self.__class__.code_format, str):
      if self.__class__.arity==0:
        return self.__class__.code_format
      elif self.__class__.arity!=-1:
        return self.__class__.code_format % tuple(x.to_code(coder) for x in self.args)
      else:
        return self.__class__.code_format % (', '.join([x.to_code(coder) for x in self.args]))
    else: 
      return self.__class__.code_format(self, coder)
    
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
      
  def __hash__(self):
    return hash(self.__class__.__name__)

  def __repr__(self):
    try: 
      if self.arity==0: 
        return 'il.%s'%self.__class__.__name__
    except: pass
    return 'il.%s(%s)'%(self.__class__.__name__, 
              ', '.join([repr(x) for x in self.args]))
   
def vop(name, arity, code_format):
  class Vop(VirtualOperation): pass
  Vop.name = Vop.__name__  = name
  Vop.arity = arity
  Vop.code_format = code_format
  Vop.is_statement = False
  return Vop

class VirtualOperation2(VirtualOperation):
  def insert_return_yield(self, klass):
    return self
  
  def replace_return_yield(self, klass):
    return self
    
def vop2(name, arity, code_format):
  class Vop(VirtualOperation2): pass
  Vop.__name__ = name
  Vop.arity = arity
  Vop.code_format = code_format
  Vop.is_statement = True
  return Vop

class GetItem(Element):
  arity = 2
  
  def __repr__(self):
    return '%r[%r]'%(self.args)

GetItem = vop('GetItem', 2, '(%s)[%s]')  
Not = vop('Not', 1, "not %s")
def AssignFromList_to_code(self, coder):
  return "%s = %s" % (', '.join([x.to_code(coder) for x in self.args[:-1]]), 
                      self.args[-1].to_code(coder))
AssignFromList = vop2('AssignFromList', -1, AssignFromList_to_code)
Isinstance = vop('Isinstance', 2, "isinstance(%s, %s)")
EmptyList = vop('empty_list', 0, '[]')
empty_list = EmptyList()
ListAppend = vop('ListAppend', 2, '%s.append(%s)')
Len = vop('Len', 1, 'len(%s)')
RaiseTypeError = vop2('RaiseTypeError', 1, 'raise %s')
SetExitBlockContMap = vop2('SetExitBlockContMap', 2, 'solver.exit_block_cont_map[%s] = %s')
SetContinueBlockContMap = vop2('SetContinueBlockContMap', 2, 'solver.continue_block_cont_map[%s] = %s')
GetExitBlockCont = vop('GetExitBlockCont', 1, 'solver.exit_block_cont_map[%s]')
GetContinueBlockCont = vop('GetContinueBlockCont', 1, 'solver.continue_block_cont_map[%s]')

PopCatchCont = vop('PopCatchCont', 1, "solver.pop_catch_cont(%s)")
FindCatchCont = vop('FindCatchCont', 1, "solver.find_catch_cont(%s)")
#PushCatchCont = vop2('PushCatchCont', 2, "solver.push_catch_cont(%s, %s)")
PushCatchCont = vop2('PushCatchCont', 2, "solver.catch_cont_map.setdefault(%s, []).append(%s)")
#PushUnwindCont = vop2("PushUnwindCont", 1, "solver.push_unwind_cont(%s)")
PushUnwindCont = vop2("PushUnwindCont", 1, "solver.unwind_cont_stack.append(%s)")
#top_unwind_cont = vop('top_unwind_cont', 0, "solver.top_unwind_cont()")()
#top_unwind_cont = vop('top_unwind_cont', 0, "solver.unwind_cont_stack[-1]")()
#pop_unwind_cont = vop('pop_unwind_cont', 0, "solver.pop_unwind_cont()")()
pop_unwind_cont = vop('pop_unwind_cont', 0, "solver.unwind_cont_stack.pop()")()
unwind_cont_stack_length = vop('unwind_cont_stack_length', 0, "len(solver.unwind_cont_stack)")()
#Unwind = vop('Unwind', 1, "solver.unwind(%s)")
Unwind = vop2('Unwind', 1, "while len(solver.unwind_cont_stack)>%s:\n"
                          "    solver.unwind_cont_stack[-1](None)")

SetContent = vop2('SetContent', 2, '%s[0] = %s')
Content = vop('Content', 1, '%s[0]')
#MakeCell = vop('MakeCell', 1, '[%s]') # assign to upper level variable is possible.
MakeCell = vop('MakeCell', 0, '[None]') #assign always generate new local variable, like python.

SetFailCont = vop2('SetFailCont', 1, 'solver.fail_cont = %s')
FailCont = vop('failcont', 0, 'solver.fail_cont')  
failcont = FailCont()

SetCutCont = vop2('SetCutCont', 1, 'solver.cut_cont = %s')
CutCont = vop('CutCont', 0, 'solver.cut_cont')
cut_cont = CutCont()

SetCutOrCont = vop2('SetCutOrCont', 1, 'solver.cut_or_cont = %s')
CutOrCont = vop('CutOrCont', 0, 'solver.cut_or_cont')
cut_or_cont = CutOrCont()


IsLogicVar = vop('IsLogicVar', 1, 'isinstance(%s, LogicVar)')
Deref = vop('Deref', 1, 'deref(%s, solver.bindings)')
SetBinding = vop2('SetBinding', 2, 'solver.bindings[%s] = %s')
DelBinding = vop2('DelBinding', 1, 'del solver.bindings[%s]')
GetValue = vop('GetValue', 1, 'getvalue(%s, solver.bindings')

SetParseState = vop2('SetParseState', 1, 'solver.parse_state = %s')
ParseState = vop('parse_state', 0, 'solver.parse_state')
parse_state = ParseState()

new_logicvar = vop('new_logicvar', 1, 'solver.new_logicvar(%s)')
Prin = vop2('Prin', 1, 'print %s,')

def binary_to_code(self, coder):
  return '(%s) %s (%s)'%(self.args[0].to_code(coder), 
                         self.symbol, 
                         self.args[1].to_code(coder))

def binary(name, symbol):
  class Binary(VirtualOperation): 
    def __repr__(self):
      return '(%s%s%s)'%(repr(self.args[0]), self.__class__.symbol, repr(self.args[1]))
  Binary.__name__ = Binary.name = name
  Binary.arity = 2
  Binary.symbol = symbol
  Binary.code_format = binary_to_code
  return Binary    
    
Lt = binary('Lt', '<')
Le = binary('Le', '<=')
Eq = binary('Eq', '==')
Ne = binary('Ne', '!=')
Ge = binary('Ge', '>=')
Gt = binary('Gt', '>')

def append_failcont(compiler, exp):
  v, fc = LocalVar('v'), LocalVar('fc1')
  v1 =  compiler.new_var(v)
  fc1 = compiler.new_var(fc)
  return Begin((
    Assign(fc1, failcont),
    SetFailCont(
      clamda(v1, 
                SetFailCont(fc1),
                exp,
                fc1(FALSE)))
    ))