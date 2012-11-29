from dao.base import classeq

from dao.compilebase import MAX_EXTEND_CODE_SIZE, to_code_list
from dao.compilebase import VariableNotBound, CompileTypeError
from element import Element, Begin, Assign, begin, Return, Yield#, element
from lamda import Apply, optimize_args, Var, LocalVar, clamda, LogicVar, Lamda, MacroLamda, RulesDict, SolverVar
from element import pythonize_args, FALSE, NONE, Symbol, no_side_effects, unknown
from element import Atom, element, Integer, Bool

import operator

#from element import Integer

class BinaryOperation(Element):
  def __init__(self, name, operator, operator_function, have_side_effects=True):
    self.name, self.operator = name, operator
    self.operator_function = operator_function
    self.have_side_effects = have_side_effects
  
  def alpha_convert(self, env, compiler):
    return self
  
  def assign_convert(self, env, compiler):
    return self
  
  def optimization_analisys(self, data):  
    return self
  
  def subst(self, bindings):  
    return self

  def optimize(self, data):
    return self
  
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

add = BinaryOperation('add', '+', operator.add, False)
sub = BinaryOperation('sub', '-', operator.sub, False)
mul = BinaryOperation('mul', '*', operator.mul, False)
div = BinaryOperation('div', '/', operator.div, False)
IsNot = BinaryOperation('is_not', operator.is_not, 'is not', False)
And = BinaryOperation('and', 'and', operator.and_, False)
Or = BinaryOperation('or', 'or', operator.or_, False)

def and_(*exps):
  if len(exps)==2:
    return And(*exps)
  else:
    return And(exps[0], and_(*exps[1:]))
               
def or_(*exps):
  if len(exps)==2:
    return Or(*exps)
  else:
    return Or(exps[0], or_(*exps[1:]))
               
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
      
  def optimize(self, data): 
    caller = self.caller
    args = optimize_args(self.args, data)
    for arg in args:
      if not isinstance(arg, Atom):
        break
    else:
       return element(caller.operator_function(*tuple(arg.value for arg in args)))
    return self.__class__(caller, args)

  def insert_return_statement(self):
    return Return(self)
  
  def pythonize_exp(self, env, compiler):
    exps, args, has_statement = pythonize_args(self.args, env, compiler)
    return exps+(self.__class__(self.caller, args),), has_statement
  
  def to_code(self, coder):
    if not self.caller.operator[0].isalpha():
      return '(%s)%s(%s)'%(self.args[0].to_code(coder), 
                        self.caller.to_code(coder), 
                        self.args[1].to_code(coder))
    else:
      return '(%s) %s (%s)'%(self.args[0].to_code(coder), 
                              self.caller.to_code(coder), 
                              self.args[1].to_code(coder))      
    
  def __repr__(self):
    return '%r(%s)'%(self.caller, ', '.join([repr(arg) for arg in self.args]))
  
  #def __repr__(self):
    #return '%r%s%r'%(self.args[0], self.caller.operator, self.args[1])
  
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
  
  def optimize(self, data):
    return self.__class__(*optimize_args(self.args, data))
  
  def bool(self):
    return unknown
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
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
   
class Deref(Element):
  def __init__(self, item):
    self.item = item
  
  def side_effects(self):
    return False
    
  def optimization_analisys(self, data):
    self.item.optimization_analisys(data)
    
  def subst(self, bindings):  
    return Deref(self.item.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def optimize(self, data):
    item = self.item.optimize(data)
    if isinstance(item, Atom) or isinstance(item, Lamda):
      return item
    return Deref(item)
  
  def pythonize_exp(self, env, compiler):
    exps, has_statement = self.item.pythonize_exp(env, compiler)
    return exps[:-1]+(self.__class__(exps[-1]),), has_statement
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def to_code(self, coder):
    return  'deref(%s, solver.bindings)'%self.item.to_code(coder)
  
  def __repr__(self):
    return 'il.Deref(%s)'%self.item

class Len(Element):
  def __init__(self, item):
    self.item = item
  
  def side_effects(self):
    return False
    
  def optimization_analisys(self, data):
    self.item.optimization_analisys(data)
    
  def subst(self, bindings):  
    return Len(self.item.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def optimize(self, data):
    item = self.item.optimize(data)
    if isinstance(item, Atom):
      return Integer(len(item.value))
    return Len(item)
  
  def pythonize_exp(self, env, compiler):
    exps, has_statement = self.item.pythonize_exp(env, compiler)
    return exps[:-1]+(Len(exps[-1]),), has_statement
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def to_code(self, coder):
    return  'len(%s)'%self.item.to_code(coder)
  
  def __repr__(self):
    return 'il.Len(%s)'%self.item

class In(Element):
  def __init__(self, item, container):
    self.item = item
    self.container = container
  
  def side_effects(self):
    return False
    
  def optimization_analisys(self, data):
    self.item.optimization_analisys(data)
    self.container.optimization_analisys(data)
    
  def subst(self, bindings):  
    return In(self.item.subst(bindings), self.container.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def optimize(self, data):
    item = self.item.optimize(data)
    container = self.container.optimize(data)
    if isinstance(item, Atom):
      if isinstance(container, Atom):
        return Bool(item.value in container.value)
      elif isinstance(container, RulesDict):
        return Bool(item.value in container.arity_body_map)
    return In(item, container)
  
  def pythonize_exp(self, env, compiler):
    exps, has_statement = self.item.pythonize_exp(env, compiler)
    return exps[:-1]+(In(exps[-1]),), has_statement
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def bool(self):
    if isinstance(self.item, Atom):
      if isinstance(self.container, Atom):
        return self.item.value in self.container.value
      elif isinstance(self.container, RulesDict):
        return self.item.value, self.container.arity_body_map
    return unknown
  
  def to_code(self, coder):
    return  '(%s) in (%s)'%(self.item.to_code(coder), self.container.to_code(coder))
  
  def __repr__(self):
    return 'il.In(%r, %r)'%(self.item, self.container)

class GetItem(Element):
  def __init__(self, container, index):
    self.container = container
    self.index = index
  
  def side_effects(self):
    return False
    
  def optimization_analisys(self, data):
    self.index.optimization_analisys(data)
    self.container.optimization_analisys(data)
    
  def subst(self, bindings):  
    return GetItem(self.container.subst(bindings), self.index.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def optimize(self, data):
    index = self.index.optimize(data)
    container = self.container.optimize(data)
    if isinstance(index, Atom):
      if isinstance(container, Atom):
        return element(container.value[index.value])
      elif isinstance(container, RulesDict):
        return element(container.arity_body_map[index.value])
        #try:
          #return element(container.arity_body_map[index.value])
        #except: 
          #return GetItem(container, index)
    return GetItem(container, index)
  
  def pythonize_exp(self, env, compiler):
    container_exps, has_statement = self.container.pythonize_exp(env, compiler)
    index_exps, has_statement = self.index.pythonize_exp(env, compiler)
    return container_exps[:-1]+index_exps[:-1]+(GetItem(container_exps[-1], index_exps[-1]),), has_statement
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def bool(self):
    if isinstance(self.index, Atom):
      if isinstance(self.container, Atom):
        return Bool(bool(self.container.value[self.index.value]))
      elif isinstance(self.container, RulesDict):
        return Bool(bool(self.container.arity_body_map[self.index.value]))
    return unknown
  
  def to_code(self, coder):
    return  '(%s)[%s]'%(self.container.to_code(coder), self.index.to_code(coder))
  
  def __repr__(self):
    return 'il.GetItem(%r, %r)'%(self.container, self.index)

class IsMacroFunction(Element):
  def __init__(self, item):
    self.item = item
  
  def side_effects(self):
    return False
    
  def optimization_analisys(self, data):
    self.item.optimization_analisys(data)
    
  def subst(self, bindings):  
    return IsMacroFunction(self.item.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def optimize(self, data):
    return IsMacroFunction(self.item.optimize(data))
  
  def pythonize_exp(self, env, compiler):
    exps, has_statement = self.item.pythonize_exp(env, compiler)
    return exps[:-1]+(self.__class__(exps[-1]),), has_statement
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def to_code(self, coder):
    return  'isinstance(%s, MacroFunction)'%self.item.to_code(coder)
  
  def bool(self):
    if isinstance(self.item, Lamda): 
      return False
    elif isinstance(self.item, MacroLamda):
      return True
    else: 
      return unknown
  
  def __repr__(self):
    return 'il.IsMacroFunction(%s)'%self.item

def find_last_assigns(exp, var):
  return

def vop(name, arity, code_format):
  class Vop(VirtualOperation): pass
  Vop.name = Vop.__name__  = name
  Vop.arity = arity
  Vop.code_format = code_format
  Vop.is_statement = False
  return Vop

class VirtualOperation2(VirtualOperation):
  def insert_return_statement(self):
    return Begin((self, Return()))
  
  def replace_return_with_yield(self):
    return self
    
def vop2(name, arity, code_format):
  class Vop(VirtualOperation2): pass
  Vop.__name__ = Vop.name  = name
  Vop.arity = arity
  Vop.code_format = code_format
  Vop.is_statement = True
  return Vop

#solver = Var('solver')

class LogicOperation(VirtualOperation): pass
class BinaryLogicOperation(VirtualOperation): pass
class UnaryLogicOperation(VirtualOperation): pass

def Call_to_code(self, coder):
  return '%s(%s)'%(self.args[0].to_code(coder), ', '.join([x.to_code(coder) for x in self.args[1:]]))  
Call = vop('Call', -1, Call_to_code)

Attr = no_side_effects(vop('Attr', 2, '%s.%s'))

def AttrCall_to_code(self, coder):
  return '%s(%s)'%(self.args[0].to_code(coder), ', '.join([x.to_code(coder) for x in self.args[1:]]))
AttrCall = vop('AttrCall', -1, AttrCall_to_code)

#GetItem = vop('GetItem', 2, '(%s)[%s]')

SetItem = vop2('SetItem', 3, '(%s)[%s] = %s')
#def SetItem(item, key, value): return Assign(GetItem(item, key), value)
  
Slice2 = vop('Slice2', 2, '%s:%s')

Not = vop('Not', 1, "not %s")

def AssignFromList_to_code(self, coder):
  return "%s = %s" % (', '.join([x.to_code(coder) for x in self.args[:-1]]), 
                      self.args[-1].to_code(coder))
AssignFromList = vop2('AssignFromList', -1, AssignFromList_to_code)

AddAssign = vop2('AddAssign', 2, '%s += %s')

Isinstance = vop('Isinstance', 2, "isinstance(%s, %s)")

EmptyList = vop('empty_list', 0, '[]')
empty_list = EmptyList()

ListAppend = vop('ListAppend', 2, '%s.append(%s)')

#Len = vop('Len', 1, 'len(%s)')
#def Len(item): return Call(Symbol('len'), item)

RaiseTypeError = vop2('RaiseTypeError', 1, 'raise %s')

SetExitBlockContMap = vop2('SetExitBlockContMap', 2, 'solver.exit_block_cont_map[%s] = %s')
#def SetExitBlockContMap(key, value): return SetItem(SolverVar('exit_block_cont_map'), key, value)

SetContinueBlockContMap = vop2('SetContinueBlockContMap', 2, 'solver.continue_block_cont_map[%s] = %s')
#def SetContinueBlockContMap(key, value): return SetItem(SolverVar('continue_block_cont_map'), key, value)

GetExitBlockCont = vop('GetExitBlockCont', 1, 'solver.exit_block_cont_map[%s]')
#def GetExitBlockCont(key): return GetItem(SolverVar('exit_block_cont_map'), key)

GetContinueBlockCont = vop('GetContinueBlockCont', 1, 'solver.continue_block_cont_map[%s]')
#def GetContinueBlockCont(key): return GetItem(SolverVar('continue_block_cont_map'), key)

def QuoteItem_to_code(self, coder):
  return '%s'%repr(self.args[0])
QuoteItem = vop('QuoteItem', 1, QuoteItem_to_code)

UnquoteSplice = vop('UnquoteSplice', 1, "UnquoteSplice(%s)")

MakeTuple = vop('MakeTuple', 1, 'tuple(%s)')

Cle = vop('Cle', 3, '(%s) <= (%s) <= (%s)')

Cge = vop('Cge', 3, '(%s) >= (%s) >= (%s)')

PopCatchCont = vop('PopCatchCont', 1, "solver.pop_catch_cont(%s)")
#def PopCatchCont(tag): return Call(SolverVar('pop_catch_cont'), tag)

FindCatchCont = vop('FindCatchCont', 1, "solver.find_catch_cont(%s)")

PushCatchCont = vop2('PushCatchCont', 2, "solver.push_catch_cont(%s, %s)")
#PushCatchCont = vop2('PushCatchCont', 2, "solver.catch_cont_map.setdefault(%s, []).append(%s)")

PushUnwindCont = vop2("PushUnwindCont", 1, "solver.push_unwind_cont(%s)")
#PushUnwindCont = vop2("PushUnwindCont", 1, "solver.unwind_cont_stack.append(%s)")

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

#FailCont = vop('failcont', 0, 'solver.fail_cont')  
#failcont = FailCont()
failcont = SolverVar('fail_cont')

#SetFailCont = vop2('SetFailCont', 1, 'solver.fail_cont = %s')
def SetFailCont(cont): return Assign(failcont, cont)

#CutCont = vop('CutCont', 0, 'solver.cut_cont')
#cut_cont = CutCont()
cut_cont = SolverVar('cut_cont')

#SetCutCont = vop2('SetCutCont', 1, 'solver.cut_cont = %s')
def SetCutCont(cont): return Assign(cut_cont, cont)

#CutOrCont = vop('CutOrCont', 0, 'solver.cut_or_cont')
#cut_or_cont = CutOrCont()
cut_or_cont = SolverVar('cut_or_cont')

#SetCutOrCont = vop2('SetCutOrCont', 1, 'solver.cut_or_cont = %s')
def SetCutOrCont(cont): return Assign(cut_or_cont, cont)


IsLogicVar = vop('IsLogicVar', 1, 'isinstance(%s, LogicVar)')

SetBinding = vop2('SetBinding', 2, 'solver.bindings[%s] = %s')

DelBinding = vop2('DelBinding', 1, 'del solver.bindings[%s]')

GetValue = vop('GetValue', 1, 'getvalue(%s, solver.bindings')

#ParseState = vop('parse_state', 0, 'solver.parse_state')
#parse_state = ParseState()
parse_state = SolverVar('parse_state')

#SetParseState = vop2('SetParseState', 1, 'solver.parse_state = %s')
def SetParseState(state): return Assign(parse_state, state)

new_logicvar = vop('new_logicvar', 1, 'solver.new_logicvar(%s)')

Optargs = vop('Optargs', 1, '*%s')

Continue = vop('Continue', 0, "continue\n")
continue_ = Continue()

def Prin_to_code(self, coder):
  return 'print %s,'%', '.join([x.to_code(coder) for x in self.args])
Prin = vop2('Prin', -1, Prin_to_code)

def Print_to_code(self, coder):
  return 'print %s'%', '.join([x.to_code(coder) for x in self.args])
PrintLn = vop2('PrintLn', -1, Print_to_code)

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

def append_failcont(compiler, *exps):
  v, fc = LocalVar('v'), LocalVar('fc1')
  v1 =  compiler.new_var(v)
  fc1 = compiler.new_var(fc)
  return Begin((
    Assign(fc1, failcont),
    SetFailCont(
      clamda(v1, 
                SetFailCont(fc1),
                begin(*exps),
                fc1(FALSE)))
    ))


EvalExpressionWithCode = vop('EvalExpressionWithCode', 1, '(%s).function()')

