import operator

from dao.base import classeq

from dao.compilebase import MAX_EXTEND_CODE_SIZE
from dao.compilebase import VariableNotBound, CompileTypeError
from element import Element, Begin, begin, Return, Yield
from element import Atom, Integer, Bool, MacroArgs, Tuple, Dict, List , element
from element import pythonize_args, FALSE, NONE, Symbol, no_side_effects, unknown, ConstAtom
from lamda import Apply, optimize_args, clamda, Lamda, MacroLamda, RulesDict
from lamda import Var, LocalVar, ConstLocalVar, SolverVar, LogicVar, Assign, ExpressionWithCode#, ValueAssignBox

class BinaryOperation(Element):
  def __init__(self, name, operator, operator_function, has_side_effects=True):
    self.name, self.operator = name, operator
    self.operator_function = operator_function
    self.has_side_effects = has_side_effects
  
  def analyse(self, compiler):  
    return self
  
  def subst(self, bindings):  
    return self

  def optimize(self, env, compiler):
    return self
  
  def code_size(self): 
    return 1
  
  def pythonize(self, env, compiler):
    return (self,), False
    
  def to_code(self, compiler):
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
IsNot = BinaryOperation('is_not', 'is not', operator.is_not, False)
And = BinaryOperation('and', 'and', operator.and_, False)
Or = BinaryOperation('or', 'or', operator.or_, False)

Lt = BinaryOperation('Lt', '<', operator.lt, False)
Le = BinaryOperation('Le', '<=', operator.le, False)
Eq = BinaryOperation('Eq', '==', operator.eq, False)
Ne = BinaryOperation('Ne', '!=', operator.ne, False)
Ge = BinaryOperation('Ge', '>=', operator.ge, False)
Gt = BinaryOperation('Gt', '>', operator.gt, False)

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

  def analyse(self, compiler):  
    compiler.called_count[self.caller] = compiler.called_count.setdefault(self.caller, 0)+1
    self.caller.analyse(compiler)
    for arg in self.args:
      arg.analyse(compiler)
        
  def code_size(self):
    return self.caller.code_size()+sum([x.code_size() for x in self.args])
              
  def side_effects(self):
    if isinstance(self.caller, Var): return True
    elif self.caller.has_side_effects: return True
    else: return False # after cps, all of value have been solved before called, 
                       # so have no side effects.
          
  def subst(self, bindings):  
    return self.__class__(self.caller.subst(bindings), 
                 tuple(arg.subst(bindings) for arg in self.args))
      
  def optimize(self, env, compiler): 
    caller = self.caller
    args = optimize_args(self.args, env, compiler)
    for arg in args:
      if not isinstance(arg, Atom):
        break
    else:
       return element(caller.operator_function(*tuple(arg.item for arg in args)))
    return self.__class__(caller, args)

  def insert_return_statement(self):
    return Return(self)
  
  def pythonize(self, env, compiler):
    exps, args, has_statement = pythonize_args(self.args, env, compiler)
    return exps+(self.__class__(self.caller, args),), has_statement
  
  def free_vars(self):
    result = set()
    for arg in self.args:
      result |= arg.free_vars()
    return result
  
  def to_code(self, compiler):
    if not self.caller.operator[0].isalpha():
      return '(%s)%s(%s)'%(self.args[0].to_code(compiler), 
                        self.caller.to_code(compiler), 
                        self.args[1].to_code(compiler))
    else:
      return '(%s) %s (%s)'%(self.args[0].to_code(compiler), 
                              self.caller.to_code(compiler), 
                              self.args[1].to_code(compiler))      
    
  def __repr__(self):
    return '%r(%s)'%(self.caller, ', '.join([repr(arg) for arg in self.args]))
    
class VirtualOperation(Element):
  def __init__(self, *args):
    if self.arity>=0:
      assert len(args)==self.arity, \
           '%s should have %s arguments.'%(
             self.__class__.__name__, self.__class__.arity)
    self.args = args
  
  def __call__(self, *args):
    return Apply(self, args)

  def find_assign_lefts(self):
    return set()
  
  def side_effects(self):
    return True

  def analyse(self, compiler):
    for arg in self.args:
      arg.analyse(compiler)
  
  def subst(self, bindings):  
    return self.__class__(*tuple(x.subst(bindings) for x in self.args))
  
  def code_size(self):
    return 1
  
  def optimize(self, env, compiler):
    if self.has_side_effects:
      return self.__class__(*optimize_args(self.args, env,compiler))
  
    args = optimize_args(self.args, env,compiler)
    free_vars = set()
    for arg in args:
      free_vars |= arg.free_vars()
    for var in free_vars:
      assign = None
      try:
        assign = env[var]
      except:
        pass
      if assign is not None:
        assign.dont_remove()
    result = self.__class__(*args)
    return result
  
  
  def bool(self):
    return unknown
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def pythonize(self, env, compiler):
    exps, args, has_statement = pythonize_args(self.args, env, compiler)
    try: self_is_statement = self.is_statement
    except: self_is_statement = False
    return exps+(self.__class__(*args),), self_is_statement or has_statement
  
  def to_code(self, compiler):
    if isinstance(self.__class__.code_format, str):
      if self.__class__.arity==0:
        return self.__class__.code_format
      elif self.__class__.arity!=-1:
        return self.__class__.code_format % tuple(x.to_code(compiler) for x in self.args)
      else:
        return self.__class__.code_format % (', '.join([x.to_code(compiler) for x in self.args]))
    else: 
      return self.__class__.code_format(self, compiler)
    
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
      
  def __hash__(self):
    return hash(self.__class__.__name__)
  
  def free_vars(self):
    result = set()
    for arg in self.args:
      result |= arg.free_vars()
    return result
  
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
    
  def analyse(self, compiler):
    self.item.analyse(compiler)
    
  def subst(self, bindings):  
    return Deref(self.item.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def free_vars(self):
    return self.item.free_vars()

  def optimize(self, env, compiler):
    item = self.item.optimize(env, compiler)
    if isinstance(item, Atom) or isinstance(item, Lamda):
      return item
    if isinstance(item, Deref):
      return item
    return Deref(item)
  
  def pythonize(self, env, compiler):
    exps, has_statement = self.item.pythonize(env, compiler)
    return exps[:-1]+(self.__class__(exps[-1]),), has_statement
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def to_code(self, compiler):
    return  'deref(%s, solver.bindings)'%self.item.to_code(compiler)
  
  def __repr__(self):
    return 'il.Deref(%s)'%self.item

class EvalExpressionWithCode(Element):
  def __init__(self, item):
    self.item = item
  
  def side_effects(self):
    return False
    
  def analyse(self, compiler):
    self.item.analyse(compiler)
    
  def subst(self, bindings):  
    return EvalExpressionWithCode(self.item.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def free_vars(self):
    return self.item.free_vars()

  def optimize(self, env, compiler):
    item = self.item.optimize(env, compiler)
    if isinstance(item, Var):
      return EvalExpressionWithCode(item)
    elif isinstance(item, ExpressionWithCode):
      return item.function.body.optimize(env, compiler)
    else:
      raise CompileTypeError(item)
  
  def pythonize(self, env, compiler):
    exps, has_statement = self.item.pythonize(env, compiler)
    return exps[:-1]+(self.__class__(exps[-1]),), has_statement
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def to_code(self, compiler):
    return  '(%s).function()'%self.item.to_code(compiler)
  
  def __repr__(self):
    return 'il.EvalExpressionWithCode(%s)'%self.item

class Len(Element):
  def __init__(self, item):
    self.item = item
  
  def side_effects(self):
    return False
    
  def analyse(self, compiler):
    self.item.analyse(compiler)
    
  def subst(self, bindings):  
    return Len(self.item.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def free_vars(self):
    return self.item.free_vars()

  def optimize(self, env, compiler):
    item = self.item.optimize(env, compiler)
    if isinstance(item, Atom) or isinstance(item, MacroArgs):
      return Integer(len(item.item))
    return Len(item)
  
  def pythonize(self, env, compiler):
    exps, has_statement = self.item.pythonize(env, compiler)
    return exps[:-1]+(Len(exps[-1]),), has_statement
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def to_code(self, compiler):
    return  'len(%s)'%self.item.to_code(compiler)
  
  def __repr__(self):
    return 'il.Len(%s)'%self.item

class In(Element):
  def __init__(self, item, container):
    self.item = item
    self.container = container
  
  def side_effects(self):
    return False
    
  def analyse(self, compiler):
    self.item.analyse(compiler)
    self.container.analyse(compiler)
    
  def subst(self, bindings):  
    return In(self.item.subst(bindings), self.container.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def free_vars(self):
    result = set()
    result |= self.item.free_vars()
    result |= self.container.free_vars()
    return result

  def optimize(self, env, compiler):
    item = self.item.optimize(env, compiler)
    container = self.container.optimize(env, compiler)
    if isinstance(item, Atom):
      if isinstance(container, Atom):
        return Bool(item.value in container.value)
      elif isinstance(container, RulesDict):
        return Bool(item.item in container.arity_body_map)
    return In(item, container)
  
  def pythonize(self, env, compiler):
    exps1, has_statement1 = self.item.pythonize(env, compiler)
    exps2, has_statement2 = self.container.pythonize(env, compiler)
    return exps1[:-1]+exps2[:-1]+(In(exps1[-1], exps2[-1]),), has_statement1 or has_statement2
  
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
  
  def to_code(self, compiler):
    return  '(%s) in (%s)'%(self.item.to_code(compiler), self.container.to_code(compiler))
  
  def __repr__(self):
    return 'il.In(%r, %r)'%(self.item, self.container)

class GetItem(Element):
  def __init__(self, container, index):
    self.container = container
    self.index = index
  
  def side_effects(self):
    return False
    
  def analyse(self, compiler):
    self.index.analyse(compiler)
    self.container.analyse(compiler)
    
  def subst(self, bindings):  
    return GetItem(self.container.subst(bindings), self.index.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def free_vars(self):
    result = set()
    result |= self.index.free_vars()
    result |= self.container.free_vars()
    return result
  
  def optimize(self, env, compiler):
    index = self.index.optimize(env, compiler)
    container = self.container.optimize(env, compiler)
    if isinstance(index, Atom):
      if isinstance(container, Atom):
        return element(container.item[index.item])
      elif isinstance(container, RulesDict):
        return element(container.arity_body_map[index.item])
        #try:
          #return element(container.arity_body_map[index.item])
        #except: 
          #return GetItem(container, index)
      elif isinstance(container, MacroArgs):
        return container.item[index.item]
    return GetItem(container, index)
  
  def pythonize(self, env, compiler):
    container_exps, has_statement1 = self.container.pythonize(env, compiler)
    index_exps, has_statement2 = self.index.pythonize(env, compiler)
    return container_exps[:-1]+index_exps[:-1]+(GetItem(container_exps[-1], index_exps[-1]),), has_statement1 or has_statement2
  
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
  
  def to_code(self, compiler):
    return  '(%s)[%s]'%(self.container.to_code(compiler), self.index.to_code(compiler))
  
  def __repr__(self):
    return 'il.GetItem(%r, %r)'%(self.container, self.index)

class ListAppend(Element):
  is_statement = False
  
  def __init__(self, container, value):
    self.container = container
    self.value = value
  
  def side_effects(self):
    return True
    
  def analyse(self, compiler):
    self.value.analyse(compiler)
    self.container.analyse(compiler)
    
  def subst(self, bindings):  
    return ListAppend(self.container.subst(bindings), self.value.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def free_vars(self):
    result = set()
    result |= self.value.free_vars()
    result |= self.container.free_vars()
    return result
  
  def optimize(self, env, compiler):
    value = self.value.optimize(env, compiler)
    return ListAppend(self.container, value)
  
  def pythonize(self, env, compiler):
    container_exps, has_statement1 = self.container.pythonize(env, compiler)
    value_exps, has_statement2 = self.value.pythonize(env, compiler)
    return container_exps[:-1]+value_exps[:-1]+(ListAppend(container_exps[-1], value_exps[-1]),), has_statement1 or has_statement2
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def bool(self):
    return False
  
  def to_code(self, compiler):
    return  '%s.append(%s)'%(self.container.to_code(compiler), self.value.to_code(compiler))
  
  def __repr__(self):
    return 'il.ListAppend(%r, %r)'%(self.container, self.value)

catch_cont_map = SolverVar('catch_cont_map')
    
class PushCatchCont(Element):
  is_statement = False
  
  def __init__(self, tag, cont):
    self.tag = tag
    self.cont = cont
  
  def side_effects(self):
    return True
    
  def analyse(self, compiler):
    self.tag.analyse(compiler)
    self.cont.analyse(compiler)
    
  def subst(self, bindings):  
    return PushCatchCont(self.tag.subst(bindings), self.cont.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def free_vars(self):
    result = set([catch_cont_map])
    result |= self.tag.free_vars()
    result |= self.cont.free_vars()
    return result
  
  def optimize(self, env, compiler):
    tag = self.tag.optimize(env, compiler)
    cont = self.cont.optimize(env, compiler)
    #if isinstance(tag, ConstAtom) and env[catch_cont_map] is not None:
      #env[catch_cont_map].right_value().item.setdefault(tag, []).append(cont)
      #return NONE
    if env[catch_cont_map] is not None:
      result = Begin((Assign(catch_cont_map, env[catch_cont_map]), 
                    PushCatchCont(tag, cont)))
      env[catch_cont_map] = None
      return result
    return PushCatchCont(tag, cont)
  
  def pythonize(self, env, compiler):
    tag_exps, has_statement1 = self.tag.pythonize(env, compiler)
    cont_exps, has_statement2 = self.cont.pythonize(env, compiler)
    return tag_exps[:-1]+cont_exps[:-1]+(PushCatchCont(tag_exps[-1], cont_exps[-1]), ), True
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def bool(self):
    return False
  
  def to_code(self, compiler):
    return "solver.catch_cont_map.setdefault(%s, []).append(%s)" % (self.tag, self.cont)
  
  def __repr__(self):
    return 'il.PushCatchCont(%r, %r)'%(self.tag, self.cont)

class SetBinding(Element):
  is_statement = True
  
  def __init__(self, var, value):
    self.var = var
    self.value = value
  
  def side_effects(self):
    return True
    
  def analyse(self, compiler):
    self.var.analyse(compiler)
    self.value.analyse(compiler)
    
  def subst(self, bindings):  
    return SetBinding(self.var.subst(bindings), self.value.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def free_vars(self):
    return self.value.free_vars()
  
  def optimize(self, env, compiler):
    value = self.value.optimize(env, compiler)
    return SetBinding(self.var, value)
  
  def pythonize(self, env, compiler):
    var = self.var.item if isinstance(self.var, Deref) else self.var
    var_exps, has_statement1 = (var,), False
    value_exps, has_statement2 = self.value.pythonize(env, compiler)
    return var_exps[:-1]+value_exps[:-1]+(SetBinding(var_exps[-1], value_exps[-1]), ), True
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def bool(self):
    return False
  
  def to_code(self, compiler):
    return "solver.bindings[%s] = %s" % (self.var.to_code(compiler), self.value.to_code(compiler))
  
  def __repr__(self):
    return 'il.SetBinding(%r, %r)'%(self.var, self.value)

class FindCatchCont(Element):
  is_statement = False
  
  def __init__(self, tag):
    self.tag = tag
  
  def side_effects(self):
    return True
  
  def __call__(self, value):
    return Apply(self, (value,))
    
  def analyse(self, compiler):
    self.tag.analyse(compiler)
    
  def subst(self, bindings):  
    return FindCatchCont(self.tag.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def free_vars(self):
    result = set([catch_cont_map])
    result |= self.tag.free_vars()
    return result
  
  def optimize(self, env, compiler):
    tag = self.tag.optimize(env, compiler)
    if isinstance(tag, Atom) and env[catch_cont_map] is not None:
      try:
        cont_stack = env[catch_cont_map].right_value().item[tag]
        cont = cont_stack.pop()
        if not cont_stack:
          del env[catch_cont_map].right_value().item[tag]
        return cont
      except:
        return RaiseExcept(Symbol('DaoUncaughtError'))
    if env[catch_cont_map] is not None:
      result = Begin((Assign(catch_cont_map, env[catch_cont_map]), 
                    FindCatchCont(tag)))
      env[catch_cont_map] = None
      return result
    return FindCatchCont(tag)
  
  def pythonize(self, env, compiler):
    tag_exps, has_statement1 = self.tag.pythonize(env, compiler)
    return tag_exps[:-1]+(FindCatchCont(tag_exps[-1]), ), True
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def bool(self):
    return False
  
  def to_code(self, compiler):
    return "solver.find_catch_cont(%s)" % self.tag
  
  def __repr__(self):
    return 'il.FindCatchCont(%r)'%(self.tag)

def AddAssign(var, value):
  return Assign(var, BinaryOperationApply(add, (var, value)))

class IsMacroFunction(Element):
  def __init__(self, item):
    self.item = item
  
  def side_effects(self):
    return False
    
  def analyse(self, compiler):
    self.item.analyse(compiler)
    
  def subst(self, bindings):  
    return IsMacroFunction(self.item.subst(bindings))
  
  def code_size(self):
    return 1  
  
  def optimize(self, env, compiler):
    return IsMacroFunction(self.item.optimize(env, compiler))
  
  def pythonize(self, env, compiler):
    exps, has_statement = self.item.pythonize(env, compiler)
    return exps[:-1]+(self.__class__(exps[-1]),), has_statement
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def to_code(self, compiler):
    return  'isinstance(%s, MacroFunction)'%self.item.to_code(compiler)
  
  def bool(self):
    if isinstance(self.item, Lamda): 
      return False
    elif isinstance(self.item, MacroLamda):
      return True
    else: 
      return unknown
  
  def __repr__(self):
    return 'il.IsMacroFunction(%s)'%self.item

def vop(name, arity, code_format, has_side_effects):
  class Vop(VirtualOperation): pass
  Vop.name = Vop.__name__  = name
  Vop.arity = arity
  Vop.code_format = code_format
  Vop.is_statement = False
  vop.has_side_effects = has_side_effects
  return Vop

class VirtualOperation2(VirtualOperation):
  def insert_return_statement(self):
    return Begin((self, Return()))
  
  def replace_return_with_yield(self):
    return self
    
def vop2(name, arity, code_format, has_side_effects):
  class Vop(VirtualOperation2): pass
  Vop.__name__ = Vop.name  = name
  Vop.arity = arity
  Vop.code_format = code_format
  Vop.is_statement = True
  vop.has_side_effects = has_side_effects
  return Vop

class LogicOperation(VirtualOperation): pass
class BinaryLogicOperation(VirtualOperation): pass
class UnaryLogicOperation(VirtualOperation): pass

def Call_to_code(self, compiler):
  return '%s(%s)'%(self.args[0].to_code(compiler), ', '.join([x.to_code(compiler) for x in self.args[1:]]))  
Call = vop('Call', -1, Call_to_code, True)

Attr = vop('Attr', 2, '%s.%s', False)

def AttrCall_to_code(self, compiler):
  return '%s(%s)'%(self.args[0].to_code(compiler), ', '.join([x.to_code(compiler) for x in self.args[1:]]))
AttrCall = vop('AttrCall', -1, AttrCall_to_code, True)

SetItem = vop2('SetItem', 3, '(%s)[%s] = %s', True)
#def SetItem(item, key, value): return Assign(GetItem(item, key), value)
  
Slice2 = vop('Slice2', 2, '%s:%s', False)

Not = vop('Not', 1, "not %s", False)

Isinstance = vop('Isinstance', 2, "isinstance(%s, %s)", False)

empty_list = List([])

empty_dict = Dict({})

RaiseTypeError = vop2('RaiseTypeError', 1, 'raise %s', True)

RaiseException = vop2('RaiseException', 1, 'raise %s', True)

def QuoteItem_to_code(self, compiler):
  return '%s'%repr(self.args[0])
QuoteItem = vop('QuoteItem', 1, QuoteItem_to_code, False)

UnquoteSplice = vop('UnquoteSplice', 1, "UnquoteSplice(%s)", False)

MakeTuple = vop('MakeTuple', 1, 'tuple(%s)', False)

Cle = vop('Cle', 3, '(%s) <= (%s) <= (%s)', False)

Cge = vop('Cge', 3, '(%s) >= (%s) >= (%s)', False)

failcont = SolverVar('fail_cont')

def SetFailCont(cont): return Assign(failcont, cont)

def append_failcont(compiler, *exps):
  v =  compiler.new_var(ConstLocalVar('v'))
  fc = compiler.new_var(ConstLocalVar('fc'))
  return Begin((
    Assign(fc, failcont),
    SetFailCont(
      clamda(v, 
                SetFailCont(fc),
                begin(*exps),
                fc(FALSE)))
    ))

cut_cont = SolverVar('cut_cont')

def SetCutCont(cont): return Assign(cut_cont, cont)

cut_or_cont = SolverVar('cut_or_cont')

def SetCutOrCont(cont): return Assign(cut_or_cont, cont)

IsLogicVar = vop('IsLogicVar', 1, 'isinstance(%s, LogicVar)', False)


DelBinding = vop2('DelBinding', 1, 'del solver.bindings[%s]', True)

GetValue = vop('GetValue', 1, 'getvalue(%s, {}, solver.bindings)', False)

parse_state = SolverVar('parse_state')
def SetParseState(state): return Assign(parse_state, state)

Optargs = vop('Optargs', 1, '*%s', False)

Continue = vop('Continue', 0, "continue\n", False)
continue_ = Continue()

Prin = vop2('Prin', 1, "print %s,", True)
PrintLn = vop2('PrintLn', 1, "print %s", True)


def Format_to_code(self, compiler):
  return '%s%%(%s)'%(self.args[0].to_code(compiler), 
                     ', '.join([x.to_code(compiler) for x in self.args[1:]]))
Format = vop('Format', -1,Format_to_code, False)

def Concat_to_code(self, compiler):
  return '%s'%''.join([arg.to_code(compiler) for arg in self.args])
Concat = vop('Concat', -1, Concat_to_code, False)


def Format_to_code(self, compiler):
  return 'file(%s, %s)'%(self.args[0].to_code(compiler), 
                     ', '.join([x.to_code(compiler) for x in self.args[1:]]))
OpenFile = vop('OpenFile', -1, Format_to_code, True)
CloseFile = vop('CloseFile', 1, "%s.close()", True)
ReadFile = vop('ReadFile', 1, '%s.read()', True)
Readline = vop('ReadLine', 1, '%s.readline()', True)
Readlines = vop('Readlines', 1, '%s.readlines()', True)
WriteFile = vop('WriteFile', 2, '%s.write(%s)', True)
