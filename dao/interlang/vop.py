from dao.compilebase import alpha_convert, cps_convert, assign_convert, find_assign_lefts
from dao.compilebase import optimization_analisys, optimize_once
from dao.compilebase import side_effects, optimize, subst, code_size, MAX_EXTEND_CODE_SIZE
from dao.compilebase import insert_return_yield, pythonize, to_code, to_code_list
from dao.compilebase import VariableNotBound, CompileTypeError

from elements import Element

class BinaryOperation(Element):
  def __init__(self, name, operator, have_side_effects=True):
    self.name, self.operator = name, operator
    self.have_side_effects = have_side_effects
  
  def alpha_convert(self, env, compiler):
    return self
  
  def optimization_analisys(self, data):  
    return self
  
  def subst(self, bindings):  
    return self

  def optimize_once(self, data):
    return self, False
    
  def pythonize(self, env, compiler):
    return self
    
  def to_code(self, coder):
    return self.operator
      
  def __call__(self, args):
    return BinaryOperationApply(self, args)
  
  def __eq__(x, y):
      return classeq(x, y) and x.operator==y.operator
    
  def __hash__(self): return hash(self.operator)
  
  def __repr__(self):
    return 'il.%s'%self.name

add = BinaryOperation('add', '+', False)

class BinaryOperationApply:
  is_statement = False
  
  def __init__(self, caller, args):
    self.caller, self.args = caller, args

  def alpha_convert(self, env, compiler):
    return self.__class__(alpha_convert(self.caller, env, compiler), 
                 tuple(alpha_convert(arg, env, compiler) for arg in self.args))
  
  def to_code(self, coder):
    return '%s%s%s'%(to_code(coder, self.args[0]), 
                        to_code(coder, self.caller), 
                        to_code(coder, self.args[1]))
    
  def assign_convert(self, env, compiler):
    return self.__class__(assign_convert(self.caller, env, compiler), 
                 tuple(assign_convert(arg, env, compiler) for arg in self.args))    
    
  def optimization_analisys(self, data):  
    data.called_count[self.caller] = data.called_count.setdefault(self.caller, 0)+1
    optimization_analisys(self.caller, data)
    for arg in self.args:
      optimization_analisys(arg, data)
        
  def code_size(self):
    return code_size(self.caller)+sum([code_size(x) for x in self.args])
              
  def side_effects(self):
    if isinstance(self.caller, Var): return True
    elif self.caller.have_side_effects: return True
    else: return False # after cps, all of value have been solved before called, 
                       # so have no side effects.
          
  def subst(self, bindings):  
    return self.__class__(subst(self.caller, bindings), 
                 tuple(subst(arg, bindings) for arg in self.args))
      
  def optimize_once(self, data):    
    changed = False
    caller, changed1 = optimize_once(self.caller, data)
    args, changed2 = optimize_once(self.args, data)
    return self.__class__(caller, args), changed1 or changed2

  def insert_return_yield(self, klass):
    return klass(self)
  
  def pythonize(self, env, compiler):
    return self
  
  def __repr__(self):
    return '%r(%r)'%(self.caller, self.args)

class VirtualOperation(Element):
  def __call__(self, *args):
    return Apply(self, args)

  def alpha_convert(self, env, compiler):
    return self
  
  def assign_convert(self, env, compiler):
    return self
  
  def find_assign_lefts(self):
    return set()
  
  def optimization_analisys(self, data):  
    return self
  
  def subst(self, bindings):  
    return self

  def optimize_once(self, data):
    return self, False
  
  def pythonize(self, env, compiler): 
    return self
  
  def to_code(self, coder):
    if isinstance(self.__class__.code_format, str):
      if self.__class__.arity==0:
        return self.__class__.code_format
      elif self.__class__.arity!=-1:
        return self.__class__.code_format % tuple(to_code(coder, x) for x in self.args)
      else:
        return self.__class__.code_format % (', '.join([to_code(coder, x) for x in self.args]))
    else: 
      return self.__class__.code_format(self, self.args, coder)
      
  def __hash__(self):
    return hash(self.__class__.__name__)
  
def vop(name, arity, code_format):
  class Vop(VirtualOperation): pass
  Vop.__name__ = name
  Vop.arity = arity
  Vop.code_format = code_format
  return Vop

class GetItem(Element):
  arity = 2
  
  def __repr__(self):
    return '%r[%r]'%(self.args)
  
Not = vop('Not', 1, "not %s")
def AssignFromList_to_code(self, args, coder):
  return "%s = %s" % (', '.join([to_code(x, coder) for x in args[:-1]]), to_code(args[-1], coder))
AssignFromList = vop('AssignFromList', -1, AssignFromList_to_code)
Isinstance = vop('Isinstance', 2, "isinstance(%s, %s)")
EmptyList = vop('empty_list', 0, '[]')
empty_list = EmptyList()
ListAppend = vop('ListAppend', 2, '%s = %s')
Len = vop('Len', 1, 'len(%s)')
RaiseTypeError = vop('RaiseTypeError', 1, 'raise %s')
SetContents = vop('SetContents', 2, '%s = %s')
MakeCell = vop('MakeCell', 1, '%s = %s')

SetFailCont = vop('SetFailCont', 1, 'solver.fail_cont = %s')
FailCont = vop('failcont', 0, 'solver.fail_cont')  
failcont = FailCont()

def AppendFailCont_code_format(self, args, coder):
  fc = coder.newvar('old_fail_cont')
  new_fail_cont = coder.newvar('new_fail_cont')
  result = "%s = solver.fail_cont\n" % fc
  result += "def %s(v):\n"%new_fail_cont
  result += coder.indent_space+"solver.fail_cont = %s\n"%fc
  for stmt in args:
    result += coder.indent_space+to_code(coder, stmt)+'\n'
  result += 'solver.fail_cont = %s' % new_fail_cont
  return result
AppendFailCont = vop('AppendFailCont', -1, AppendFailCont_code_format) 
'''il.Assign(fc, get_failcont)
  SetFailCont(
    Clambda(v, 
      SetFailCont(fc),
      statements
  ))'''  
SetCutOrCont = vop('SetCutOrCont', 1, 'solver.cut_or_cont = %s')
CutOrCont = vop('CutOrCont', 0, 'solver.cut_or_cont')
cut_or_cont = CutOrCont()

Deref = vop('Deref', 1, 'deref(%s, solver.bindings)')
SetBinding = vop('SetBinding', 2, 'solver.bindings[%s] = %s')
DelBinding = vop('DelBinding', 1, 'del solver.bindings[%s]')
GetValue = vop('GetValue', 1, 'getvalue(%s, solver.bindings')

SetParseState = vop('SetParseState', 1, 'solver.parse_state = %s')
ParseState = vop('parse_state', 0, 'solver.parse_state')
parse_state = ParseState()

def binary(name, symbol):
  class Binary(Element): 
    def __repr__(self):
      return '(%s%s%s)'%(repr(self.args[0]), self.__class__.symbol, repr(self.args[1]))
  Binary.__name__ = name
  Binary.arity = 2
  Binary.symbol = symbol
  return Binary    
    
Lt = binary('Lt', '<')
Le = binary('Le', '<=')
Eq = binary('Eq', '==')
Ne = binary('Ne', '!=')
Ge = binary('Ge', '>=')
Gt = binary('Gt', '>')

