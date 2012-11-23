from dao.base import classeq
from dao import base

from dao.compilebase import CompileTypeError #VariableNotBound, CompileTypeError

def element(exp):
  if isinstance(exp, base.Element):
    return exp
  else:
    try: 
      return type_map[type(exp)](exp)
    except: 
      raise CompileTypeError(exp)
  
class Element(base.Element):
  have_side_effects = True
  is_statement = False
  
  def tail_recursive_convert(self):
    return self
  
  def find_assign_lefts(self):
    return set()
  
  def trampoline(self):
    return self
    
  def to_code_if_in_lambda_body(self, coder):
    return self.to_code(coder)
  
  def replace_return_with_yield(self):
    return self
  
  def __eq__(x, y):
    return classeq(x, y)
  
  def __repr__(self):
    return self.__class__.__name__
   
class Atom(Element):
  def __init__(self, value):
    self.value = value
    
  def alpha_convert(self, env, compiler):
    return self
    
  def cps_convert(self, compiler, cont):
    return cont(self)
    
  def quasiquote(self, compiler, cont):
    return cont(self)
   
  def vars(self):
    return set()
  
  def interlang(self):
    return self
  
  def assign_convert(self, env, compiler):
    return self
  
  def find_assign_lefts(self):
    return set()
  
  def optimization_analisys(self, data):  
    return
  
  def side_effects(self):
    return False
  
  def subst(self, bindings):
    return self
  
  def optimize_once(self, data):
    return self, False
  
  def tail_recursive_convert(self):
    return self
  
  def trampoline(self):
    return self
  
  def insert_return_statement(self, klass):
    return klass(self)
  
  def replace_return_with_yield(self):
    return klass(self)
  
  def pythonize_exp(self, env, compiler):
    return (self,), False
  
  def code_size(self):
    return 1
  
  def to_code(self, coder):
    return repr(self.value) 
  
  def __eq__(x, y):
    return classeq(x, y) and x.value==y.value
  
  def __hash__(self): return hash(self.value)
  
  def __repr__(self):
    return 'il.%s(%s)'%(self.__class__.__name__, self.value)

class Expression(Atom):  
  def cps_convert(self, compiler, cont):
    return self.value.cps_convert(compiler, cont)
  
  def to_code(self, coder):
    #return 'Expression(%s)'%self.value.to_code(coder)
    return '%s'%self.value.to_code(coder)
  
class Integer(Atom): 
  def __eq__(x, y):
    return Atom.__eq__(x, y) or (isinstance(y, int) and x.value==y)
  
class Float(Atom): 
  def __eq__(x, y):
    return Atom.__eq__(x, y) or (isinstance(y, float) and x.value==y)
  
class String(Atom): 
  def __eq__(x, y):
    return Atom.__eq__(x, y) or (isinstance(y, str) and x.value==y)
  
class List(Atom): 
  def __eq__(x, y):
    return Atom.__eq__(x, y) or (isinstance(y, list) and x.value==y)
  
class Bool(Atom): 
  def __eq__(x, y):
    return Atom.__eq__(x, y) or (isinstance(y, bool) and x.value==y)
  

class Symbol(Atom): 
  def to_code(self, coder):
    return self.value
  def __eq__(x, y):
    return classeq(x, y) and x.value==y.value
    

class Klass(Atom):
  def to_code(self, coder):
    return self.value
  
  def __repr__(self):
    return 'il.Klass(%s)'%(self.value)
  
TRUE = Bool(True)
FALSE = Bool(False)
NONE = Atom(None)

def make_tuple(value):
  return Tuple(*tuple(element(x) for x in value))

class Tuple(Atom): 
  def __init__(self, *value):
    self.value = value
    
  def assign_convert(self, env, compiler):
    return Tuple(*tuple(x.assign_convert(env, compiler) for x in self.value))
  
  def find_assign_lefts(self):
    return set()
  
  def optimization_analisys(self, data):  
    for x in self.value:
      x.optimization_analisys(data)
  
  def side_effects(self):
    return False
  
  def subst(self, bindings):
    return Tuple(*tuple(x.subst(bindings) for x in self.value))
  
  def code_size(self):
    return sum([x.code_size() for x in self.value])
  
  def to_code(self, coder):
    if len(self.value)!=1:
      return '(%s)'% ', '.join([x.to_code(coder) for x in self.value])
    else: 
      return '(%s, )'%self.value[0].to_code(coder)
  
  def __eq__(x, y):
    return classeq(x, y) and x.value==y.value
  
  def __repr__(self):
    return 'il.%s(%s)'%(self.__class__.__name__, self.value)

class Assign(Element):
  is_statement = True
  
  def __init__(self, var, exp):
    self.var, self.exp =  var, exp
  
  def assign_convert(self, env, compiler):
    return SetContent(env[self.var], self.exp.assign_convert(env, compiler))
  
  def find_assign_lefts(self):
    return set([self.var])
  
  def optimization_analisys(self, data):  
    self.exp.optimization_analisys(data)
  
  def insert_return_statement(self, klass):
    return begin(self, klass(None))
  
  def code_size(self):
    return code_size(self.exp)+2
    
  def side_effects(self):
    return True
    
  def subst(self, bindings):  
    return Assign(self.var, self.exp.subst(bindings))
        
  def optimize_once(self, data):
    exp, changed = self.exp.optimize_once(data)
    return Assign(self.var, exp), changed
    
  def pythonize_exp(self, env, compiler):
    exps, has_statement = self.exp.pythonize_exp(env, compiler)
    if exps[-1].is_statement:
      return exps+(Assign(self.var, NONE),), True
    else:
      return exps[:-1]+(Assign(self.var, exps[-1]),), True
    
  def to_code(self, coder):
    return  '%s = %s' % (self.var.to_code(coder), self.exp.to_code(coder))
    
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.exp==y.exp
  
  def __repr__(self):
    return 'il.Assign(%r, %r)'%(self.var, self.exp)
  
class Return(Element):
  is_statement = True
  
  def __init__(self, *args):
    self.args = args
  
  def assign_convert(self, env, compiler):
    return self.__class__(*tuple(arg.assign_convert(env, compiler) for arg in self.args))
    
  def optimization_analisys(self, data):  
    for arg in self.args:
      arg.optimization_analisys(data)
        
  def code_size(self):
    return sum([code_size(x) for x in self.args])

  def side_effects(self):
    return False
        
  def subst(self, bindings):  
    return self.__class__(*tuple(arg.subst(bindings) for arg in self.args))
    
  def optimize_once(self, data):
    if len(self.args)==1 and isinstance(self.args[0], Return):
      args = self.args[0].args
    else:
      for arg in self.args: 
        if isinstance(arg, Return): 
          raise CompileError
      args = self.args
    changed = False
    result = []
    for arg in args:
      arg, arg_changed = arg.optimize_once(data)
      result.append(arg)
      changed = changed or arg_changed
    return self.__class__(*result), changed
  
  def pythonize_exp(self, env, compiler):
    exps, args, has_statement = pythonize_args(self.args, env, compiler)
    return exps+(self.__class__(*args),), True
    
  def to_code(self, coder):
    return  'return %s' % ', '.join([x.to_code(coder) for x in self.args])
  
  def insert_return_statement(self, klass):
    return klass(*self.args)
  
  def replace_return_with_yield(self):
    return Yield(*self.args)
  
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    return 'il.Return(%s)'%', '.join([repr(x) for x in self.args])

class Yield(Return): 
  def to_code(self, coder):
    return  'yield %s' % ', '.join([x.to_code(coder) for x in self.args])

  def insert_return_statement(self, klass):
    return self
  
  def __repr__(self):
    return 'il.Yield(%s)'%', '.join([repr(x) for x in self.args])

class If(Element):
  def __init__(self, test, then, else_):
    self.test, self.then, self.else_ = test, then, else_
    if else_==pseudo_else: self.is_statement = True
    
  def assign_convert(self, env, compiler):
    return If(self.test.assign_convert(env, compiler), 
                 self.then.assign_convert(env, compiler), 
                 self.else_.assign_convert(env, compiler))

  def find_assign_lefts(self):
    return self.then.find_assign_lefts() | self.else_.find_assign_lefts()
  
  def optimization_analisys(self, data):  
    self.test.optimization_analisys(data)
    self.then.optimization_analisys(data)
    self.else_.optimization_analisys(data)
    
  def code_size(self):
    return 3 + self.test.code_size() + \
           self.then.code_size() + \
           self.else_.code_size()
  
  def side_effects(self):
    return not self.test.side_effects() and\
           not self.then.side_effects() and\
           not self.else_.side_effects()
    
  def subst(self, bindings):  
    return If(self.test.subst(bindings),
              self.then.subst(bindings), 
              self.else_.subst(bindings))
    
  def optimize_once(self, data):
    changed = False
    result = self
    if isinstance(result.then, If): # (if a (if a b c) d)
      if result.then.test==result.test:
        result = If(result.test, result.then.then, result.else_)
        changed = True
    if isinstance(result.else_, If): # (if a b (if a c d))
      if result.else_.test==result.test:
        result = If(result.test, result.then, result.else_.else_)
        changed = True
    test, test_changed = result.test.optimize_once(data)
    then, then_changed = result.then.optimize_once(data)
    else_, else__changed = result.else_.optimize_once(data)
    result = If(test, then, else_)
    #if isinstance(result.test, Let):
      #result = Let(result.bindings, If(Begin(let.body), 
                                             #result.then, result.else_))
    return result, changed or test_changed or then_changed or else__changed

  def insert_return_statement(self, klass):
    result = If(self.test, 
              self.then.insert_return_statement(klass), 
              self.else_.insert_return_statement(klass))
    result.is_statement = True
    return result
  
  def replace_return_with_yield(self):
    result = If(self.test, 
              self.then.replace_return_with_yield(), 
              self.else_.replace_return_with_yield())
    result.is_statement = True
    return result
  
  def pythonize_exp(self, env, compiler):
    test, has_statement1 = self.test.pythonize_exp(env, compiler)
    then, has_statement2 = self.then.pythonize_exp(env, compiler)
    else_, has_statement3 = self.else_.pythonize_exp(env, compiler)
    if_ = If(test[-1], begin(*then), begin(*else_))
    if_.is_statement = if_.is_statement or has_statement2 or has_statement3
    return test[:-1]+(if_,), has_statement1 or if_.is_statement
    
  def to_code(self, coder):
    if self.is_statement:
      result = 'if %s: \n%s\n' % (self.test.to_code(coder), 
                                  coder.indent(self.then.to_code(coder)))
      if self.else_!=pseudo_else:
        result += 'else:\n%s\n'% coder.indent(self.else_.to_code(coder)) 
      return result
    else:
      return '%s if %s else %s' % (self.then.to_code(coder), 
                                   self.test.to_code(coder), 
                                   self.else_.to_code(coder))        
  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.then==y.then and x.else_==y.else_
  
  def __repr__(self):
    if self.else_!=pseudo_else:
      return 'il.If(%r, %r, %r)'%(self.test, self.then, self.else_)
    else:
      return 'il.If(%r, %r)'%(self.test, self.then)

def if2(test, then):
  return If(test, then, pseudo_else)

class PseudoElse(Atom):
  def __init__(self):
    return
  
  def code_size(self):
    return 0
  
  def insert_return_statement(self, klass):
    return self
  
  def replace_return_with_yield(self):
    return self
  
  def __eq__(x, y):
    return classeq(x, y)
  
  def __repr__(self):
    return 'il.pseudo_else'

pseudo_else = PseudoElse()

def begin(*exps):
  assert isinstance(exps, tuple)
  if len(exps)==0: return exps
  elif len(exps)==1: 
    return exps[0]
  else:
    result = []
    for e in exps:
      if isinstance(e, Begin):
        result += e.statements
      else:
        result.append(e)
    return Begin(tuple(result))

class Begin(Element):
  is_statement = True
  
  def __init__(self, statements):
    self.statements = statements
    
  def assign_convert(self, env, compiler):
    return Begin(tuple(x.assign_convert(env, compiler) for x in self.statements))
  
  def find_assign_lefts(self):
    result = set()
    for exp in self.statements:
      result |= exp.find_assign_lefts()
    return result
  
  def optimization_analisys(self, data):  
    for x in self.statements:
      x.optimization_analisys(data)  
  
  def subst(self, bindings):  
    return Begin(tuple(x.subst(bindings) for x in self.statements))
  
  def optimize_once(self, data):
    changed = False
    result = []
    for x in self.statements:
      x, x_changed = x.optimize_once(data)
      result.append(x)
      changed = changed or x_changed
    return begin(*tuple(result)), changed
        
  def insert_return_statement(self, klass):
    inserted = self.statements[-1].insert_return_statement(klass)
    return Begin(self.statements[:-1]+(inserted,))
  
  def replace_return_with_yield(self):
    return Begin(tuple(exp.replace_return_with_yield() for exp in self.statements))
  
  def pythonize_exp(self, env, compiler):
    return pythonize_exps(self.statements, env, compiler)
  
  def to_code(self, coder):
    return  '\n'.join([x.to_code(coder) for x in self.statements])
      
  def to_code_if_in_lambda_body(self, coder):
    return  '(%s)'%', '.join([x.to_code(coder) for x in self.statements])

  def __eq__(x, y):
      return classeq(x, y) and x.statements==y.statements
  
  def __repr__(self):
    return 'il.begin(%s)'%', '.join([repr(x) for x in self.statements])

type_map = {int:Integer, float: Float, str:String, unicode: String, tuple: make_tuple, list:List, bool:Bool}

def optimize_once_args(args, data):
  changed = False
  result = []
  for arg in args:
    arg, changed1 = arg.optimize_once(data)
    changed = changed or changed1
    result.append(arg)
  return tuple(result), changed
    
def pythonize_exps(exps, env, compiler):
  result = ()
  has_any_statement = False
  for exp in exps:
    exps2, any_statement = exp.pythonize_exp(env, compiler)
    has_any_statement = has_any_statement or any_statement
    result += exps2
  return result, has_any_statement

def pythonize_args(args, env, compiler):
  # used in Apply, Return, Yield, VirtualOpteration
  result = []
  exps = ()
  has_statement = False
  for arg in args:
    exps2, has_statement1 = arg.pythonize_exp(env, compiler)
    has_statement = has_statement or has_statement1
    if exps2[-1].is_statement:
      result.append(NONE)
      exps += exps2
    else:
      result.append(exps2[-1])
      exps += exps2[:-1]      
  return exps, result, has_statement
    
