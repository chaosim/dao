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

def no_side_effects(exp):
  def fun(self):
    return False
  exp.side_effects = fun
  return exp

#True == 1
#False==0
unknown = -1

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
  
  def next_exp(self):
    try:
      return self._next_exp
    except:
      return self.parent.next_exp().first_exp
  
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
  
  def optimize(self, data):
    return self
  
  def replace_assign(self, data):
    return self
  
  def tail_recursive_convert(self):
      return self
  
  def trampoline(self):
    return self
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def pythonize_exp(self, env, compiler):
    return (self,), False
  
  def code_size(self):
    return 1
  
  def to_code(self, coder):
    return repr(self.value) 
  
  def free_vars(self):
    return set()
  
  def bool(self):
    if self.value: 
      return True
    else: 
      return False
  
  def __eq__(x, y):
    return classeq(x, y) and x.value==y.value
  
  def __hash__(self): return hash(self.value)
  
  def __repr__(self):
    return '%s'%self.value

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

def macro_args(value):
  return MacroArgs(tuple(element(x) for x in value))

class MacroArgs(Element): 
  def __init__(self, value):
    self.value = value
    
  def assign_convert(self, env, compiler):
    return MacroArgs(tuple(x.assign_convert(env, compiler) for x in self.value))
  
  def find_assign_lefts(self):
    return set()
  
  def optimization_analisys(self, data):  
    for x in self.value:
      x.optimization_analisys(data)
      
  def optimize(self, data):
    return MacroArgs(optimize_args(self.value, data))
  
  def side_effects(self):
    return False
  
  def subst(self, bindings):
    return MacroArgs(tuple(x.subst(bindings) for x in self.value))
  
  def pythonize_exp(self, env, compiler):
    has_statement = False
    exps = []
    args = []
    for arg in self.value:
      exps1, has_statement1 = arg.pythonize_exp(env, compiler)
      has_statement = has_statement or has_statement1
      exps += exps1[:-1]
      args.append(exps1[-1])
    exps.append(MacroArgs(tuple(args)))
    return tuple(exps), has_statement
  
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
  
  def insert_return_statement(self):
    return begin(self, Return(self.var))
  
  def code_size(self):
    return code_size(self.exp)+2
    
  def side_effects(self):
    return True
    
  def subst(self, bindings):  
    return Assign(self.var, self.exp.subst(bindings))
        
  def optimize(self, data):
    exp = self.exp.optimize(data)
    if exp.side_effects():
      if self.var in data.assign_bindings:
        del data.assign_bindings[self.var]
      return Assign(self.var, exp)
    else:
      data.assign_bindings[self.var] = exp
      self.parent.remove(self)
  
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
    
  def optimize(self, data):
    if len(self.args)==1 and isinstance(self.args[0], Return):
      return self.__class__(*self.args[0].args)
    else:
      for arg in self.args: 
        if isinstance(arg, Return): 
          raise CompileError
      return self.__class__(*optimize_args(self.args, data))
  
  def pythonize_exp(self, env, compiler):
    exps, args, has_statement = pythonize_args(self.args, env, compiler)
    return exps+(self.__class__(*args),), True
    
  def to_code(self, coder):
    return  'return %s' % ', '.join([x.to_code(coder) for x in self.args])
  
  def insert_return_statement(self):
    return Return(*self.args)
  
  def replace_return_with_yield(self):
    return Begin((Yield(*self.args), Return()))
  
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    return 'il.Return(%s)'%', '.join([repr(x) for x in self.args])

class Yield(Return): 
  def to_code(self, coder):
    return  'yield %s' % ', '.join([x.to_code(coder) for x in self.args])

  def insert_return_statement(self):
    return self
  
  def __repr__(self):
    return 'il.Yield(%s)'%', '.join([repr(x) for x in self.args])

def if_(test, then, else_):
  return If(element(test), element(then), element(else_))
  
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
    
  def optimize(self, data):
    test = self.test.optimize(data)
    then = self.then.optimize(data)
    else_ = self.else_.optimize(data)
    if isinstance(then, If): # (if a (if a b c) d)
      if then.test==self.test:
        return If(self.test, then.then, self.else_).optimize(data)
    if isinstance(else_, If): # (if a b (if a c d))
      if else_.test==self.test:
        return If(self.test, self.then, else_.else_).optimize(data)
    test_bool = test.bool()
    if test_bool==True:
      return then.optimize(data)
    elif test_bool==False:
      return else_.optimize(data)
    return If(test, then, else_)

  def insert_return_statement(self):
    result = If(self.test, 
              self.then.insert_return_statement(), 
              self.else_.insert_return_statement())
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
  
  def insert_return_statement(self):
    return self
  
  def replace_return_with_yield(self):
    return self
  
  def __eq__(x, y):
    return classeq(x, y)
  
  def __repr__(self):
    return 'il.pseudo_else'

pseudo_else = PseudoElse()

class Try(Element):
  def __init__(self, test, body):
    self.test, self.body = test, body
    
  def assign_convert(self, env, compiler):
    return Try(self.test.assign_convert(env, compiler), 
                 self.body.assign_convert(env, compiler))

  def find_assign_lefts(self):
    return self.body.find_assign_lefts()
  
  def optimization_analisys(self, data):  
    self.test.optimization_analisys(data)
    self.body.optimization_analisys(data)
    
  def code_size(self):
    return 3 + self.test.code_size() + \
           self.body.code_size()
  
  def side_effects(self):
    return not self.test.side_effects() and\
           not self.body.side_effects()
    
  def subst(self, bindings):  
    return Try(self.test.subst(bindings),
              self.body.subst(bindings))
    
  def optimize(self, data):
    return Try(self.test.optimize(data), self.body.optimize(data))
  
  def insert_return_statement(self):
    result = Try(self.test, 
              self.body.insert_return_statement())
    result.is_statement = True
    return result
  
  def replace_return_with_yield(self):
    result = Try(self.test, 
              self.body.replace_return_with_yield())
    result.is_statement = True
    return result
  
  def pythonize_exp(self, env, compiler):
    test, has_statement1 = self.test.pythonize_exp(env, compiler)
    body, has_statement2 = self.body.pythonize_exp(env, compiler)
    result = Try(test[-1], begin(*body))
    return test[:-1]+(result,), True
    
  def to_code(self, coder):
    return 'try:\n%s\nexcept:\n%s\n' % (coder.indent(self.test.to_code(coder)), 
                                  coder.indent(self.body.to_code(coder)))
           
  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.body==y.body
  
  def __repr__(self):
    return 'il.Try(%r, %r)'%(self.test, self.body)

def while_(test, *exps):
  return While(element(test), begin(*[x for x in exps]))

class While(Element):
  def __init__(self, test, body):
    self.test, self.body = test, body
    
  def assign_convert(self, env, compiler):
    return While(self.test.assign_convert(env, compiler), 
                 self.body.assign_convert(env, compiler))

  def find_assign_lefts(self):
    return self.body.find_assign_lefts()
  
  def optimization_analisys(self, data):  
    self.test.optimization_analisys(data)
    self.body.optimization_analisys(data)
    
  def code_size(self):
    return 3 + self.test.code_size() + \
           self.body.code_size()
  
  def side_effects(self):
    return not self.test.side_effects() and\
           not self.body.side_effects()
    
  def subst(self, bindings):  
    return While(self.test.subst(bindings),
              self.body.subst(bindings))
    
  def optimize(self, data):
    return While(self.test.optimize(data), self.body.optimize(data))

  def insert_return_statement(self):
    result = While(self.test, 
              self.body.insert_return_statement())
    result.is_statement = True
    return result
  
  def replace_return_with_yield(self):
    result = While(self.test, 
              self.body.replace_return_with_yield())
    result.is_statement = True
    return result
  
  def pythonize_exp(self, env, compiler):
    test, has_statement1 = self.test.pythonize_exp(env, compiler)
    body, has_statement2 = self.body.pythonize_exp(env, compiler)
    result = While(test[-1], begin(*body))
    return test[:-1]+(result,), True
    
  def to_code(self, coder):
    return 'while %s:\n%s\n' % (self.test.to_code(coder), 
                                  coder.indent(self.body.to_code(coder)))
           
  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.body==y.body
  
  def __repr__(self):
    return 'il.While(%r, %r)'%(self.test, self.body)

def for_(var, range, *exps):
  return For(element(var), element(range), begin(*[x for x in exps]))

class For(Element):
  def __init__(self, var, range, body):
    self.var, self.range, self.body = var, range, body
    
  def assign_convert(self, env, compiler):
    return For(self.var.assign_convert(env, compiler), 
               self.range.assign_convert(env, compiler), 
               self.body.assign_convert(env, compiler))

  def find_assign_lefts(self):
    return self.body.find_assign_lefts()
  
  def optimization_analisys(self, data):  
    self.var.optimization_analisys(data)
    self.range.optimization_analisys(data)
    self.body.optimization_analisys(data)
    
  def code_size(self):
    return 3 + self.var.code_size() + self.range.code_size() + self.body.code_size()
  
  def side_effects(self):
    return not self.var.side_effects() and\
           not self.range.side_effects() and\
           not self.body.side_effects()
    
  def subst(self, bindings):  
    return For(self.var.subst(bindings),
               self.range.subst(bindings),
               self.body.subst(bindings))
    
  def optimize(self, data):
    return For(self.var, self.range.optimize(data), self.body.optimize(data))

  def insert_return_statement(self):
    return For(self.var, self.range, self.body.insert_return_statement())
  
  def replace_return_with_yield(self):
    return For(self.var, self.range, self.body.replace_return_with_yield())
  
  def pythonize_exp(self, env, compiler):
    var, has_statement1 = self.var.pythonize_exp(env, compiler)
    range, has_statement1 = self.range.pythonize_exp(env, compiler)
    body, has_statement2 = self.body.pythonize_exp(env, compiler)
    return (For(var[-1], range[-1], begin(*body)),), True
    
  def to_code(self, coder):
    return 'for %s in %s:\n%s\n' % (self.var.to_code(coder), 
                                    self.range.to_code(coder), 
                                  coder.indent(self.body.to_code(coder)))
           
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.range==y.range and x.body==y.body
  
  def __repr__(self):
    return 'il.For(%r, %r, %r)'%(self.var, self.range, self.body)

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
    count = len(statements)
    for i, exp in enumerate(statements):
      exp.parent = self
      if i<count-1: exp._next_exp = statements[i+1]
    self._first_exp = statements[0]
    
    
  def assign_convert(self, env, compiler):
    return Begin(tuple(x.assign_convert(env, compiler) for x in self.statements))
  
  def find_assign_lefts(self):
    result = set()
    for exp in self.statements:
      result |= exp.find_assign_lefts()
    return result
  
  def side_effects(self):
    return True
  
  def optimization_analisys(self, data):  
    for x in self.statements:
      x.optimization_analisys(data)  
  
  def subst(self, bindings):  
    return Begin(tuple(x.subst(bindings) for x in self.statements))
  
  def optimize(self, data):
    return begin(*optimize_args(self.statements, data))
  
  def remove(self, exp):
    for i, stmt in enumerate(self.statements):
      if stmt is exp: break
    else: return self
    return begin(*(self.statements[:i]+self.statements[i+1:]))
  
  def insert_return_statement(self):
    inserted = self.statements[-1].insert_return_statement()
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

type_map = {int:Integer, float: Float, str:String, unicode: String, 
            tuple: make_tuple, list:List, bool:Bool, type(None): Atom}

def optimize_args(args, data):
  result = []
  for arg in args:
    arg = arg.optimize(data)
    if arg is not None:
      result.append(arg)
  return tuple(result)
    
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
    
