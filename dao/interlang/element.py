from dao.base import classeq
from dao import base

from dao.compilebase import CompileTypeError #VariableNotBound, CompileTypeError

#True == 1
#False==0
unknown = -1

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

def optimize_args(args, env, compiler):
  result = []
  for arg in args:
    arg = arg.optimize(env, compiler)
    if arg is not None:
      result.append(arg)
  return tuple(result)
    
def pythonize_args(args, env, compiler):
  # used in Apply, Return, Yield, VirtualOpteration
  result = []
  exps = ()
  has_statement = False
  for arg in args:
    exps2, has_statement1 = arg.pythonize(env, compiler)
    has_statement = has_statement or has_statement1
    if exps2[-1].is_statement:
      result.append(NONE)
      exps += exps2
    else:
      result.append(exps2[-1])
      exps += exps2[:-1]      
  return exps, result, has_statement
    
class Element(base.Element):
  has_side_effects = True
  is_statement = False
  
  def tail_recursive_convert(self):
    return self
  
  def find_assign_lefts(self):
    return set()
  
  def replace_return_with_yield(self):
    return self
  
  def __eq__(x, y):
    return classeq(x, y)
  
  def __repr__(self):
    return self.__class__.__name__
   
class Atom(Element):
  def __init__(self, item):
    self.item = item
    
  def find_assign_lefts(self):
    return set()
  
  def analyse(self, compiler):  
    return
  
  def side_effects(self):
    return False
  
  def subst(self, bindings):
    return self
  
  def optimize(self, env, compiler):
    return self
  
  def replace_assign(self, compiler):
    return self
  
  def tail_recursive_convert(self):
      return self
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def pythonize(self, env, compiler):
    return (self,), False
  
  def code_size(self):
    return 1
  
  def to_code(self, compiler):
    return repr(self.item) 
  
  def free_vars(self):
    return set()
  
  def bool(self):
    if self.item: 
      return True
    else: 
      return False
  
  def __eq__(x, y):
    return classeq(x, y) and x.item==y.item
  
  def __hash__(self): return hash(self.item)
  
  def __repr__(self):
    return '%s'%self.item

class ConstAtom(Atom):
  pass

class Integer(ConstAtom): 
  def __eq__(x, y):
    return ConstAtom.__eq__(x, y) or (isinstance(y, int) and x.item==y)
  
class Float(ConstAtom): 
  def __eq__(x, y):
    return ConstAtom.__eq__(x, y) or (isinstance(y, float) and x.item==y)
  
class String(ConstAtom): 
  def __eq__(x, y):
    return ConstAtom.__eq__(x, y) or (isinstance(y, str) and x.item==y)
  
class Bool(ConstAtom): 
  def __eq__(x, y):
    return ConstAtom.__eq__(x, y) or (isinstance(y, bool) and x.item==y)
  
class Symbol(ConstAtom): 
  def to_code(self, compiler):
    return self.item    

class Klass(ConstAtom):
  def to_code(self, compiler):
    return self.item
  
  def __repr__(self):
    return 'il.Klass(%s)'%(self.item)

class PyFunction(ConstAtom):  
  def to_code(self, compiler):
    return self.item.func_name
  
  def __repr__(self):
    return 'il.PyFunction(%s)'%(self.item)

TRUE = Bool(True)
FALSE = Bool(False)
NONE = Atom(None)

def make_tuple(item):
  return Tuple(*tuple(element(x) for x in item))

class Tuple(ConstAtom): 
  def __init__(self, *item):
    self.item = item
    
  def find_assign_lefts(self):
    return set()
  
  def analyse(self, compiler):  
    for x in self.item:
      x.analyse(compiler)
  
  def side_effects(self):
    return False
  
  def subst(self, bindings):
    return Tuple(*tuple(x.subst(bindings) for x in self.item))
  
  def code_size(self):
    return sum([x.code_size() for x in self.item])
  
  def optimize(self, env, compiler):
    return Tuple(*tuple(x.optimize(env, compiler) for x in self.item))
  
  def to_code(self, compiler):
    if len(self.item)!=1:
      return '(%s)'% ', '.join([x.to_code(compiler) for x in self.item])
    else: 
      return '(%s, )'%self.item[0].to_code(compiler)
  
  def __eq__(x, y):
    return classeq(x, y) and x.item==y.item
  
  def __repr__(self):
    return 'il.%s(%s)'%(self.__class__.__name__, self.item)

class MutableAtom(Atom):
  pass

class List(MutableAtom): 
  def __eq__(x, y):
    return (classeq(x, y) and x.item==y.item) or (
      (isinstance(y, list) and x.item==y))

  
class Dict(MutableAtom): 
  def __eq__(x, y):
    return Atom.__eq__(x, y) or (isinstance(y, dict) and x.item==y)
  
def macro_args(item):
  return MacroArgs(item)

class MacroArgs(Element): 
  def __init__(self, item):
    self.item = item
    
  def find_assign_lefts(self):
    return set()
  
  def analyse(self, compiler):  
    for x in self.item:
      x.analyse(compiler)
      
  def optimize(self, env, compiler):
    return MacroArgs(optimize_args(self.item, env, compiler))
  
  def side_effects(self):
    return False
  
  def free_vars(self):
    result = set()
    for x in self.item:
      result |= x.free_vars()
    return result
  
  def subst(self, bindings):
    return MacroArgs(tuple(x.subst(bindings) for x in self.item))
  
  def pythonize(self, env, compiler):
    has_statement = False
    exps = []
    args = []
    for arg in self.item:
      exps1, has_statement1 = arg.pythonize(env, compiler)
      has_statement = has_statement or has_statement1
      exps += exps1[:-1]
      args.append(exps1[-1])
    exps.append(MacroArgs(tuple(args)))
    return tuple(exps), has_statement
  
  def code_size(self):
    return sum([x.code_size() for x in self.item])
  
  def to_code(self, compiler):
    if len(self.item)!=1:
      return '(%s)'% ', '.join([x.to_code(compiler) for x in self.item])
    else: 
      return '(%s, )'%self.item[0].to_code(compiler)
  
  def __eq__(x, y):
    return classeq(x, y) and x.item==y.item
  
  def __repr__(self):
    return 'il.%s(%s)'%(self.__class__.__name__, self.item)

class Return(Element):
  is_statement = True
  
  def __init__(self, *args):
    self.args = args
  
  def analyse(self, compiler):  
    for arg in self.args:
      arg.analyse(compiler)
        
  def code_size(self):
    return sum([code_size(x) for x in self.args])

  def side_effects(self):
    return False
  
  def free_vars(self):
    result = set()
    for x in self.args:
      result |= x.free_vars()
    return result
  
  def subst(self, bindings):  
    return self.__class__(*tuple(arg.subst(bindings) for arg in self.args))
    
  def optimize(self, env, compiler):
    if len(self.args)==1 and isinstance(self.args[0], Return):
      return self.__class__(*self.args[0].args)
    else:
      for arg in self.args: 
        if isinstance(arg, Return): 
          raise CompileError
      return self.__class__(*optimize_args(self.args, env, compiler))
  
  def pythonize(self, env, compiler):
    if len(self.args)==1 and isinstance(self.args[0], Begin):
      return Begin(self.args[0].statements[:-1]+(Return(self.args[0].statements[-1]),)).pythonize(env, compiler)
    elif len(self.args)==1 and isinstance(self.args[0], If):
      return If(self.args[0].test, Return(self.args[0].then), Return(self.args[0].else_)).pythonize(env, compiler)
    exps, args, has_statement = pythonize_args(self.args, env, compiler)
    return exps+(self.__class__(*args),), True
    
  def to_code(self, compiler):
    return  'return %s' % ', '.join([x.to_code(compiler) for x in self.args])
  
  def insert_return_statement(self):
    return Return(*self.args)
  
  def replace_return_with_yield(self):
    return Begin((Yield(*self.args), Return()))
  
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    return 'il.Return(%s)'%', '.join([repr(x) for x in self.args])

class Yield(Return): 
  def to_code(self, compiler):
    return  'yield %s' % ', '.join([x.to_code(compiler) for x in self.args])

  def insert_return_statement(self):
    return self
  
  def __repr__(self):
    return 'il.Yield(%s)'%', '.join([repr(x) for x in self.args])

class Try(Element):
  def __init__(self, test, body):
    self.test, self.body = test, body
    
  def find_assign_lefts(self):
    return self.body.find_assign_lefts()
  
  def analyse(self, compiler):  
    self.test.analyse(compiler)
    self.body.analyse(compiler)
    
  def code_size(self):
    return 3 + self.test.code_size() + \
           self.body.code_size()
  
  def side_effects(self):
    return not self.test.side_effects() and\
           not self.body.side_effects()
    
  def subst(self, bindings):  
    return Try(self.test.subst(bindings),
              self.body.subst(bindings))
    
  def optimize(self, env, compiler):
    return Try(self.test.optimize(env, compiler), self.body.optimize(env, compiler))
  
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
  
  def pythonize(self, env, compiler):
    test, has_statement1 = self.test.pythonize(env, compiler)
    body, has_statement2 = self.body.pythonize(env, compiler)
    result = Try(test[-1], begin(*body))
    return test[:-1]+(result,), True
    
  def to_code(self, compiler):
    return 'try:\n%s\nexcept:\n%s\n' % (compiler.indent(self.test.to_code(compiler)), 
                                  compiler.indent(self.body.to_code(compiler)))
           
  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.body==y.body
  
  def __repr__(self):
    return 'il.Try(%r, %r)'%(self.test, self.body)

def begin(*exps):
  result = []
  length = len(exps)
  for i, e in enumerate(exps):
    if isinstance(e, Begin):
      result += e.statements
    elif e==():
      continue
    else:
      if e==NONE and i!=length-1:
        continue
      else:
        result.append(e)
  if len(result)==0: 
    return pass_statement
  elif len(result)==1:
    return result[0]
  else:
    return Begin(tuple(result))

class Begin(Element):
  is_statement = True
  
  def __init__(self, statements):
    self.statements = statements  
    
  def find_assign_lefts(self):
    result = set()
    for exp in self.statements:
      result |= exp.find_assign_lefts()
    return result
  
  def side_effects(self):
    return True
  
  def subst(self, bindings):  
    return Begin(tuple(x.subst(bindings) for x in self.statements))
  
  def free_vars(self):
    result = set()
    for exp in self.statements:
      result |= exp.free_vars()
    return result
  
  def code_size(self):
    return 1
  
  def analyse(self, compiler):  
    for x in self.statements:
      x.analyse(compiler)  
  
  def optimize(self, env, compiler):
    result = []
    for arg in self.statements:
      arg1 = arg.optimize(env, compiler)
      if arg1 is not None:
        result.append(arg1)
    if result:
      return begin(*([x for x in result[:-1] if not isinstance(x, Atom)]+[result[-1]]))
    else: return pass_statement
  
  def remove(self, exp):
    for i, stmt in enumerate(self.statements):
      if stmt is exp: break
    else: return self
    return begin(*(self.statements[:i]+self.statements[i+1:]))
        
  def insert_return_statement(self):
    inserted = self.statements[-1].insert_return_statement()
    return begin(*(self.statements[:-1]+(inserted,)))
  
  def replace_return_with_yield(self):
    return Begin(tuple(exp.replace_return_with_yield() for exp in self.statements))
  
  def pythonize(self, env, compiler):    
    result = ()
    has_any_statement = False
    for exp in self.statements:
      exps2, any_statement = exp.pythonize(env, compiler)
      has_any_statement = has_any_statement or any_statement
      result += exps2
    return result, has_any_statement or len(result)>1
  
  def to_code(self, compiler):
    return  '\n'.join([x.to_code(compiler) for x in self.statements])
      
  def __eq__(x, y):
      return classeq(x, y) and x.statements==y.statements
  
  def __repr__(self):
    return 'il.begin(%s)'%'\n '.join([repr(x) for x in self.statements])

class PassStatement(Element):
  is_statement = True
  
  def __init__(self):
    return
  
  def code_size(self):
    return 0
  
  def side_effects(self):
    return False
  
  def analyse(self, compiler):  
    return
  
  def optimize(self, env, compiler):
    return pass_statement
  
  def pythonize(self, env, compiler):
    return (self,), True
  
  def insert_return_statement(self):
    return self
  
  def replace_return_with_yield(self):
    return self
  
  def subst(self, bindings):
    return self
  
  def __eq__(x, y):
    return classeq(x, y)
  
  def to_code(self, compiler):
    return 'pass'
  
  def __repr__(self):
    return 'il.pass_statement'

pass_statement = PassStatement()

type_map = {int:Integer, float: Float, str:String, unicode: String, 
            tuple: make_tuple, list:List, dict:Dict, 
            bool:Bool, type(None): Atom}

