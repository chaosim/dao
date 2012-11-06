'''generate code'''

from dao.compiler.compile import AlphaConvertEnvironment, CompileTypeError
from dao.compiler import interlang as il

def collocate(defs, exp):
  if defs:
    return il.StatementList(defs + (exp,))
  else: 
    return exp

def is_statement(exp):
  try: return exp.is_statement
  except:
    if isinstance(exp, il.Element):
      return False
    if isinstance(exp, list) or isinstance(exp, tuple) or\
      ( isinstance(exp, int) or isinstance(exp, float)
        or isinstance(exp, str) or isinstance(exp, unicode)):
      return False
  raise CompileTypeError(exp)
  
def to_code(exp):
  exp = pythonize(exp, AlphaConvertEnvironment())
  coder = CodeGenerator()
  return coder.to_code(exp)

def pythonize_list(exps, env):
  defs = ()
  exps2 = ()    
  for x in exps:
    exp = pythonize(x, env)
    if isinstance(exp, il.Function):
      defs += (exp,)
      exps2 += (exp.name,)
    else:
      exps2 += (exp,)
  return defs, exps2

def pythonize(exp, env):
  
  if isinstance(exp, il.Lamda):
    body_exps = ()
    body_is_statement = False
    for x in exp.body:
      x = pythonize(x, env)
      if is_statement(x):
        body_is_statement = True
      body_exps += (x,)
    if not body_is_statement:
      return il.Lamda(exp.params, *body_exps)
    else:
      return il.Function(env.new_var(il.Var('function')), exp.params, *body_exps)
    
  elif isinstance(exp, il.Var): return exp
    
  elif isinstance(exp, il.LogicVar): return exp
    
  elif  isinstance(exp, il.Apply):
    caller = pythonize(exp.caller, env)
    defs = ()
    if isinstance(caller, il.Function):
      defs += (caller,)
      caller = caller.name
    defs1, args = pythonize_list(exp.args, env)
    defs += defs1
    return collocate(defs, exp.__class__(caller,args))
  
  elif  isinstance(exp, il.BinaryOperation):
    return exp
  
  elif  isinstance(exp, il.Return):
    defs, args = pythonize_list(exp.args, env)
    return collocate(defs, il.Return(*args))
  
  elif  isinstance(exp, il.Assign): # var = value, exp.exp should be a single var, 
                                    # which is the continuation param which ref to the value
    return exp
  
  elif  isinstance(exp, il.If):
    defs, (test, then, else_) = pythonize_list((exp.test, exp.then, exp.else_), env)
    return collocate(defs, il.If(test, then, else_))
  
  elif  isinstance(exp, il.If2):
    defs, (test, then) = pythonize_list((exp.test, exp.then), env)
    return collocate(defs, il.If2(test, then))
  
  elif isinstance(exp, il.Unify):
    defs, (left, right, cont, fcont) = pythonize_list((exp.left, exp.right, exp.cont, exp.fcont), env)
    return collocate(defs, il.Unify(left, right, cont, fcont))
  
  elif isinstance(exp, list):
    return exp
  
  elif isinstance(exp, tuple):
    return exp
  
  elif isinstance(exp, int) or isinstance(exp, float) or \
       isinstance(exp, str) or isinstance(exp, unicode):
    return exp
  
  elif exp is None:
    return None
  
  else: raise CompileTypeError(exp)
  
class CodeGenerator: 
  def __init__(self, indent_space='  ', language='python'):
    self.language = language
    self.indent_space = indent_space
    self.var_index_map = {'function':0}
    self.var_index = 0
    self.lambda_stack = []
    
  def indent(self, code, level=1):
    lines = code.split('\n')
    lines = tuple(self.indent_space*level + line for line in lines)
    return '\n'.join(lines)
  
  def newvar(self, kind='function'):
    if kind=='function':
      self.var_index_map[kind] += 1
      return 'function'+repr(self.var_index_map[kind])
    self.var_index += 1
    return 'x'+repr(self.var_index)
    
  # if the definition for same method of different class can be put in one place,
  # then the if_elif can be avoided, meanwhile the code is more understandable.
  
  def to_code(self, exp):
    
    if isinstance(exp, il.Function):
      head = "def %s(%s):\n" % (exp.name, ', '.join(self.to_code_list(exp.params)))
      self.lambda_stack.append(exp)
      result =  head + self.indent('\n'.join(self.to_code_list(exp.body)))
      self.lambda_stack.pop()
      return result
      
    elif isinstance(exp, il.Lamda):
      head = "lambda %s: " % ', '.join(self.to_code_list(exp.params))
      self.lambda_stack.append(exp)
      result = head + '(%s)'%', '.join(self.to_code_list(exp.body))
      self.lambda_stack.pop()
      return result
        
    elif isinstance(exp, il.BinaryOperation): # MUST be BEFORE subclass.
      return exp.operator
      
    elif isinstance(exp, il.BinaryOperationApply):
      return '%s%s%s'%(self.to_code(exp.args[0]), 
                          self.to_code(exp.caller), 
                          self.to_code(exp.args[1]))
    
    elif  isinstance(exp, il.Apply): # MUST be AFTER subclass.
      if isinstance(exp.caller, il.Lamda):
        return "(%s)"%self.to_code(exp.caller) + '(%s)'%', '.join([self.to_code(x) for x in exp.args])
      else:
        return self.to_code(exp.caller) + '(%s)'%', '.join([self.to_code(x) for x in exp.args])        

    elif  isinstance(exp, il.StatementList):
      return  '\n'.join([self.to_code(x) for x in exp.statements])
      
    elif  isinstance(exp, il.Return):
      if self.lambda_stack and isinstance(self.lambda_stack[-1], il.Function):
        return  'return %s' % ', '.join([self.to_code(x) for x in exp.args])
      else:
        return  ', '.join([self.to_code(x) for x in exp.args])
    
    elif  isinstance(exp, il.Assign):
      return  '%s = %s' % (self.to_code(exp.var), self.to_code(exp.exp))
    
    elif  isinstance(exp, il.If):
      if self.lambda_stack and isinstance(self.lambda_stack[-1], il.Function):
        return 'if %s: \n%s\nelse:\n%s' % (self.to_code(exp.test), self.indent(self.to_code(exp.then)), 
                                         self.indent(self.to_code(exp.else_)))        
      else:
        return '%s if %s else %s' % (self.to_code(exp.then), self.to_code(exp.test), 
                                         self.to_code(exp.else_))        
    
    elif  isinstance(exp, il.If2):
      return 'if %s: \n%s\n' % (self.to_code(exp.test), self.indent(self.to_code(exp.then)))

    elif isinstance(exp, il.Unify):
      return 'unify(%s, %s, %s, %s)' % (self.to_code(exp.left), self.to_code(exp.right), 
                                       self.to_code(exp.cont), self.to_code(exp.fcont))
    
    elif isinstance(exp, il.Var):
      return exp.name
    
    elif isinstance(exp, il.LogicVar):
      return  "LogicVar('%s')"%exp.name
    
    elif isinstance(exp, list):
      return '[%s]'%', '.join(tuple(self.to_code(x) for x in exp))
    
    elif isinstance(exp, tuple):
      return '(%s)'%', '.join(tuple(self.to_code(x) for x in exp))
    
    elif isinstance(exp, int) or isinstance(exp, float) or\
         isinstance(exp, str) or isinstance(exp, unicode):
      return repr(exp)
    
    elif exp is None:
      return 'None'
    
    else:
      return repr(exp)
   
  def to_code_list(self, items, in_lambda=True):
    return [self.to_code(x) for x in items]  