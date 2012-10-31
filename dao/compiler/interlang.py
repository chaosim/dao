from dao.base import classeq

class Optimizer: 
  def __init__(self): 
    pass
  
class CodeGenerator: 
  def __init__(self, indent_space='  ', language='python'):
    self.language = language
    self.indent_space = indent_space
    self.var_index_map = {'function':0}
    self.var_index = 0
    
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
    
  def to_code(self, exp):
    try: 
      exp_to_code = exp.to_code
    except: 
      return repr(exp)
    return exp_to_code(self)
   
  def to_code_list(self, items):
    return [self.to_code(x) for x in items]  

class Element:
  def free_variables(self):
    return set()
  
class Clamda(Element):
  def __init__(self, v, fc, exps):
    self.params, self.body = (v, fc), exps
    self.name = None
    
  def __call__(self, v, fc):
    return Apply(self, v, fc)
  
  def to_code(self, coder):
    if self.name is None: 
      self.name = coder.newvar()
      head = "def %s(%s):\n" % (self.name, ', '.join(coder.to_code_list(self.params)))
      return  head + coder.indent('\n'.join(coder.to_code_list(self.body)))
    else: return self.name
    
  def not_use_params(self):
    for p in self.params:
      if p in self.body.free_variables(): 
        return False
    return True
  
  def free_variables(self):
    return self.body.variables()-set(self.params)
    
  def __eq__(x, y):
    return classeq(x, y) and x.params==y.params and x.body==y.body
  
  def __hash__(self): return hash(id(self))
  
  def __repr__(self):
    return 'clamda(%r, %r, %s)'%(self.params[0], self.params[1], ', '.join([repr(x) for x in self.body]))

def clamda(v, fc, *exps): 
  return Clamda(v, fc, exps)

class Var(Element):
  def __init__(self, name):
    self.name = name
        
  def to_code(self, coder):
    return  self.name
  
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  def free_variables(self):
    return set([self])
  
  #def __repr__(self):
    #return 'Var(%r)'%self.name
  
  def __repr__(self):
    return self.name
    
class Apply(Element):
  def __init__(self, caller, *args):
    self.caller, self.args = caller, args
                
  def to_code(self, coder):
    if isinstance(self.caller, Clamda) and self.caller.name is None:
      return self.caller.to_code(coder) + '\n' + \
             self.caller.to_code(coder)+'(%s)'%', '.join([coder.to_code(x) for x in self.args])
    else: 
      return self.caller.name + '(%s)'%', '.join([coder.to_code(x) for x in self.args])
  
  def free_variables(self):
    result = set()
    result |= self.caller.free_variables()
    for arg in self.args:
      result |= arg.free_variables()
    return result
  
  def optimize(self):
    if isinstance(self.caller, Clamda):
      if self.caller.not_use_params():
        return self.caller.body
      subst = {}
      if self.caller.free_occur_once(self.caller.params):
        assigns = tuple(assign(p, a) for p, a in zip(self.caller.params, self.args))
        body = self.caller.body.subst(self.caller.params, self.args)
        return assigns+body          
      return self
    
    return self
  
  def __eq__(x, y):
    return classeq(x, y) and x.caller==y.caller and x.args==y.args
  
  #def __repr__(self):
    #return 'Apply(%r, %s)'%(self.caller, ', '.join([repr(x) for x in self.args]))
  
  def __repr__(self):
    return '%r(%s)'%(self.caller, ', '.join([repr(x) for x in self.args]))
  
class Return(Element):
  def __init__(self, args):
    self.args = args
    
  def to_code(self, coder):
    return  'return %s' % ', '.join([coder.to_code(x) for x in self.args])
  
  def free_variables(self):
    result = set()
    for arg in self.args:
      result |= arg.free_variables()
    return result
  
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    return 'ret(%s)'%', '.join([repr(x) for x in self.args])
  
def ret(*args): return Return(args)

class Assign(Element):
  def __init__(self, var, exp):
    self.var, self.exp =  var, exp
    
  def to_code(self, coder):
    return  '%s = %s' % (coder.to_code(self.var), coder.to_code(self.exp))
  
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.exp==y.exp
  
  def __repr__(self):
    return 'il.assign(%r, %r)'%(self.var, self.exp)
  
def assign(var, exp): return Assign(var, exp)

class If(Element):
  def __init__(self, test, then, else_):
    self.test, self.then, self.else_ = test, then, else_
    
  def to_code(self, coder):
    return  'if %s: \n%s\nelse:\n%s' % (self.test.to_code(coder), coder.indent(coder.to_code(self.then)), 
                                 coder.indent(coder.to_code(self.else_)))
  
  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.then==y.then and x.else_==y.else_
  
  def __repr__(self):
    if self.else_ is not None:
      return 'il.if_(%r, %r, %r)'%(self.test, self.then, self.else_)
    else:
      return 'il.if_(%r, %r)'%(self.test, self.then)

def if_(test, then, else_=None): return If(test, then, else_)

class If_without_else(Element):
  def __init__(self, test, then):
    self.test, self.then = test, then
    
  def to_code(self, coder):
    return  'if %s: \n%s\n' % (self.test.to_code(coder), coder.indent(coder.to_code(self.then)))
  
  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.then==y.then
  
  def __repr__(self):
    if self.else_ is not None:
      return 'il.if_(%r, %r, %r)'%(self.test, self.then)
    else:
      return 'il.if_(%r, %r)'%(self.test, self.then)

def if_without_else(test, then): return If_without_else(test, then)

class Unify(Element):
  def __init__(self, left, right, cont, fcont):
    self.left, self.right, self.cont, self.fcont =  left, right, cont,fcont
    
  def to_code(self, coder):
    return  'unify(%s, %s, %s, %s)' % (self.left.to_code(coder), self.right.to_code(coder), 
                                       self.cont.to_code(coder), self.fcont.to_code(coder))
  
  def __eq__(x, y):
    return classeq(x, y) and x.left==y.left and x.right==y.right and x.cont==y.cont and x.fcont==y.fcont
  
  def __repr__(self):
    return 'il.unify(%r, %r, %r, %r)'%(self.left, self.right, self.cont, self.fcont)

def unify(left, right, cont, fcont): return Unify(left, right, cont, fcont)

class BinaryOperationApply(Element):
  def __init__(self, operator, args):
    self.operator, self.args = operator, args
  
    
  def to_code(self, coder):
    return '%s%s%s'%(coder.to_code(self.args[0]), 
                        self.operator.to_code(coder), 
                        coder.to_code(self.args[1]))
  
  def __eq__(x, y):
    return classeq(x, y) and x.operator==y.operator and x.args==y.args
    
  def __repr__(self):
    return '%r(%r)'%(self.operator, self.args)

class BinaryOperation(Element):
  def __init__(self, name, operator):
    self.name, self.operator = name,operator
  
  def to_code(self, coder):
    return  self.operator
  
  def __call__(self, args):
    return BinaryOperationApply(self, args)
  
  def __eq__(x, y):
      return classeq(x, y) and x.operator==y.operator
  
  def __repr__(self):
    return 'il.%s'%self.name

add = BinaryOperation('add', '+')

class Literal(Element):
  def __init__(self, value):
    self.value = value
    
  def to_code(self, coder):
    return  repr(self.value)
  
  def __call__(self, args):
    raise TypeError
  
  def __eq__(x, y):
    return (classeq(x, y) and x.value==y.other.value) or x.value==y or x==y.value
  
  def __repr__(self):
    return 'il.literal(%s)'%self.value
  
def literal(value): return Literal(value)

class LogicVar(Element):
  def __init__(self, name):
    self.name = name
  
  def to_code(self, coder):
    return  "LogicVar(self.name)"
  
  def __call__(self, args):
    raise TypeError
  
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  def __repr__(self):
    return 'il.logicvar(name)'%self.name
  
def logicvar(name): return LogicVar(name)

class Tuple(Element):
  def __init__(self, elements):
    self.elements = elements
  
  def to_code(self, coder):
    if len(self.element)!=1:
      return "(%s)"%', '.join([x.to_code(coder) for x in self.elements])
    else: 
      return '(%s,)'%self.elements[0].to_code(coder)
  
  def __eq__(x, y):
    return classeq(x, y) and x.elements==y.elements
  
  def __repr__(self):
    return 'il.tuple(%s)'%', '.join([repr(x) for x in self.elements])
  
def tuple(*elements): return Tuple(elements)
