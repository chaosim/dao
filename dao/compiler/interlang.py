from dao.base import classeq

class Element:
  pass
   
class Lamda(Element):
  def __init__(self, params, *body):
    self.params, self.body = params, body
    
  def __call__(self, *args):
    return Apply(self, args)
  
  def cps_convert(self, compiler, cont, fcont):
    k = Var('k')
    return cont(Lamda(self.params+(k,), compiler.cps_convert_exps(self.body, k, fcont)), fcont)
  
  def __eq__(x, y):
    return classeq(x, y) and x.params==y.params and x.body==y.body
  
  def __hash__(self): return hash(id(self))
  
  def __repr__(self):
    return 'Lamda((%s), %s)'%(', '.join([repr(x) for x in self.params]),
                              ', '.join([repr(x) for x in self.body]))

class Function(Lamda):
  is_statement = True
  def __init__(self, name, params, *body):
    Lamda.__init__(self, params, *body)
    self.name = name
  
class Clamda(Lamda):
  def __init__(self, v, fc, *exps):
    self.params, self.body = (v, fc), exps
    self.name = None
    
  def __repr__(self):
    return 'Clamda(%r, %r, %s)'%(self.params[0], self.params[1], ', '.join([repr(x) for x in self.body]))

class Apply(Element):
  def __init__(self, caller, args):
    self.caller, self.args = caller, args
    
  def __eq__(x, y):
    return classeq(x, y) and x.caller==y.caller and x.args==y.args
  
  def __repr__(self):
    return '%r(%s)'%(self.caller, ', '.join([repr(x) for x in self.args]))
  
class Var(Element):
  def __init__(self, name):
    self.name = name
        
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  def __call__(self, *args):
    return Apply(self, args)
  
  def free_variables(self):
    return set([self])
  
  def __hash__(self): return hash(self.name)

  def __repr__(self):
    return self.name #enough in tests
    
class LogicVar(Element):
  def __init__(self, name):
    self.name = name
  
  def __call__(self, args):
    raise TypeError
  
  def cps_convert_unify(self, other, cont, fcont):
    return Unify(self,other, cont, fcont)
  
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  def __repr__(self):
    return self.name # enough in tests
  
class Return(Element):
  
  def __init__(self, *args):
    self.args = args
  
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    return 'Return(%s)'%', '.join([repr(x) for x in self.args])
  
class Assign(Element):
  is_statement = True
  def __init__(self, var, exp):
    self.var, self.exp =  var, exp
  
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.exp==y.exp
  
  def __repr__(self):
    return 'il.Assign(%r, %r)'%(self.var, self.exp)
  
class If(Element):
  def __init__(self, test, then, else_):
    self.test, self.then, self.else_ = test, then, else_
    
  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.then==y.then and x.else_==y.else_
  
  def __repr__(self):
    if self.else_ is not None:
      return 'il.If(%r, %r, %r)'%(self.test, self.then, self.else_)
    else:
      return 'il.If(%r, %r)'%(self.test, self.then)

class If2(Element):
  is_statement = True
  def __init__(self, test, then):
    self.test, self.then = test, then
    
  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.then==y.then
  
  def __repr__(self):
    if self.else_ is not None:
      return 'il.If2(%r, %r, %r)'%(self.test, self.then)
    else:
      return 'il.If2(%r, %r)'%(self.test, self.then)

class Unify(Element):
  def __init__(self, left, right, cont, fcont):
    self.left, self.right, self.cont, self.fcont =  left, right, cont,fcont
    
  def __eq__(x, y):
    return classeq(x, y) and x.left==y.left and x.right==y.right and x.cont==y.cont and x.fcont==y.fcont
  
  def __repr__(self):
    return 'il.Unify(%r, %r, %r, %r)'%(self.left, self.right, self.cont, self.fcont)

class BinaryOperationApply(Apply):
  def __repr__(self):
    return '%r(%r)'%(self.caller, self.args)

class BinaryOperation(Element):
  def __init__(self, name, operator):
    self.name, self.operator = name, operator
  
  def __call__(self, args):
    return BinaryOperationApply(self, args)
  
  def __eq__(x, y):
      return classeq(x, y) and x.operator==y.operator
  
  def __repr__(self):
    return 'il.%s'%self.name

add = BinaryOperation('add', '+')

class StatementList(Element):
  is_statement = True
  
  def __init__(self, statements):
    self.statements = statements
