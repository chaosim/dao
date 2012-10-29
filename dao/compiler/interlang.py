from dao.base import classeq

class Clamda:
  def __init__(self, v, fc, exps):
    self.params, self.body = (v, fc), exps
    
  def __call__(self, v, fc):
    return Apply(self, v, fc)
  
  def __eq__(x, y):
    return classeq(x, y) and x.params==y.params and x.body==y.body
  
  def __repr__(self):
    return 'clamda(%r, %r, %s)'%(self.params[0], self.params[1], ', '.join([repr(x) for x in self.body]))

def clamda(v, fc, *exps): return Clamda(v, fc, exps)

class Var:
  def __init__(self, name):
    self.name = name
        
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  #def __repr__(self):
    #return 'Var(%r)'%self.name
  
  def __repr__(self):
    return self.name
    
class Apply:
  def __init__(self, caller, *args):
    self.caller, self.args = caller, args
                
  def __eq__(x, y):
    return classeq(x, y) and x.caller==y.caller and x.args==y.args
  
  #def __repr__(self):
    #return 'Apply(%r, %s)'%(self.caller, ', '.join([repr(x) for x in self.args]))
  
  def __repr__(self):
    return '%r(%s)'%(self.caller, ', '.join([repr(x) for x in self.args]))
  
class Return:
  def __init__(self, args):
    self.args = args
    
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    return 'ret(%s)'%', '.join([repr(x) for x in self.args])
  
def ret(*args): return Return(args)

class Assign:
  def __init__(self, var, exp):
    self.var, self.exp =  var, exp
    
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.exp==y.exp
  
  def __repr__(self):
    return 'il.assign(%r, %r)'%(self.var, self.exp)
  
def assign(var, exp): return Assign(var, exp)

class If:
  def __init__(self, test, then, else_):
    self.test, self.then, self.else_ = test, then, else_
    
  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.then==y.then and x.else_==y.else_
  
  def __repr__(self):
    if self.else_ is not None:
      return 'il.if_(%r, %r, %r)'%(self.test, self.then, self.else_)
    else:
      return 'il.if_(%r, %r)'%(self.test, self.then)

def if_(test, then, else_=None): return If(test, then, else_)

class Unify:
  def __init__(self, left, right, cont, fcont):
    self.left, self.right, self.cont, self.fcont =  left, right, cont,fcont
    
  def __eq__(x, y):
    return classeq(x, y) and x.left==y.left and x.right==y.right and x.cont==y.cont and x.fcont==y.fcont
  
  def __repr__(self):
    return 'il.unify(%r, %r, %r, %r)'%(self.left, self.right, self.cont, self.fcont)

def unify(left, right, cont, fcont): return Unify(left, right, cont, fcont)

class BinaryOperationApply:
  def __init__(self, operator, args):
    self.operator, self.args = operator, args
  
  def __eq__(x, y):
    return classeq(x, y) and x.operator==y.operator and x.args==y.args
    
  def __repr__(self):
    return '%r(%r)'%(self.operator, self.args)

class BinaryOperation:
  def __init__(self, name, operator):
    self.name, self.operator = name,operator
    
  def __call__(self, args):
    return BinaryOperationApply(self, args)
  
  def __eq__(x, y):
      return classeq(x, y) and x.operator==y.operator
  
  def __repr__(self):
    return 'il.%s'%self.name

add = BinaryOperation('add', '+')