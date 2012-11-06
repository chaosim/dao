from dao.base import classeq

class Element:
  have_side_effects = True
  def __init__(self, *args):
    if self.arity>=0:
      assert len(args)==self.arity, \
           '%s should have %s arguments.'%(
             self.__class__.__name__, self.__class__.arity)
    self.args = args
    
  def __getitem__(self, index):
    return getitem(self, index)
  
  def __lt__(x, y): return lt(x, y)
  def __le__(x, y): return le(x, y)
  def __ge__(x, y): return ge(x, y)
  def __gt__(x, y): return gt(x, y)
  
  def __add__(x, y): return BinaryOperationApply(add, (x, y))
  
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    return 'il.%s(%s)'%(self.__class__.__name__, 
              ', '.join([repr(x) for x in self.args]))
   
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
    return 'il.Lamda((%s), %s)'%(', '.join([repr(x) for x in self.params]),
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
    return 'il.Clamda(%r, %r, %s)'%(self.params[0], self.params[1], ', '.join([repr(x) for x in self.body]))

class Done(Clamda):
  def __init__(self):
    v, fc = Var('v'), Var('fc')
    self.params, self.body = (v, fc), (Return((v, fc)),)
    self.name = None
    
  def __repr__(self):
    return 'il.Done()'

class CFunction(Clamda):
  is_statement = True
  def __init__(self, name, v, fc, *body):
    Clamda.__init__(self,  v, fc, *body)
    self.name = name
  def __repr__(self):
    return 'il.CFunction(%r, %r, %r, %s)'%(self.name, self.params[0], self.params[1], ', '.join([repr(x) for x in self.body]))
  
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
    return 'il.Return(%s)'%', '.join([repr(x) for x in self.args])
  
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
    return 'il.If2(%r, %r)'%(self.test, self.then)

class Unify(Element):
  def __init__(self, left, right, cont, fcont):
    self.left, self.right, self.cont, self.fcont =  left, right, cont,fcont
    
  def __call__(self, v, fc):
    return Apply(self, (v, fc))
    
  def __eq__(x, y):
    return classeq(x, y) and x.left==y.left and x.right==y.right and x.cont==y.cont and x.fcont==y.fcont
  
  def __repr__(self):
    return 'il.Unify(%r, %r, %r, %r)'%(self.left, self.right, self.cont, self.fcont)

class Let(Element):
  def __init__(self, bindings, *body):
    self.bindings = bindings
    self.body = body
    
  def __eq__(x, y):
    return classeq(x, y) and x.bindings==y.bindings and x.body==y.body
  
  def __repr__(self):
    return 'il.Let(%r, %s)'%(self.bindings, ', '.join([repr(x) for x in self.body]))
    
class BinaryOperationApply(Apply):
  def __repr__(self):
    return '%r(%r)'%(self.caller, self.args)

class BinaryOperation(Element):
  def __init__(self, name, operator, have_side_effects=True):
    self.name, self.operator = name, operator
    self.have_side_effects = have_side_effects
  
  def __call__(self, args):
    return BinaryOperationApply(self, args)
  
  def __eq__(x, y):
      return classeq(x, y) and x.operator==y.operator
    
  def __hash__(self): return hash(self.operator)
  
  def __repr__(self):
    return 'il.%s'%self.name

add = BinaryOperation('add', '+', False)

class StatementList(Element):
  is_statement = True
  
  def __init__(self, statements):
    self.statements = statements
    
  def __eq__(x, y):
      return classeq(x, y) and x.statements==y.statements
  
  def __repr__(self):
    return 'il.StatementList(%s)'%repr(self.statements)

def statements(exps):
  assert isinstance(exps, tuple)
  if len(exps)==1: 
    return exps[0]
  else:
    return StatementList(exps)
  
def vop(name, arity):
  class Vop(Element): pass
  Vop.__name__ = name
  Vop.arity = arity
  return Vop

class getitem(Element):
  arity = 2
  
  def __repr__(self):
    return '%r[%r]'%(self.args)
  
Not = vop('Not', 1)
Len = vop('Len', 1)
get_parse_state = vop('get_parse_state', 0)
set_parse_state = vop('set_parse_state', 1)
assign_from_list = vop('assign_from_list', -1)
begin = vop('begin', -1)
empty_list = vop('empty_list', 0)
list_append = vop('list_append', 2)
getvalue = vop('getvalue', 1)

def binary(name, symbol):
  class Binary(Element): 
    def __repr__(self):
      return '(%s%s%s)'%(repr(self.args[0]), self.__class__.symbol, repr(self.args[1]))
  Binary.__name__ = name
  Binary.arity = 2
  Binary.symbol = symbol
  return Binary
  
lt = binary('lt', '<')
le = binary('le', '<=')
eq = vop('eq', 2)
ne = vop('ne', 2)
ge = binary('ge', '>=')
gt = binary('gt', '>')