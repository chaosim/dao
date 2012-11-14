from dao.base import classeq
from dao.compilebase import alpha_convert, cps_convert, assign_convert, find_assign_lefts
from dao.compilebase import optimization_analisys, optimize_once
from dao.compilebase import side_effects, optimize, subst, code_size, MAX_EXTEND_CODE_SIZE
from dao.compilebase import insert_return_yield, pythonize, to_code, to_code_list
from dao.compilebase import VariableNotBound, CompileTypeError

from utility import pythonize_list

class Element:
  have_side_effects = True
  is_statement = False
  
  def __init__(self, *args):
    if self.arity>=0:
      assert len(args)==self.arity, \
           '%s should have %s arguments.'%(
             self.__class__.__name__, self.__class__.arity)
    self.args = args
    
  def __getitem__(self, index):
    return GetItem(self, index)
  
  def __lt__(x, y): return Lt(x, y)
  def __le__(x, y): return Le(x, y)
  def __ge__(x, y): return Ge(x, y)
  def __gt__(x, y): return Gt(x, y)
  
  def __add__(x, y): return BinaryOperationApply(add, (x, y))
  
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    try: 
      if self.arity==0: 
        return 'il.%s'%self.__class__.__name__
    except: pass
    return 'il.%s(%s)'%(self.__class__.__name__, 
              ', '.join([repr(x) for x in self.args]))
   
class Var:
  def __init__(self, name):
    self.name = name
        
  def alpha_convert(self, env, compiler):
    return env[self]
    
  def cps_convert(self, compiler, cont):
    return cont(self)
  
  def cps_convert_unify(x, y, cont):
    v = Var('v')
    return begin(
      Assign(x, Deref(x)),
      If(Isinstance(x, LogicVar),
         begin(SetBinding(x, y),
               AppendFailCont(DelBinding(x)),
               cont(True)),
         begin(
           Assign(y, Deref(y)), 
           If(Isinstance(y, LogicVar),
              begin(SetBinding(y, x),
                    AppendFailCont(DelBinding(y)),
                    cont(True)),
              If(Eq(x, y), cont(True), failcont(True))))))
    
  def assign_convert(self, env, compiler):
    if self in env:
      return contents(env[self])
    else: return self
    
  def find_assign_lefts(self):
    return set()
  
  def optimization_analisys(self, data):
    data.ref_count[self] = data.ref_count.setdefault(self, 0)+1    
    
  def code_size(self):
    return 1
        
  def side_effects(self):
    return False
        
  def subst(self, bindings):  
    try: return bindings[self]
    except: return self
      
  def optimize_once(self, data):
    return self, False
      
  def pythonize(self, env, compiler):
    return self
      
  def to_code(self, coder):
    return self.name
    
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  def __call__(self, *args):
    return Apply(self, args)
  
  def free_variables(self):
    return set([self])
  
  def __hash__(self): return hash(self.name)

  def __repr__(self):
    return self.name #enough in tests

class LogicVar:
  is_statement = False
  
  def __init__(self, name):
    self.name = name
  
  def alpha_convert(self, env, compiler):
    return self
  
  def cps_convert(self, compiler, cont):
    return cont(self)
  
  def assign_convert(self, env, compiler):
    return self
  
  def cps_convert_unify(self, other, cont):
    return begin(SetBinding(self, other),
                 AppendFailCont(DelBinding(self)),
                 cont(True))
  
  def optimization_analisys(self, data): 
    return
  
  def optimize_once(self, data):
    return self, False
    
  def insert_return_yield(self, klass):
    return klass(self)
  
  def pythonize(self, env, compiler):
    return self
      
  def to_code(self, coder):
    return  "LogicVar('%s')"%self.name
  
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  def __repr__(self):
    return "LogicVar(%s)"%self.name 

class Return(Element):
  is_statement = True
  
  def __init__(self, *args):
    self.args = args
  
  def alpha_convert(self, env, compiler):
    return self.__class__(*tuple(env.alpha_convert(arg, compiler) for arg in self.args))    
    
  def assign_convert(self, env, compiler):
    return self.__class__(*tuple(assign_convert(arg, env, compiler) for arg in self.args))
    
  def optimization_analisys(self, data):  
    for arg in self.args:
      optimization_analisys(arg, data)
        
  def code_size(self):
    return sum([code_size(x) for x in self.args])

  def side_effects(self):
    return False
        
  def subst(self, bindings):  
    return self.__class__(*tuple(subst(arg, bindings) for arg in self.args))
    
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
    for x in args:
      x, x_changed = optimize_once(x, data)
      result.append(x)
      changed = changed or x_changed
    return self.__class__(*result), changed
  
  def pythonize(self, env, compiler):
    defs, args = pythonize_list(self.args, env, compiler)
    return collocate(defs, self.__class__(*args))
    
  def to_code(self, coder):
    if coder.lambda_stack:
      try: 
        coder.lambda_stack[-1].is_function
        return  'return %s' % ', '.join([to_code(coder, x) for x in self.args])
      except:
        return  ', '.join([to_code(coder, x) for x in self.args])
    else:
      return  ', '.join([to_code(coder, x) for x in self.args])
  
  def insert_return_yield(self, klass):
    return self
  
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    return 'il.Return(%s)'%', '.join([repr(x) for x in self.args])

class Yield(Return): 
  def to_code(self, coder):
    if coder.lambda_stack:
      try: 
        coder.lambda_stack[-1].is_function
        return  'yield %s' % ', '.join([to_code(coder, x) for x in self.args])
      except:
        return  ', '.join([to_code(coder, x) for x in self.args])
    else:
      return  ', '.join([to_code(coder, x) for x in self.args])
  def __repr__(self):
    return 'il.Yield(%s)'%', '.join([repr(x) for x in self.args])

class Assign(Element):
  is_statement = True
  def __init__(self, var, exp):
    self.var, self.exp =  var, exp
  
  def alpha_convert(self, env, compiler):
    try: converted_var = env[self.var]
    except VariableNotBound:
      converted_var = env.bindings[self.var] = compiler.new_var(self.var)
    return Assign(converted_var, alpha_convert(self.exp, env))
    
  def assign_convert(self, env, compiler):
    # var = value, exp.exp should be a single var, 
    # which is the continuation param which ref to the value    
    return vop.SetContents(self.var, self.exp)
  
  def find_assign_lefts(self):
    return set([self.var])
  
  def optimization_analisys(self, data):  
    optimization_analisys(self.exp, data)
  
  def insert_return_yield(self, klass):
    return begin(self, klass(None))
  
  def code_size(exp):
    # var = value, exp.exp should be a single var, 
    # which is the continuation param which ref to the value
    return code_size(exp.value)+2
    
  def side_effects(self):
    # var = value, self.self should be a single var, 
    # which is the continuation param which ref to the value
    return True
    
  def subst(self, bindings):  
    return Assign(self.var, subst(self.exp, bindings))
        
  def optimize_once(self, data):
    return self, False
    
  def pythonize(self, env, compiler):
    # var = value, self.self should be a single var, 
    # which is the continuation param which ref to the value
    return self
    
  def to_code(self, coder):
    return  '%s = %s' % (to_code(coder, self.var), to_code(coder, self.exp))
    
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.exp==y.exp
  
  def __repr__(self):
    return 'il.Assign(%r, %r)'%(self.var, self.exp)
  
class If(Element):
  def __init__(self, test, then, else_):
    self.test, self.then, self.else_ = test, then, else_
    
  def alpha_convert(self, env, compiler):
    return If(alpha_convert(self.test, env, compiler), alpha_convert(self.then, env, compiler), 
                 alpha_convert(self.else_, env, compiler))

  def assign_convert(self, env, compiler):
    return If(assign_convert(self.test, env, compiler), 
                 assign_convert(self.then, env, compiler), 
                 assign_convert(self.else_, env, compiler))

  def find_assign_lefts(self):
    return find_assign_lefts(self.then) | find_assign_lefts(self.else_)
  def optimization_analisys(self, data):  
    optimization_analisys(self.test, data)
    optimization_analisys(self.then, data)
    optimization_analisys(self.else_, data)
    
  def code_size(self):
    return 3 + code_size(self.test) + \
           code_size(self.then_) + \
           code_size(self.else_)
  
  def side_effects(self):
    return not side_effects(self.test) and\
           not side_effects(self.then_) and\
           not side_effects(self.else_)
    
  def subst(self, bindings):  
    return If(subst(self.test, bindings), 
                 subst(self.then, bindings), subst(self.else_, bindings))
    
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
    test, test_changed = optimize_once(result.test, data)
    then, then_changed = optimize_once(result.then, data)
    else_, else__changed = optimize_once(result.else_, data)
    result = If(test, then, else_)
    #if isinstance(result.test, Let):
      #result = Let(result.bindings, If(Begin(let.body), 
                                             #result.then, result.else_))
    return result, changed or test_changed or then_changed or else__changed

  def insert_return_yield(self, klass):
    return If(self.test, 
              insert_return_yield(self.then, klass), 
              insert_return_yield(self.else_, klass))
  
  def pythonize(self, env, compiler):
    defs, (test, then, else_) = pythonize_list((self.test, self.then, self.else_), env, compiler)
    return collocate(defs, If(test, then, else_))
    
  def to_code(self, coder):
    if coder.lambda_stack and isinstance(coder.lambda_stack[-1], Function):
      return 'if %s: \n%s\nelse:\n%s' % (to_code(coder, self.test), coder.indent(to_code(coder, self.then)), 
                                       coder.indent(to_code(coder, self.else_)))        
    else:
      return '%s if %s else %s' % (to_code(coder, self.then), to_code(coder, self.test), 
                                       to_code(coder, self.else_))        
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
    
  def alpha_convert(self, env, compiler):
    return If2(env.alpha_convert(self.test, compiler), env.alpha_convert(self.then, compiler))
    
  def assign_convert(self, env, compiler):
    return If(assign_convert(self.test, env, compiler), 
                 assign_convert(self.then, env, compiler))
    
  def optimization_analisys(self, data):  
    optimization_analisys(self.test, data)
    optimization_analisys(self.then, data)
    
  def code_size(self):
    return 3 + code_size(self.test) + \
           code_size(self.then_)
  
  def side_effects(self):
    return not side_effects(self.test) and\
           not side_effects(self.then_)
  
  def subst(self, bindings):  
    return If(subst(self.test, bindings), 
                 subst(self.then, bindings), 
                 subst(self.else_, bindings))
    
  #def optimize_once(exp, data):
      #return If2(optimize(exp.test, data), optimize(exp.then, data))
    
  def pythonize(self, env, compiler):
    defs, (test, then) = pythonize_list((self.test, self.then), env, compiler)
    return collocate(defs, If2(test, then))
    
  def to_code(self, coder):
    return 'if %s: \n%s\n' % (to_code(coder, self.test), coder.indent(to_code(coder, self.then)))

  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.then==y.then
  
  def __repr__(self):
    return 'il.If2(%r, %r)'%(self.test, self.then)

class Unify(Element):
  def __init__(self, left, right, cont, fcont):
    self.left, self.right, self.cont, self.fcont =  left, right, cont, fcont
    
  def alpha_convert(self, env, compiler):
    return Unify(env.alpha_convert(self.left, compiler), env.alpha_convert(self.right),
                 env.alpha_convert(self.cont, compiler), env.alpha_convert(self.fcont, compiler))
    
  def assign_convert(self, env, compiler):
    return Unify(assign_convert(self.left, env, compiler), assign_convert(self.right, env, compiler),
                 assign_convert(self.cont, env, compiler), assign_convert(self.fcont, env, compiler))
    
  def optimization_analisys(self, data):  
    optimization_analisys(self.left, data)
    optimization_analisys(self.right, data)
    optimization_analisys(self.cont, data)
    optimization_analisys(self.fcont, data)

  def side_effects(self):
      return False
      
  def code_size(self):
    return code_size(self.left) + code_size(self.right) + code_size(self.cont) + code_size(self.fcont)
    
  def subst(self, bindings):  
    return Unify(subst(self.left, bindings), subst(self.right, bindings),
                 subst(self.cont, bindings), subst(self.fcont, bindings))

  def optimize_once(self, data):
    left, left_changed = optimize_once(self.left, data)
    right, right_changed = optimize_once(self.right, data)
    cont, cont_changed = optimize_once(self.cont, data)
    fcont, fcont_changed = optimize_once(self.fcont, data)
    return Unify(left, right, cont), left_changed or right_changed or cont_changed or fcont_changed
    
  def pythonize(self, env, compiler):
    defs, (left, right, cont) = pythonize_list((self.left, self.right, self.cont, self.fcont), env, compiler)
    return collocate(defs, Unify(left, right, cont))
    
  def to_code(self, coder):
    return 'unify(%s, %s, %s, %s)' % (to_code(coder, self.left), to_code(coder, self.right), 
                                     to_code(coder, self.cont), to_code(coder, self.fcont))
    
  def __call__(self, v):
    return Apply(self, (v,))
    
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
    

def collocate(defs, exp):
  if defs:
    return Begin(defs + (exp,))
  else: 
    return exp

class Begin(Element):
  is_statement = True
  
  def __init__(self, statements):
    self.statements = statements
    
  def alpha_convert(self, env):
    return Begin(tuple([alpha_convert(x, env, compiler) for x in self.statements]))
  
  def assign_convert(self, env, compiler):
    return Begin(tuple(assign_convert(x, env, compiler) for x in self.statements))
  
  def optimization_analisys(self, data):  
    for x in self.statements:
      optimization_analisys(x, data)  
  
  def subst(self, bindings):  
    return Begin(tuple(subst(x, bindings) for x in self.statements))
  
  def optimize_once(self, data):
    changed = False
    result = []
    for x in self.statements:
      x, x_changed = optimize_once(x, data)
      result.append(x)
      changed = changed or x_changed
    return begin(tuple(result)), changed
        
  def insert_return_yield(self, klass):
    if not self.statements: 
      return klass(*self.statements)
    elif len(self.statements)==1:
      return insert_return_yield(self.statements[1], klass)
    else:
      inserted = insert_return_yield(self.statements[-1], klass)
      if isinstance(inserted, tuple):
        return begin(*(self.statements[:-1]+inserted))
      else:
        return begin(*(self.statements[:-1]+(inserted,)))
  
  def pythonize(self, env, compiler):
    return self
  
  def to_code(self, coder):
    return  '\n'.join([to_code(coder, x) for x in self.statements])
      
  def __eq__(x, y):
      return classeq(x, y) and x.statements==y.statements
  
  def __repr__(self):
    return 'il.begin(%s)'%', '.join([repr(x) for x in self.statements])

def begin(*exps):
  assert isinstance(exps, tuple)
  if len(exps)==1: 
    return exps[0]
  else:
    result = []
    for e in exps:
      if isinstance(e, Begin):
        result += e.statements
      else:
        result.append(e)
    return Begin(tuple(result))

