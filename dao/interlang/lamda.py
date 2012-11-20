from dao.base import classeq

from dao.compilebase import optimize, MAX_EXTEND_CODE_SIZE, to_code_list, lambda_side_effects
from dao.compilebase import VariableNotBound, CompileTypeError

from element import Element, element, begin, pythonize_args, Return, Assign, optimize_once_args

def lamda(params, *body):
  body = tuple(element(x) for x in body)
  return Lamda(params, begin(*body))

class Lamda(Element):
  def __init__(self, params, body):
    self.params, self.body = params, body
    
  def new(self, params, body):
    return self.__class__(params, body)
  
  def __call__(self, *args):
    return Apply(self, tuple(element(arg) for arg in args))
  
  def find_assign_lefts(self):
    return self.body.find_assign_lefts()
  
  def assign_convert(self, env, compiler):
    try:
      self.before_assign_convert
      return self
    except: self.before_assign_convert = self.params, self.body
    
    lefts = self.find_assign_lefts()
    for p in lefts: 
      env[p] = compiler.new_var(p)
    if lefts:
      #make_cells = tuple((env[p], MakeCell(p)) for p in lefts)
      make_cells = tuple((env[p], MakeCell()) for p in lefts)
      self.body = let(make_cells, self.body.assign_convert(env, compiler))
    else:
      self.body = self.body.assign_convert(env, compiler)
    return self
    
  def optimization_analisys(self, data):
    try: self.seen
    except:
      self.seen = True
      data.occur_count[self] = data.occur_count.setdefault(self, 0)+1
      self.body.optimization_analisys(data)
        
  def code_size(self):
      return self.body.code_size()+len(self.params)+2
    
  def side_effects(self):
      return False
    
  def subst(self, bindings):
    return self.new(self.params, self.body.subst(bindings))
      
  def optimize_once(self, data):
    body, changed = self.body.optimize_once(data)
    return self.new(self.params, body), changed
  
  def insert_return_yield(self, klass):
    return klass(self)
  
  def pythonize_exp(self, env, compiler):
    body_exps, body_has_any_statement = self.body.pythonize_exp(env, compiler)
    if 0:#not body_has_any_statement:
      return (self.new(self.params, begin(*body_exps)),), False
    else:
      if not body_exps[-1].is_statement:
        body_exps = body_exps[:-1]+(Return(body_exps[-1]),)
      name = compiler.new_var(Var('function'))
      return (Function(name, self.params, begin(*body_exps)), name), True
        
  def to_code(self, coder):
    head = "lambda %s: " % ', '.join(to_code_list(coder, self.params))
    result = head + '%s'%self.body.to_code_if_in_lambda_body(coder)
    return result
  
  def __eq__(x, y):
    return classeq(x, y) and x.params==y.params and x.body==y.body
  
  def __hash__(self): return hash(id(self))
  
  def __repr__(self):
    return 'il.Lamda((%s), %s)'%(', '.join([repr(x) for x in self.params]),
                              repr(self.body))

def function( name, params, *body):
  body = tuple(element(x) for x in body)
  return Function(name, params, begin(*body))

class Function(Lamda):
  '''recuvsive Function'''
  is_statement = True
  is_function = True
  
  def __init__(self, name, params, body):
    Lamda.__init__(self, params, body)
    self.name = name
  
  def new(self, params, body):
    return self.__class__(self.name, params, body)
  
  def pythonize_exp(self, env, compiler):
    body_exps, has_any_statement = self.body.pythonize_exp(env, compiler)
    if not body_exps[-1].is_statement:
      body_exps = body_exps[:-1] + (Return(body_exps[-1]),)
    return (self.new(self.params, begin(*body_exps)), self.name), True
    
  def to_code(self, coder):
    head = "def %s(%s):\n" % (self.name, ', '.join(to_code_list(coder, self.params)))
    result =  head + coder.indent(self.body.to_code(coder))
    return result
  
  def __repr__(self):
    return 'il.Function(%s, (%s), %s)'%(self.name, ', '.join([repr(x) for x in self.params]),
                                 repr(self.body))    

def clamda(v, *body):
  body = tuple(element(x) for x in body)
  return Clamda(v, begin(*body))

class Clamda(Lamda):
  def __init__(self, v, body):
    self.params, self.body = (v, ), body
    self.name = None
    
  def new(self, params, body):
    return self.__class__(params[0], body)
  
  def __repr__(self):
    return 'il.Clamda(%r, %s)'%(self.params[0], repr(self.body))

class Done(Clamda):
  def __init__(self):
    v = Var('v')
    self.params, self.body = (v,), v
    
  def new(self, params, body):
    return self.__class__()
  
  def __call__(self, *args):
    return self.body.subst({self.params[0]:args[0]})
  
  def __repr__(self):
    return 'il.Done(%r, %s)'%(self.params[0], repr(self.body))

def cfunction(name, v, *body):
  body = tuple(element(x) for x in body)
  return CFunction(name, v, begin(*body))

class CFunction(Function):
  is_statement = True
  is_function = True
  
  def __init__(self, name, v, body):
    Function.__init__(self,  name, (v,), body)
    
  def new(self, params, body):
    return self.__class__(self.name, params[0], body)
  
  def __repr__(self):
    return 'il.CFunction(%r, %r, %s)'%(self.name, self.params[0],repr(self.body))
  
class Apply(Element):
  is_statement = False
  
  def __init__(self, caller, args):
    self.caller, self.args = caller, args

  def assign_convert(self, env, compiler):
    return self.__class__(self.caller.assign_convert(env, compiler), 
                 tuple(arg.assign_convert(env, compiler) for arg in self.args))    
    
  def find_assign_lefts(exp):
    return set()
  
  def optimization_analisys(self, data):  
    data.called_count[self.caller] = data.called_count.setdefault(self.caller, 0)+1
    self.caller.optimization_analisys(data)
    for arg in self.args:
      arg.optimization_analisys(data)
        
  def code_size(self):
    return self.caller.code_size()+sum([x.code_size() for x in self.args])
              
  def side_effects(self):
    if isinstance(self.caller, Lamda):
      if lambda_side_effects(self.caller): return True
    elif isinstance(self.caller, Var): return True
    elif self.caller.have_side_effects: return True
    else: return False # after cps, all of value have been solved before called, 
                       # so have no side effects.
          
  def subst(self, bindings):  
    return self.__class__(self.caller.subst(bindings), 
                 tuple(arg.subst(bindings) for arg in self.args))
      
  def optimize_once(self, data):    
    if isinstance(self.caller, Function):
      body, changed = self.caller.body.optimize_once(data)
      args, changed1 = optimize_once_args(self.args, data)
      return Apply(self.caller.new(self.caller.params, body), tuple(args)), changed or changed1
    
    if isinstance(self.caller, Lamda):
      #1. ((lambda () body))  =>  body 
      if len(self.caller.params)==0:
        return optimize(self.caller.body, data), True
      
      #2. (lamda x: ...x...)(y) => (lambda : ... y ...)() 
      bindings = {}
      args = self.args
      new_params, new_args = (), ()
      for i, p in enumerate(self.caller.params):
        arg = args[i]
        if arg.side_effects():
          new_params += (p,)
          new_args += (arg,)
          continue
        else:
          ref_count = data.ref_count.get(p, 0)
          if ref_count==0:
            continue
          elif ref_count==1:
            bindings[p] = arg
          else:
            if arg.code_size()*ref_count>MAX_EXTEND_CODE_SIZE: 
              # a(...y...), and a is (lamda ...x...: ...x...), 
              #then convert as above if code size is ok. 
              new_params += (p,)
              new_args += (arg,)
            else: 
              bindings[p] = arg
      
      if new_params:
        if bindings:
          return Apply(self.caller.new(new_params, optimize(self.caller.body.subst(bindings), data)), 
                          tuple(optimize(arg, data) for arg in new_args)), True
        else:
          if len(new_params)!=len(self.caller.params):
            Apply(self.caller.new(new_params, self.caller.body.subst(bindings).optimize(data)), 
                  tuple(optimize(arg, data) for arg in new_args)), True            
          else:
            caller_body, changed1 = self.caller.body.optimize_once(data)
            args, changed2 = optimize_once_args(new_args, data)
            return Apply(self.caller.new(new_params, caller_body), args), changed1 or changed2
      else:
        if not isinstance(self.caller, Function):
          if bindings:
            return optimize(self.caller.body.subst(bindings), data), True
          else:
            return optimize(self.caller.body, data), True
        else:
          if bindings:
            return Function(self.caller.name, (), 
                            optimize(self.caller.body.subst(bindings), data))(), True
          else:
            return Function(self.caller.name, (), 
                            optimize(self.caller.body, data))(), True
          
    else: 
      changed = False
      caller, changed1 = self.caller.optimize_once(data)
      args, changed2 = optimize_once_args(self.args, data)
      return self.__class__(caller, args), changed1 or changed2

  def insert_return_yield(self, klass):
    return klass(self)
  
  def pythonize_exp(self, env, compiler):
    exps, has_statement = self.caller.pythonize_exp(env, compiler)
    caller = exps[-1]
    exps = exps[:-1]
    exps2, args, has_statement2 = pythonize_args(self.args, env, compiler)
    return exps+exps2+(self.__class__(caller,args),), has_statement or has_statement2
    
  def to_code(self, coder):
    if isinstance(self.caller, Lamda):
      return "(%s)"%self.caller.to_code(coder) + '(%s)'%', '.join([x.to_code(coder) for x in self.args])
    else:
      return self.caller.to_code(coder, ) + '(%s)'%', '.join([x.to_code(coder) for x in self.args])        

  def __eq__(x, y):
    return classeq(x, y) and x.caller==y.caller and x.args==y.args
  
  def __repr__(self):
    return '%r(%s)'%(self.caller, ', '.join([repr(x) for x in self.args]))
  
class Var(Element):
  is_statement = False
  
  def __init__(self, name):
    self.name = name
        
  def assign_convert(self, env, compiler):
    if self in env:
      return Content(env[self])
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
      
  def insert_return_yield(self, klass):
    return klass(self)
  
  def pythonize_exp(self, env, compiler):
    return (self,), False
      
  def to_code(self, coder):
    return self.name
    
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  def __call__(self, *args):
    args = tuple(element(arg) for arg in args)
    return Apply(self, args)
  
  def free_variables(self):
    return set([self])
  
  def __hash__(self): return hash(self.name)

  def __repr__(self):
    return self.name #enough in tests

class LogicVar(Var):
  is_statement = False
  
  def __init__(self, name):
    self.name = name
  
  def assign_convert(self, env, compiler):
    return self
  
  def find_assign_lefts(exp):
    return set()
  
  def optimization_analisys(self, data): 
    return
  
  def optimize_once(self, data):
    return self, False
    
  def pythonize_exp(self, env, compiler):
    return (self,), False
      
  def deref(self, bindings):
    # todo:
    # how to shorten the binding chain? need to change solver.fail_cont.
    # deref(self, solver) can help
    while 1: 
      next = bindings[self]
      if not isinstance(next, LogicVar) or next==self:
        return next
      else: 
        self = next
  
  def to_code(self, coder):
    return  "LogicVar('%s')"%self.name
  
  def __repr__(self):
    return "LogicVar(%s)"%self.name 

