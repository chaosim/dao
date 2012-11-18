from dao.base import classeq

from dao.compilebase import optimize, MAX_EXTEND_CODE_SIZE, to_code_list
from dao.compilebase import VariableNotBound, CompileTypeError

def pythonize(self, env, compiler):
  exps, has_any_statement = self.pythonize_exp(env, compiler)
  return begin(*exps)

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
    result.append(exps2[-1])
    exps += exps2[:-1]
  return exps, result, has_statement
    
def cps_convert_exps(compiler, exps, cont):
  v = compiler.new_var(Var('v'))
  if not exps: return Clamda(v, cont(il.tuple()))
  if len(exps)==1:
    return exps[0].cps_convert(compiler, cont)
  else:
    return exps[0].cps_convert(compiler, Clamda(v, cps_convert_exps(compiler, exps[1:], cont)))
  
class Element:
  have_side_effects = True
  is_statement = False
  
  def __init__(self, *args):
    if self.arity>=0:
      assert len(args)==self.arity, \
           '%s should have %s arguments.'%(
             self.__class__.__name__, self.__class__.arity)
    self.args = args
  
  def tail_recursive_convert(self):
    return self
  
  def trampoline(self):
    return self
    
  def lambda_body_to_code(self, coder):
    return self.to_code(coder)
  
  def replace_return_yield(self, klass):
    return self
  
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    try: 
      if self.arity==0: 
        return 'il.%s'%self.__class__.__name__
    except: pass
    return 'il.%s(%s)'%(self.__class__.__name__, 
              ', '.join([repr(x) for x in self.args]))
   
class Atom(Element):
  def __init__(self, value):
    self.value = value
    
  def alpha_convert(self, env, compiler):
    return self
    
  def cps_convert(self, compiler, cont):
    return cont(self)
    
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
  
  def insert_return_yield(self, klass):
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
  
class Integer(Atom): pass
class Float(Atom): pass
class String(Atom): pass
class List(Atom): pass

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
  
  def alpha_convert(self, env, compiler):
    try:
      self.before_alpha_convert
      return self
    except: self.before_alpha_convert  = self.params, self.body
    
    new_env = env.extend()
    for p in self.params: 
      new_env.bindings[p] = compiler.new_var(p)
    self.params = tuple(new_env[p] for p in self.params)
    self.body = self.body.alpha_convert(new_env, compiler)
    self.variables = new_env.bindings.values()
    return self
    
  def cps_convert(self, compiler, cont):
    k = compiler.new_var(Var('cont'))
    return cont(self.new((k,)+self.params, self.body.cps_convert(compiler, k)))
  
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
    if not body_has_any_statement:
      return (self.new(self.params, begin(*body_exps)),), False
    else:
      if not body_exps[-1].is_statement:
        body_exps = body_exps[:-1]+(Return(body_exps[-1]),)
      name = compiler.new_var(Var('function'))
      return (Function(name, self.params, begin(*body_exps)), name), True
        
  def to_code(self, coder):
    head = "lambda %s: " % ', '.join(to_code_list(coder, self.params))
    result = head + '%s'%self.body.lambda_body_to_code(coder)
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
  
def optimize_once_args(args, data):
  changed = False
  result = []
  for arg in args:
    arg, changed1 = arg.optimize_once(data)
    changed = changed or changed1
    result.append(arg)
  return tuple(result), changed
    
class Apply(Element):
  is_statement = False
  
  def __init__(self, caller, args):
    self.caller, self.args = caller, args

  def alpha_convert(self, env, compiler):
    return self.__class__(self.caller.alpha_convert(env, compiler), 
                 tuple(arg.alpha_convert(env, compiler) for arg in self.args))
  
  def cps_convert(self, compiler, cont):
    # see The 90 minute Scheme to C compiler by Marc Feeley
    if isinstance(self.caller, Lamda):
      args = self.args
      fun =  self.caller.body.cps_convert(compiler, cont)
      for var, arg in reversed(zip(self.caller.params, args)):
        fun = arg.cps_convert(compiler, Clamda(var, fun))
      return fun
    else:
      function = compiler.new_var(Var('function'))
      vars = tuple(compiler.new_var(Var('a'+repr(i))) for i in range(len(self.args)))
      fun = Apply(function, (cont,)+vars)
      for var, self in reversed(zip((function,)+vars, (self.caller,)+self.args)):
        fun = self.cps_convert(compiler, Clamda(var, fun))
      return fun

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
        
  def alpha_convert(self, env, compiler):
    try: 
      return env[self]
    except KeyError: 
      raise VariableNotBound(self)
    
  def cps_convert(self, compiler, cont):
    return cont(self)
  
  def cps_convert_unify(x, y, compiler, cont):
    try: 
      y.cps_convert_unify
    except:
      x1 = compiler.new_var(Var('x'))
      return begin(
        Assign(x1, Deref(x)), #for LogicVar, could be optimized when generate code.
        If(IsLogicVar(x1),
           begin(SetBinding(x1, y),
                 append_fail_cont(compiler, DelBinding(x1)),
                 cont(TRUE)),
                If(Eq(x1, y), cont(TRUE), failcont(TRUE))))
    x1 = compiler.new_var(Var('x'))
    y1 = compiler.new_var(Var('y'))
    return begin(
      Assign(x1, Deref(x)), #for LogicVar, could be optimized when generate code.
      Assign(y1, Deref(y)),
      If(IsLogicVar(x1),
         begin(SetBinding(x1, y1),
               append_fail_cont(compiler, DelBinding(x1)),
               cont(TRUE)),
         begin(
           If(IsLogicVar(y1),
              begin(SetBinding(y1, x1),
                    append_fail_cont(compiler, DelBinding(y1)),
                    cont(TRUE)),
              If(Eq(x1, y1), cont(TRUE), failcont(TRUE))))))

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
  
  def alpha_convert(self, env, compiler):
    return self
  
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

class Assign(Element):
  is_statement = True
  
  def __init__(self, var, exp):
    self.var, self.exp =  var, exp
  
  def alpha_convert(self, env, compiler):
    try: converted_var = env[self.var]
    except VariableNotBound:
      converted_var = env.bindings[self.var] = compiler.new_var(self.var)
    return Assign(converted_var, self.exp.alpha_convert(env, compiler))
    
  def assign_convert(self, env, compiler):
    return SetContent(env[self.var], self.exp.assign_convert(env, compiler))
  
  def find_assign_lefts(self):
    return set([self.var])
  
  def optimization_analisys(self, data):  
    self.exp.optimization_analisys(data)
  
  def insert_return_yield(self, klass):
    return begin(self, klass(None))
  
  def code_size(self):
    # var = value, exp.exp should be a single var, 
    # which is the continuation param which ref to the value
    return code_size(self.exp)+2
    
  def side_effects(self):
    # var = value, self.self should be a single var, 
    # which is the continuation param which ref to the value
    return True
    
  def subst(self, bindings):  
    return Assign(self.var, self.exp.subst(bindings))
        
  def optimize_once(self, data):
    exp, changed = self.exp.optimize_once(data)
    return Assign(self.var, exp), changed
    
  def pythonize_exp(self, env, compiler):
    exps, has_statement = self.exp.pythonize_exp(env, compiler)
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
  
  def alpha_convert(self, env, compiler):
    return self.__class__(*tuple(arg.alpha_convert(env, compiler) for arg in self.args))    
    
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
  
  def insert_return_yield(self, klass):
    return klass(*self.args)
  
  def replace_return_yield(self, klass):
    return klass(*self.args)
  
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    return 'il.Return(%s)'%', '.join([repr(x) for x in self.args])

class Yield(Return): 
  def to_code(self, coder):
    return  'yield %s' % ', '.join([x.to_code(coder) for x in self.args])

  def __repr__(self):
    return 'il.Yield(%s)'%', '.join([repr(x) for x in self.args])

class PseudoElse(Atom):
  def __init__(self):
    return
  def code_size(self):
    return 0
  def __eq__(x, y):
    return classeq(x, y)
  
  def __repr__(self):
    return 'il.pseudo_else'

pseudo_else = PseudoElse()

class If(Element):
  def __init__(self, test, then, else_):
    self.test, self.then, self.else_ = test, then, else_
    if else_==pseudo_else: self.is_statement = True
    
  def alpha_convert(self, env, compiler):
    return If(self.test.alpha_convert(env, compiler), self.then.alpha_convert(env, compiler), 
                 self.else_.alpha_convert(env, compiler))

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

  def insert_return_yield(self, klass):
    return If(self.test, 
              self.then.insert_return_yield(klass), 
              self.else_.insert_return_yield(klass))
  
  def replace_return_yield(self, klass):
    return If(self.test, 
              self.then.replace_return_yield(klass), 
              self.else_.replace_return_yield(klass))
  
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

class Begin(Element):
  is_statement = True
  
  def __init__(self, statements):
    self.statements = statements
    
  def alpha_convert(self, env, compiler):
    return Begin(tuple([x.alpha_convert(env, compiler) for x in self.statements]))
  
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
        
  def insert_return_yield(self, klass):
    replaced = tuple(exp.replace_return_yield(klass) for exp in self.statements[:-1])
    inserted = self.statements[-1].insert_return_yield(klass)
    return Begin(replaced+(inserted,))
  
  def replace_return_yield(self, klass):
    return Begin(tuple(exp.replace_return_yield(klass) for exp in self.statements))
  
  def pythonize_exp(self, env, compiler):
    return pythonize_exps(self.statements, env, compiler)
  
  def to_code(self, coder):
    return  '\n'.join([x.to_code(coder) for x in self.statements])
      
  def lambda_body_to_code(self, coder):
    return  '(%s)'%', '.join([x.to_code(coder) for x in self.statements])

  def __eq__(x, y):
      return classeq(x, y) and x.statements==y.statements
  
  def __repr__(self):
    return 'il.begin(%s)'%', '.join([repr(x) for x in self.statements])

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

class BinaryOperation(Element):
  def __init__(self, name, operator, have_side_effects=True):
    self.name, self.operator = name, operator
    self.have_side_effects = have_side_effects
  
  def alpha_convert(self, env, compiler):
    return self
  
  def assign_convert(self, env, compiler):
    return self
  
  def optimization_analisys(self, data):  
    return self
  
  def subst(self, bindings):  
    return self

  def optimize_once(self, data):
    return self, False
  
  def code_size(self): 
    return 1
  
  def pythonize_exp(self, env, compiler):
    return (self,), False
    
  def to_code(self, coder):
    return self.operator
      
  def __call__(self, *args):
    return BinaryOperationApply(self, args)
  
  def __eq__(x, y):
      return classeq(x, y) and x.operator==y.operator
    
  def __hash__(self): return hash(self.operator)
  
  def __repr__(self):
    return 'il.%s'%self.name

add = BinaryOperation('add', '+', False)
sub = BinaryOperation('sub', '-', False)
mul = BinaryOperation('mul', '*', False)
div = BinaryOperation('div', '-', False)

class BinaryOperationApply(Apply):
  is_statement = False
  
  def __init__(self, caller, args):
    self.caller, self.args = caller, args

  def alpha_convert(self, env, compiler):
    return self.__class__(self.caller.alpha_convert(env, compiler), 
                 tuple(arg.alpha_convert(env, compiler) for arg in self.args))
  
  def assign_convert(self, env, compiler):
    return self.__class__(self.caller.assign_convert(env, compiler), 
                 tuple(arg.assign_convert(env, compiler) for arg in self.args))    
    
  def optimization_analisys(self, data):  
    data.called_count[self.caller] = data.called_count.setdefault(self.caller, 0)+1
    self.caller.optimization_analisys(data)
    for arg in self.args:
      arg.optimization_analisys(data)
        
  def code_size(self):
    return self.caller.code_size()+sum([x.code_size() for x in self.args])
              
  def side_effects(self):
    if isinstance(self.caller, Var): return True
    elif self.caller.have_side_effects: return True
    else: return False # after cps, all of value have been solved before called, 
                       # so have no side effects.
          
  def subst(self, bindings):  
    return self.__class__(self.caller.subst(bindings), 
                 tuple(arg.subst(bindings) for arg in self.args))
      
  def optimize_once(self, data):    
    changed = False
    caller, changed1 = self.caller.optimize_once(data)
    args, changed2 = optimize_once_args(self.args, data)
    return self.__class__(caller, args), changed1 or changed2

  def insert_return_yield(self, klass):
    return klass(self)
  
  def pythonize_exp(self, env, compiler):
    exps, args, has_statement = pythonize_args(self.args, env, compiler)
    return exps+(self.__class__(self.caller, args),), has_statement
  
  def to_code(self, coder):
    return '%s%s%s'%(self.args[0].to_code(coder), 
                        self.caller.to_code(coder), 
                        self.args[1].to_code(coder))
    
  def __repr__(self):
    return '%r(%r)'%(self.caller, self.args)
  
  def __repr__(self):
    return '%r%s%r'%(self.args[0], self.caller.operator, self.args[1])
  

class VirtualOperation(Element):
  def __call__(self, *args):
    return Apply(self, args)

  def alpha_convert(self, env, compiler):
    return self.__class__(*tuple(arg.alpha_convert(env, compiler) for arg in self.args))
  
  def assign_convert(self, env, compiler):
    return self.__class__(*tuple(arg.assign_convert(env, compiler) for arg in self.args))
  
  def find_assign_lefts(self):
    return set()
  
  def side_effects(self):
    return True

  def optimization_analisys(self, data):
    for arg in self.args:
      arg.optimization_analisys(data)
  
  def subst(self, bindings):  
    return self.__class__(*tuple(x.subst(bindings) for x in self.args))
  
  def code_size(self):
    return 1
  
  def optimize_once(self, data):
    return self, False
  
  def insert_return_yield(self, klass):
    return klass(self)
  
  def pythonize_exp(self, env, compiler):
    exps, args, has_statement = pythonize_args(self.args, env, compiler)
    try: self_is_statement = self.is_statement
    except: self_is_statement = False
    return exps+(self.__class__(*args),), self_is_statement or has_statement
  
  def to_code(self, coder):
    if isinstance(self.__class__.code_format, str):
      if self.__class__.arity==0:
        return self.__class__.code_format
      elif self.__class__.arity!=-1:
        return self.__class__.code_format % tuple(x.to_code(coder) for x in self.args)
      else:
        return self.__class__.code_format % (', '.join([x.to_code(coder) for x in self.args]))
    else: 
      return self.__class__.code_format(self, coder)
      
  def __hash__(self):
    return hash(self.__class__.__name__)

def vop(name, arity, code_format):
  class Vop(VirtualOperation): pass
  Vop.name = Vop.__name__  = name
  Vop.arity = arity
  Vop.code_format = code_format
  Vop.is_statement = False
  return Vop

def vop2(name, arity, code_format):
  class Vop(VirtualOperation): pass
  Vop.__name__ = name
  Vop.arity = arity
  Vop.code_format = code_format
  Vop.is_statement = True
  return Vop

class GetItem(Element):
  arity = 2
  
  def __repr__(self):
    return '%r[%r]'%(self.args)

GetItem = vop('GetItem', 2, '(%s)[%s]')  
Not = vop('Not', 1, "not %s")
def AssignFromList_to_code(self, coder):
  return "%s = %s" % (', '.join([x.to_code(coder) for x in self.args[:-1]]), 
                      self.args[-1].to_code(coder))
AssignFromList = vop2('AssignFromList', -1, AssignFromList_to_code)
Isinstance = vop('Isinstance', 2, "isinstance(%s, %s)")
EmptyList = vop('empty_list', 0, '[]')
empty_list = EmptyList()
ListAppend = vop('ListAppend', 2, '%s.append(%s)')
Len = vop('Len', 1, 'len(%s)')
RaiseTypeError = vop2('RaiseTypeError', 1, 'raise %s')
SetContent = vop2('SetContent', 2, '%s[0] = %s')
Content = vop('Content', 1, '%s[0]')
#MakeCell = vop('MakeCell', 1, '[%s]') # assign to upper level variable is possible.
MakeCell = vop('MakeCell', 0, '[None]') #assign always generate new local variable, like python.

SetFailCont = vop2('SetFailCont', 1, 'solver.fail_cont = %s')
FailCont = vop('failcont', 0, 'solver.fail_cont')  
failcont = FailCont()

SetCutOrCont = vop2('SetCutOrCont', 1, 'solver.cut_or_cont = %s')
CutOrCont = vop('CutOrCont', 0, 'solver.cut_or_cont')
cut_or_cont = CutOrCont()

IsLogicVar = vop('IsLogicVar', 1, 'isinstance(%s, LogicVar)')
Deref = vop('Deref', 1, 'deref(%s, solver.bindings)')
SetBinding = vop2('SetBinding', 2, 'solver.bindings[%s] = %s')
DelBinding = vop2('DelBinding', 1, 'del solver.bindings[%s]')
GetValue = vop('GetValue', 1, 'getvalue(%s, solver.bindings')

SetParseState = vop2('SetParseState', 1, 'solver.parse_state = %s')
ParseState = vop('parse_state', 0, 'solver.parse_state')
parse_state = ParseState()

def binary_to_code(self, coder):
  return '(%s) %s (%s)'%(self.args[0].to_code(coder), 
                         self.symbol, 
                         self.args[1].to_code(coder))

def binary(name, symbol):
  class Binary(VirtualOperation): 
    def __repr__(self):
      return '(%s%s%s)'%(repr(self.args[0]), self.__class__.symbol, repr(self.args[1]))
  Binary.__name__ = Binary.name = name
  Binary.arity = 2
  Binary.symbol = symbol
  Binary.code_format = binary_to_code
  return Binary    
    
Lt = binary('Lt', '<')
Le = binary('Le', '<=')
Eq = binary('Eq', '==')
Ne = binary('Ne', '!=')
Ge = binary('Ge', '>=')
Gt = binary('Gt', '>')

def append_fail_cont(compiler, exp):
  v, fc = Var('v'), Var('fc1')
  v1 =  compiler.new_var(v)
  fc1 = compiler.new_var(fc)
  return Begin((
    Assign(fc1, failcont),
    SetFailCont(
      clamda(v1, 
                SetFailCont(fc1),
                exp,
                fc1(FALSE)))
    ))

type_map = {int:Integer, float: Float, str:String, unicode: String, tuple: make_tuple, list:List}

def element(exp):
  if isinstance(exp, Element):
    return exp
  else:
    try: 
      return type_map[type(exp)](exp)
    except: 
      raise CompileTypeError(exp)

TRUE = Atom(True)
FALSE = Atom(False)
NONE = Atom(None)
