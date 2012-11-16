from dao.base import classeq

from dao.compilebase import alpha_convert, cps_convert, assign_convert, find_assign_lefts
from dao.compilebase import optimization_analisys, optimize_once
from dao.compilebase import side_effects, optimize, subst, code_size, MAX_EXTEND_CODE_SIZE
from dao.compilebase import insert_return_yield, pythonize_exp, to_code, to_code_list
from dao.compilebase import VariableNotBound, CompileTypeError

def is_statement(exp):
  try: return exp.is_statement
  except:
    if isinstance(exp, list) or isinstance(exp, tuple) or\
       isinstance(exp, int) or isinstance(exp, float) or\
       isinstance(exp, str) or isinstance(exp, unicode):
      return False
  raise CompileTypeError(exp)

def pythonize(exp, env, compiler):
  exps, has_any_statement = pythonize_exp(exp, env, compiler)
  return begin(*exps)

def pythonize_exps(exps, env, compiler):
  result = ()
  has_any_statement = False
  for exp in exps:
    exps2, any_statement = pythonize_exp(exp, env, compiler)
    has_any_statement = has_any_statement or any_statement
    result += exps2
  return result, has_any_statement

def python_args(args, env, compiler):
  # used in Apply, Return, Yield, VirtualOpteration
  result = []
  exps = ()
  has_statement = False
  for arg in args:
    exps2, has_statement1 = pythonize_exp(arg, env, compiler)
    has_statement = has_statement or has_statement1
    result.append(exps2[-1])
    exps += exps2[:-1]
  return exps, result, has_statement
    
def cps_convert_exps(compiler, exps, cont):
  v = Var('v')
  if not exps: return Clamda(v, cont(il.tuple()))
  if len(exps)==1:
    return cps_convert(compiler, exps[0], cont)
  else:
    return cps_convert(compiler, exps[0], Clamda(v, cps_convert_exps(compiler, exps[1:], cont)))

class Element:
  have_side_effects = True
  is_statement = False
  
  def __init__(self, *args):
    if self.arity>=0:
      assert len(args)==self.arity, \
           '%s should have %s arguments.'%(
             self.__class__.__name__, self.__class__.arity)
    self.args = args
      
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    try: 
      if self.arity==0: 
        return 'il.%s'%self.__class__.__name__
    except: pass
    return 'il.%s(%s)'%(self.__class__.__name__, 
              ', '.join([repr(x) for x in self.args]))
   

def let(bindings, *body):
  params = tuple(p for p, _ in bindings)
  args = tuple(a for _, a in bindings)
  return Lamda(params, *body)(*args)

class Lamda(Element):
  def __init__(self, params, *body):
    self.params, self.body = params, body
    
  def new(self, params, body):
    return self.__class__(params, *body)
  
  def __call__(self, *args):
    return Apply(self, args)
  
  def alpha_convert(self, env, compiler):
    try:
      self.before_alpha_convert
      return self
    except: self.before_alpha_convert  = self.params, self.body
    
    new_env = env.extend()
    for p in self.params: 
      new_env.bindings[p] = compiler.new_var(p)
    self.params = tuple(new_env[p] for p in self.params)
    self.body = tuple(alpha_convert(x, new_env, compiler) for x in self.body)
    self.variables = new_env.bindings.values()
    return self
    
  def cps_convert(self, compiler, cont):
    k = compiler.new_var(Var('cont'))
    return cont(self.new(self.params+(k,), cps_convert_exps(compiler, self.body, k)))
  
  def find_assign_lefts(self):
    result = set()
    for exp in self.body:
      result |= find_assign_lefts(exp)
    return result
  
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
      self.body = (let(make_cells, *tuple(assign_convert(x, env, compiler) for x in self.body)),)
    else:
      self.body = tuple(assign_convert(x, env, compiler) for x in self.body)
    return self
    
  def optimization_analisys(self, data):
    try: self.seen
    except:
      self.seen = True
      data.occur_count[self] = data.occur_count.setdefault(self, 0)+1
      for x in self.body:
        optimization_analisys(x, data)
        
  def code_size(self):
      return code_size(self.body)+len(self.params)+2
    
  def side_effects(self):
      return False
    
  def subst(self, bindings):
    return self.new(self.params, subst(self.body, bindings))
      
  def optimize_once(self, data):
    body, changed = optimize_once(self.body, data)
    return self.new(self.params, body), changed
  
  def pythonize_exp(self, env, compiler):
    body_exps, body_has_any_statement = pythonize_exps(self.body, env, compiler)
    if not body_has_any_statement:
      return (self.new(self.params, body_exps),), False
    else:
      if not is_statement(body_exps[-1]):
        body_exps = body_exps[:-1]+(Return(body_exps[-1]),)
      name = compiler.new_var(Var('function'))
      return (Function(name, self.params, *body_exps), name), True
        
  def to_code(self, coder):
    head = "lambda %s: " % ', '.join(to_code_list(coder, self.params))
    if len(self.body)==1:
      result = head + '%s'%', '.join(to_code_list(coder, self.body))
    else:
      result = head + '(%s)'%', '.join(to_code_list(coder, self.body))
    return result
  
  def __eq__(x, y):
    return classeq(x, y) and x.params==y.params and x.body==y.body
  
  def __hash__(self): return hash(id(self))
  
  def __repr__(self):
    return 'il.Lamda((%s), %s)'%(', '.join([repr(x) for x in self.params]),
                              ', '.join([repr(x) for x in self.body]))

class Function(Lamda):
  is_statement = True
  is_function = True
  
  def __init__(self, name, params, *body):
    Lamda.__init__(self, params, *body)
    self.name = name
  
  def new(self, params, body):
    return self.__class__(self.name, params, *body)
  
  def pythonize_exp(self, env, compiler):
    body_exps, has_any_statement = pythonize_exps(self.body, env, compiler)
    if not is_statement(body_exps[-1]):
      body_exps = body_exps[:-1] + (Return(body_exps[-1]),)
    return (self.new(self.params, body_exps), self.name), True
    
  def to_code(self, coder):
    head = "def %s(%s):\n" % (self.name, ', '.join(to_code_list(coder, self.params)))
    result =  head + coder.indent('\n'.join(to_code_list(coder, self.body)))
    return result
  
  def __repr__(self):
    return 'il.Function(%s, (%s), %s)'%(self.name, ', '.join([repr(x) for x in self.params]),
                                  ', '.join([repr(x) for x in self.body]))    
          
class Clamda(Lamda):
  def __init__(self, v, *exps):
    self.params, self.body = (v, ), exps
    self.name = None
    
  def new(self, params, body):
    return self.__class__(params[0], *body)
  
  def __repr__(self):
    return 'il.Clamda(%r, %s)'%(self.params[0], ', '.join([repr(x) for x in self.body]))

class Done(Clamda):
  def __init__(self):
    v = Var('v')
    self.params, self.body = (v,), (v,)
    
  def new(self, params, body):
    return self.__class__()
  
  def __call__(self, *args):
    if isinstance(self.body, tuple):
      body = begin(*subst(self.body, {self.params[0]:args[0]}))
    else:
      body = begin(subst(self.body, {self.params[0]:args[0]}))
    return body
  
  def __repr__(self):
    return 'il.Done(%r, %s)'%(self.params[0], ', '.join([repr(x) for x in self.body]))

class CFunction(Function):
  is_statement = True
  is_function = True
  
  def __init__(self, name, v, *body):
    Function.__init__(self,  name, (v,), *body)
    
  def new(self, params, body):
    return self.__class__(self.name, params[0], *body)
  
  def __repr__(self):
    return 'il.CFunction(%r, %r, %s)'%(self.name, self.params[0], ', '.join([repr(x) for x in self.body]))
  
class Apply:
  is_statement = False
  
  def __init__(self, caller, args):
    self.caller, self.args = caller, args

  def alpha_convert(self, env, compiler):
    return self.__class__(alpha_convert(self.caller, env, compiler), 
                 tuple(alpha_convert(arg, env, compiler) for arg in self.args))
  
  def cps_convert(self, compiler, cont):
    # see The 90 minute Scheme to C compiler by Marc Feeley
    if isinstance(self.caller, Lamda):
      args = self.args
      fun = cps_convert_exps(compiler, self.caller.body, cont)
      for var, arg in reversed(zip(self.caller.params, args)):
        fun = cps_convert(compiler, arg, Clamda(var, fun))
      return fun
    else:
      function = Var('function')
      vars = tuple(Var('a'+repr(i)) for i in range(len(self.args)))
      fun = Apply(function, (cont,)+vars)
      for var, self in reversed(zip((function,)+vars, (self.caller,)+self.args)):
        fun = cps_convert(compiler, self, Clamda(var, fun))
      return fun

  def assign_convert(self, env, compiler):
    return self.__class__(assign_convert(self.caller, env, compiler), 
                 tuple(assign_convert(arg, env, compiler) for arg in self.args))    
    
  def find_assign_lefts(exp):
    return set()
  
  def optimization_analisys(self, data):  
    data.called_count[self.caller] = data.called_count.setdefault(self.caller, 0)+1
    optimization_analisys(self.caller, data)
    for arg in self.args:
      optimization_analisys(arg, data)
        
  def code_size(self):
    return code_size(self.caller)+sum([code_size(x) for x in self.args])
              
  def side_effects(self):
    if isinstance(self.caller, Lamda):
      if lambda_side_effects(self.caller): return True
    elif isinstance(self.caller, Var): return True
    elif self.caller.have_side_effects: return True
    else: return False # after cps, all of value have been solved before called, 
                       # so have no side effects.
          
  def subst(self, bindings):  
    return self.__class__(subst(self.caller, bindings), 
                 tuple(subst(arg, bindings) for arg in self.args))
      
  def optimize_once(self, data):    
    if isinstance(self.caller, Lamda):
      #1. ((lambda () body))  =>  body 
      if len(self.caller.params)==0 and not isinstance(Lamda, Function): 
        return optimize(begin(*self.caller.body), data), True
      
      #2. (lamda x: ...x...)(y) => (lambda : ... y ...)() 
      bindings = {}
      args = self.args
      new_params, new_args = (), ()
      for i, p in enumerate(self.caller.params):
        arg = args[i]
        if side_effects(arg):
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
            if code_size(arg)*ref_count>MAX_EXTEND_CODE_SIZE: 
              # a(...y...), and a is (lamda ...x...: ...x...), 
              #then convert as above if code size is ok. 
              new_params += (p,)
              new_args += (arg,)
            else: 
              bindings[p] = arg
      
      if new_params:
        if bindings:
          return Apply(self.caller.new(new_params, (optimize(subst(begin(*self.caller.body), bindings), data),)), 
                          optimize(new_args, data)), True
        else:
          if len(new_params)!=len(self.caller.params):
            Apply(self.caller.new(new_params, (optimize(begin(*self.caller.body), data), )), optimize(new_args, data)), True            
          else:
            caller_body, changed1 = optimize_once(self.caller.body, data)
            args, changed2 = optimize_once(new_args, data)
            return Apply(self.caller.new(new_params, caller_body), args), changed1 or changed2
      else:
        if not isinstance(self.caller, Function):
          if bindings:
            return optimize(subst(begin(*self.caller.body), bindings), data), True
          else:
            return optimize(begin(*self.caller.body), data), True
        else:
          if bindings:
            return Function(self.caller.name, (), optimize(subst(begin(*self.caller.body), bindings), data))(), True
          else:
            return Function(self.caller.name, (), optimize(begin(*self.caller.body), data))(), True
          
    else: 
      changed = False
      caller, changed1 = optimize_once(self.caller, data)
      args, changed2 = optimize_once(self.args, data)
      return self.__class__(caller, args), changed1 or changed2

  def insert_return_yield(self, klass):
    return klass(self)
  
  def pythonize_exp(self, env, compiler):
    exps, has_statement = pythonize_exp(self.caller, env, compiler)
    caller = exps[-1]
    exps = exps[:-1]
    exps2, args, has_statement2 = python_args(self.args, env, compiler)
    return exps+exps2+(self.__class__(caller,args),), has_statement or has_statement2
    
  def to_code(self, coder):
    if isinstance(self.caller, Lamda):
      return "(%s)"%to_code(coder, self.caller) + '(%s)'%', '.join([to_code(coder, x) for x in self.args])
    else:
      return to_code(coder, self.caller) + '(%s)'%', '.join([to_code(coder, x) for x in self.args])        

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
                 cont(True)),
                If(Eq(x1, y), cont(True), failcont(True))))
    x1 = compiler.new_var(Var('x'))
    y1 = compiler.new_var(Var('y'))
    return begin(
      Assign(x1, Deref(x)), #for LogicVar, could be optimized when generate code.
      Assign(y1, Deref(y)),
      If(IsLogicVar(x1),
         begin(SetBinding(x1, y1),
               append_fail_cont(compiler, DelBinding(x1)),
               cont(True)),
         begin(
           If(IsLogicVar(y1),
              begin(SetBinding(y1, x1),
                    append_fail_cont(compiler, DelBinding(y1)),
                    cont(True)),
              If(Eq(x1, y1), cont(True), failcont(True))))))

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
    return Assign(converted_var, alpha_convert(self.exp, env, compiler))
    
  def assign_convert(self, env, compiler):
    return SetContent(env[self.var], assign_convert(self.exp, env, compiler))
  
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
    
  def pythonize_exp(self, env, compiler):
    exps, has_statement = pythonize_exp(self.exp, env, compiler)
    return exps[:-1]+(Assign(self.var, exps[-1]),), True
    
  def to_code(self, coder):
    return  '%s = %s' % (to_code(coder, self.var), to_code(coder, self.exp))
    
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.exp==y.exp
  
  def __repr__(self):
    return 'il.Assign(%r, %r)'%(self.var, self.exp)
  
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
  
  def pythonize_exp(self, env, compiler):
    exps, args, has_statement = python_args(self.args, env, compiler)
    return exps+(self.__class__(*args),), True
    
  def to_code(self, coder):
    return  'return %s' % ', '.join([to_code(coder, x) for x in self.args])
  
  def insert_return_yield(self, klass):
    return self
  
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    return 'il.Return(%s)'%', '.join([repr(x) for x in self.args])

class Yield(Return): 
  def to_code(self, coder):
    return  'yield %s' % ', '.join([to_code(coder, x) for x in self.args])

  def __repr__(self):
    return 'il.Yield(%s)'%', '.join([repr(x) for x in self.args])

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
  
  def pythonize_exp(self, env, compiler):
    test, has_statement1 = pythonize_exp(self.test, env, compiler)
    then, has_statement2 = pythonize_exp(self.then, env, compiler)
    else_, has_statement3 = pythonize_exp(self.else_, env, compiler)
    if_ = If(test[-1], begin(*then), begin(*else_))
    if_.is_statement = has_statement2 or has_statement3
    return test[:-1]+(if_,), has_statement1 or has_statement2 or has_statement3
    
  def to_code(self, coder):
    if self.is_statement:
      return 'if %s: \n%s\nelse:\n%s' % (to_code(coder, self.test), 
                                         coder.indent(to_code(coder, self.then)), 
                                       coder.indent(to_code(coder, self.else_)))        
    else:
      return '%s if %s else %s' % (to_code(coder, self.then), 
                                   to_code(coder, self.test), 
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
    
  def optimize_once(self, data):
    return self, False
  
  def code_size(self):
    return 3 + code_size(self.test) + \
           code_size(self.then_)
  
  def side_effects(self):
    return not side_effects(self.test) and\
           not side_effects(self.then_)
  
  def subst(self, bindings):  
    return If2(subst(self.test, bindings), 
                 subst(self.then, bindings))
    
  #def optimize_once(exp, data):
      #return If2(optimize(exp.test, data), optimize(exp.then, data))
    
  def pythonize_exp(self, env, compiler):
    test, has_statement1 = pythonize_exp(self.test, env, compiler)
    then, has_statement2 = pythonize_exp(self.then, env, compiler)
    if_ = If2(test[-1], begin(*then))
    return test[:-1]+(if_,), True
    
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
    
  def pythonize_exp(self, env, compiler):
    raise Todo_Unify_pythonize_exp
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

class Begin(Element):
  is_statement = True
  
  def __init__(self, statements):
    self.statements = statements
    
  def alpha_convert(self, env):
    return Begin(tuple([alpha_convert(x, env, compiler) for x in self.statements]))
  
  def assign_convert(self, env, compiler):
    return Begin(tuple(assign_convert(x, env, compiler) for x in self.statements))
  
  def find_assign_lefts(self):
    result = set()
    for exp in self.statements:
      result |= find_assign_lefts(exp)
    return result
  
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
    return begin(*tuple(result)), changed
        
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
  
  def pythonize_exp(self, env, compiler):
    return pythonize_exps(self.statements, env, compiler)
  
  def to_code(self, coder):
    return  '\n'.join([to_code(coder, x) for x in self.statements])
      
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
    
  def pythonize_exp(self, env, compiler):
    return (self,), False
    
  def to_code(self, coder):
    return self.operator
      
  def __call__(self, args):
    return BinaryOperationApply(self, args)
  
  def __eq__(x, y):
      return classeq(x, y) and x.operator==y.operator
    
  def __hash__(self): return hash(self.operator)
  
  def __repr__(self):
    return 'il.%s'%self.name

add = BinaryOperation('add', '+', False)

class BinaryOperationApply(Apply):
  is_statement = False
  
  def __init__(self, caller, args):
    self.caller, self.args = caller, args

  def alpha_convert(self, env, compiler):
    return self.__class__(alpha_convert(self.caller, env, compiler), 
                 tuple(alpha_convert(arg, env, compiler) for arg in self.args))
  
  def assign_convert(self, env, compiler):
    return self.__class__(assign_convert(self.caller, env, compiler), 
                 tuple(assign_convert(arg, env, compiler) for arg in self.args))    
    
  def optimization_analisys(self, data):  
    data.called_count[self.caller] = data.called_count.setdefault(self.caller, 0)+1
    optimization_analisys(self.caller, data)
    for arg in self.args:
      optimization_analisys(arg, data)
        
  def code_size(self):
    return code_size(self.caller)+sum([code_size(x) for x in self.args])
              
  def side_effects(self):
    if isinstance(self.caller, Var): return True
    elif self.caller.have_side_effects: return True
    else: return False # after cps, all of value have been solved before called, 
                       # so have no side effects.
          
  def subst(self, bindings):  
    return self.__class__(subst(self.caller, bindings), 
                 tuple(subst(arg, bindings) for arg in self.args))
      
  def optimize_once(self, data):    
    changed = False
    caller, changed1 = optimize_once(self.caller, data)
    args, changed2 = optimize_once(self.args, data)
    return self.__class__(caller, args), changed1 or changed2

  def insert_return_yield(self, klass):
    return klass(self)
  
  def pythonize_exp(self, env, compiler):
    exps, args, has_statement = python_args(self.args, env, compiler)
    return exps+(self.__class__(self.caller, args),), has_statement
  
  def to_code(self, coder):
    return '%s%s%s'%(to_code(coder, self.args[0]), 
                        to_code(coder, self.caller), 
                        to_code(coder, self.args[1]))
    
  def __repr__(self):
    return '%r(%r)'%(self.caller, self.args)

class VirtualOperation(Element):
  def __call__(self, *args):
    return Apply(self, args)

  def alpha_convert(self, env, compiler):
    return self
  
  def assign_convert(self, env, compiler):
    return self.__class__(*tuple(assign_convert(arg, env, compiler) for arg in self.args))
  
  def find_assign_lefts(self):
    return set()
  
  def side_effects(self):
    return True

  def optimization_analisys(self, data):
    for arg in self.args:
      optimization_analisys(arg, data)
  
  def subst(self, bindings):  
    return self.__class__(*tuple(subst(x, bindings) for x in self.args))

  def optimize_once(self, data):
    return self, False
  
  def insert_return_yield(self, klass):
    return klass(self)
  
  def pythonize_exp(self, env, compiler):
    exps, args, has_statement = python_args(self.args, env, compiler)
    try: self_is_statement = self.is_statement
    except: self_is_statement = False
    return exps+(self.__class__(*args),), self_is_statement or has_statement
  
  def to_code(self, coder):
    if isinstance(self.__class__.code_format, str):
      if self.__class__.arity==0:
        return self.__class__.code_format
      elif self.__class__.arity!=-1:
        return self.__class__.code_format % tuple(to_code(coder, x) for x in self.args)
      else:
        return self.__class__.code_format % (', '.join([to_code(coder, x) for x in self.args]))
    else: 
      return self.__class__.code_format(self, coder)
      
  def __hash__(self):
    return hash(self.__class__.__name__)

def vop(name, arity, code_format):
  class Vop(VirtualOperation): pass
  Vop.__name__ = name
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
  return "%s = %s" % (', '.join([to_code(coder, x) for x in self.args[:-1]]), 
                      to_code(coder, self.args[-1]))
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

#def AppendFailCont_code_format(self, args, coder):
  #fc = coder.newvar('old_fail_cont')
  #new_fail_cont = coder.newvar('new_fail_cont')
  #result = "%s = solver.fail_cont\n" % fc
  #result += "def %s(v):\n"%new_fail_cont
  #result += coder.indent_space+"solver.fail_cont = %s\n"%fc
  #for stmt in args:
    #result += coder.indent_space+to_code(coder, stmt)+'\n'
  #result += 'solver.fail_cont = %s' % new_fail_cont
  #return result
#AppendFailCont = vop('AppendFailCont', -1, AppendFailCont_code_format) 
#'''il.Assign(fc, get_failcont)
  #SetFailCont(
    #Clambda(v, 
      #SetFailCont(fc),
      #statements
  #))'''  
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
  return '(%s) %s (%s)'%(to_code(coder, self.args[0]), 
                         self.symbol, 
                         to_code(coder, self.args[1]))

def binary(name, symbol):
  class Binary(VirtualOperation): 
    def __repr__(self):
      return '(%s%s%s)'%(repr(self.args[0]), self.__class__.symbol, repr(self.args[1]))
  Binary.__name__ = name
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
      Clamda(v1, 
                #SetFailCont(fc1),
                exp,
                fc1(False)))
    ))