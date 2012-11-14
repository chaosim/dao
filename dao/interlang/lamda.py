from dao.base import classeq

from dao.compilebase import alpha_convert, cps_convert, assign_convert, find_assign_lefts
from dao.compilebase import optimization_analisys, optimize_once
from dao.compilebase import side_effects, optimize, subst, code_size, MAX_EXTEND_CODE_SIZE
from dao.compilebase import insert_return_yield, pythonize, to_code, to_code_list
from dao.compilebase import VariableNotBound, CompileTypeError

from elements import Element, Var, begin

import vop

from utility import pythonize_list

def cps_convert_exps(compiler, exps, cont):
  v = Var('v')
  if not exps: return Clamda(v, cont(il.tuple()))
  if len(exps)==1:
    return cps_convert(compiler, exps[0], cont)
  else:
    return cps_convert(compiler, exps[0], Clamda(v, cps_convert_exps(compiler, exps[1:], cont)))

def let(bindings, *body):
  params = tuple(p for p, _ in bindings)
  args = tuple(a for _, a in bindings)
  return Lamda(params, body)(*args)

class Lamda(Element):
  def __init__(self, params, *body):
    self.params, self.body = params, body
    
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
    return cont(Lamda(self.params+(k,), cps_convert_exps(compiler, self.body, k)))
  
  def find_assign_lefts(self):
    #todo
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
      make_cells = tuple((env[p], vop.MakeCell(p)) for p in lefts)
      self.body = let(make_cells, *tuple(assign_convert(x, env, compiler) for x in self.body))
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
    return Lamda(self.params, *subst(self.body, bindings))
      
  def optimize_once(self, data):
    body, changed = optimize_once(self.body, data)
    return Lamda(self.params, *body), changed
  
  def pythonize_lambda_body(self, env, compiler):
    body_exps = ()
    body_is_statement = False
    for x in self.body:
      x = pythonize(x, env, compiler)
      if is_statement(x):
        body_is_statement = True
      body_exps += (x,) 
    return body_exps, body_is_statement
  
  def pythonize(self, env, compiler):
    body_exps, body_is_statement = self.pythonize_lambda_body(env, compiler)
    if not body_is_statement:
      return Lamda(self.params, *body_exps)
    else:
      return Function(compiler.new_var(Var('function')), self.params, *body_exps)
        
  def to_code(self, coder):
    head = "lambda %s: " % ', '.join(to_code_list(coder, self.params))
    coder.lambda_stack.append(self)
    if len(self.body)==1:
      result = head + '%s'%', '.join(to_code_list(coder, self.body))
    else:
      result = head + '(%s)'%', '.join(to_code_list(coder, self.body))
    coder.lambda_stack.pop()
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
  
  def optimization_analisys(self, data):
    data.occur_count[self] = data.occur_count.setdefault(self, 0)+1
    for x in self.body:
      optimization_analisys(x, data)
  
  def to_code(self, coder):
    head = "def %s(%s):\n" % (self.name, ', '.join(to_code_list(coder, self.params)))
    coder.lambda_stack.append(self)
    result =  head + coder.indent('\n'.join(to_code_list(coder, self.body)))
    coder.lambda_stack.pop()
    return result
          
class Clamda(Lamda):
  def __init__(self, v, *exps):
    self.params, self.body = (v, ), exps
    self.name = None
    
  def optimization_analisys(self, data):
    data.occur_count[self] = data.occur_count.setdefault(self, 0)+1
    for x in self.body:
      optimization_analisys(x, data)
    
  def __repr__(self):
    return 'il.Clamda(%r, %s)'%(self.params[0], ', '.join([repr(x) for x in self.body]))

class Done(Clamda):
  def __init__(self):
    v = Var('v')
    self.params, self.body = (v,), (v,)
    
  def __call__(self, *args):
    if isinstance(self.body, tuple):
      body = begin(*subst(self.body, {self.params[0]:args[0]}))
    else:
      body = begin(subst(self.body, {self.params[0]:args[0]}))
    return body
  
  def __repr__(self):
    return 'il.Done(%r, %s)'%(self.params[0], ', '.join([repr(x) for x in self.body]))

class CFunction(Clamda):
  is_statement = True
  is_function = True
  
  def __init__(self, name, v, *body):
    Clamda.__init__(self,  v, *body)
    self.name = name
    
  def optimization_analisys(self, data):
    data.occur_count[self] = data.occur_count.setdefault(self, 0)+1
    for x in self.body:
      optimization_analisys(x, data)
    
  def pythonize_function_body(self, env, compiler):
    body_exps = ()
    for x in self.body:
      x = pythonize(x, env, compiler)
      body_exps += (x,) 
    return body_exps
  
  def pythonize(self, env, compiler):
    body_exps = self.pythonize_function_body(env, compiler)
    return Function(self.name, self.params, *body_exps)
    
  def __repr__(self):
    return 'il.CFunction(%r, %r, %s)'%(self.name, self.params[0], ', '.join([repr(x) for x in self.body]))
  
class Apply:
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
      if len(self.caller.params)==0: 
        return optimize(statements(self.caller.body), data), True
      
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
          return Apply(Lamda(new_params, optimize(subst(self.caller.body, bindings), data)), 
                          optimize(new_args, data)), True
        else:
          if len(new_params)!=len(self.caller.params):
            Apply(Lamda(new_params, optimize(self.caller.body, data)), optimize(new_args, data)), True            
          else:
            caller_body, changed1 = optimize_once(self.caller.body, data)
            args, changed2 = optimize_once(new_args, data)
            return Apply(Lamda(new_params, caller_body), args), changed1 or changed2
      else:
        if bindings:
          return optimize(subst(begin(*self.caller.body), bindings), data), True
        else:
          return optimize(begin(*self.caller.body), data), True               
    else: 
      changed = False
      caller, changed1 = optimize_once(self.caller, data)
      args, changed2 = optimize_once(self.args, data)
      return self.__class__(caller, args), changed1 or changed2

  def insert_return_yield(self, klass):
    return klass(self)
  
  def pythonize(self, env, compiler):
    caller = pythonize(self.caller, env, compiler)
    defs = ()
    if isinstance(caller, Function):
      defs += (caller,)
      caller = caller.name
    defs1, args = pythonize_list(self.args, env, compiler)
    defs += defs1
    return collocate(defs, self.__class__(caller,args))
    
  def to_code(self, coder):
    if isinstance(self.caller, Lamda):
      return "(%s)"%to_code(coder, self.caller) + '(%s)'%', '.join([to_code(coder, x) for x in self.args])
    else:
      return to_code(coder, self.caller) + '(%s)'%', '.join([to_code(coder, x) for x in self.args])        

  def __eq__(x, y):
    return classeq(x, y) and x.caller==y.caller and x.args==y.args
  
  def __repr__(self):
    return '%r(%s)'%(self.caller, ', '.join([repr(x) for x in self.args]))