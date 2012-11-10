from dao.base import classeq

def collocate(defs, exp):
  if defs:
    return Begin(defs + (exp,))
  else: 
    return exp

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
    return getitem(self, index)
  
  def __call__(self, *args):
    return Apply(self, args)
  
  def __lt__(x, y): return lt(x, y)
  def __le__(x, y): return le(x, y)
  def __ge__(x, y): return ge(x, y)
  def __gt__(x, y): return gt(x, y)
  
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
   
class Lamda(Element):
  def __init__(self, params, *body):
    self.params, self.body = params, body
    
  def __call__(self, *args):
    return Apply(self, args)
  
  def alpha_convert(self, env):
    try:
      self.before_alpha_convert
      return self
    except: self.before_alpha_convert  = (self.params, self.body)
    
    new_env = env.extend()
    for p in self.params: 
      new_env.bindings[p] = new_env.new_var(p)
    self.params = tuple(new_env[p] for p in self.params)
    self.body = tuple(new_env.alpha_convert(x) for x in self.body)
    self.variables = new_env.bindings.values()
    self.lefts = new_env.lefts # prepare for assign convert
    return self    
    
  def cps_convert(self, compiler, cont):
    k = Var('k')
    return cont(Lamda(self.params+(k,), compiler.cps_convert_exps(self.body, k)))
  
  def assign_convert(self, alpha_env, env):
    try:
      self.before_assign_convert
      return self
    except: self.assign_convert = (self.params, self.body)
    
    new_env = env.extend()
    for p in self.lefts: 
      new_env.bindings[p] = alpha_env.new_var(p)
    make_cells = tuple((new_env[p], make_cell(p)) for p in self.lefts)
    self.body = (let(make_cells, *tuple(assign_convert(x, alpha_env, new_env) for x in self.body)),)
    return self
    
  def analyse_before_optimize(self, data):
    try: self.seen
    except:
      self.seen = True
      data.occur_count[self] = data.occur_count.setdefault(self, 0)+1
      for x in self.body:
        analyse_before_optimize(x, data)
        
  def code_size(self):
      return code_size(self.body)+len(self.params)+2
    
  def side_effects(self):
      return False
    
  def subst(self, bindings):
    return Lamda(self.params, *subst(self.body, bindings))
      
  def optimize_once(self, data):
    body, changed = optimize_once(self.body, data)
    return Lamda(self.params, *body), changed
    
  def pythonize(self, env):
    body_exps = ()
    body_is_statement = False
    for x in self.body:
      x = pythonize(x, env)
      if is_statement(x):
        body_is_statement = True
      body_exps += (x,)
    if not body_is_statement:
      return Lamda(self.params, *body_exps)
    else:
      return Function(env.new_var(Var('function')), self.params, *body_exps)
        
  def to_code(self, coder):
    head = "lambda %s: " % ', '.join(coder.to_code_list(self.params))
    coder.lambda_stack.append(self)
    result = head + '(%s)'%', '.join(coder.to_code_list(self.body))
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
  def __init__(self, name, params, *body):
    Lamda.__init__(self, params, *body)
    self.name = name
  
  def analyse_before_optimize(self, data):
    data.occur_count[self] = data.occur_count.setdefault(self, 0)+1
    for x in self.body:
      analyse_before_optimize(x, data)
  
  def to_code(self, coder):
    head = "def %s(%s):\n" % (self.name, ', '.join(coder.to_code_list(self.params)))
    coder.lambda_stack.append(self)
    result =  head + coder.indent('\n'.join(coder.to_code_list(self.body)))
    coder.lambda_stack.pop()
    return result
          
class Clamda(Lamda):
  def __init__(self, v, *exps):
    self.params, self.body = (v, ), exps
    self.name = None
    
  def analyse_before_optimize(self, data):
    data.occur_count[self] = data.occur_count.setdefault(self, 0)+1
    for x in self.body:
      analyse_before_optimize(x, data)
    
  def __repr__(self):
    return 'il.Clamda(%r, %r, %s)'%(self.params[0], ', '.join([repr(x) for x in self.body]))

class Done(Clamda):
  def __init__(self):
    v = Var('v')
    self.params, self.body = (v,), (Return(v),)
    self.name = None
    
  def __repr__(self):
    return 'il.Done()'

class CFunction(Clamda):
  is_statement = True
  
  def __init__(self, name, v, *body):
    Clamda.__init__(self,  v, *body)
    self.name = name
    
  def analyse_before_optimize(self, data):
    data.occur_count[self] = data.occur_count.setdefault(self, 0)+1
    for x in self.body:
      analyse_before_optimize(x, data)
    
  def __repr__(self):
    return 'il.CFunction(%r, %r, %s)'%(self.name, self.params[0], ', '.join([repr(x) for x in self.body]))
  
class Apply(Element):
  def __init__(self, caller, args):
    self.caller, self.args = caller, args

  def alpha_convert(self, env):
    return self.__class__(env.alpha_convert(self.caller), 
                 tuple(env.alpha_convert(arg) for arg in self.args))
  
  def cps_convert(self, compiler, cont):
    # see The 90 minute Scheme to C compiler by Marc Feeley
    if isinstance(self.caller, Lamda):
      args = self.args
      fun = compiler.cps_convert_exps(self.caller.body, cont)
      for var, arg in reversed(zip(self.caller.params, args)):
        fun = compiler.cps_convert(arg, Clamda(var, fun), fcont)
      return fun
    else:
      function = Var('function')
      vars = tuple(Var('a'+repr(i)) for i in range(len(self.args)))
      fun = Apply(function, (cont,)+vars)
      for var, self in reversed(zip((function,)+vars, (self.caller,)+self.args)):
        fun = compiler.cps_convert(self, Clamda(var, fun), fcont)
      return fun

  def assign_convert(self, alpha_env, env):
    return self.__class__(assign_convert(self.caller, alpha_env, env), 
                 tuple(assign_convert(arg, alpha_env, env) for arg in self.args))    
    
  def analyse_before_optimize(self, data):  
    data.called_count[self.caller] = data.called_count.setdefault(self.caller, 0)+1
    analyse_before_optimize(self.caller, data)
    for arg in self.args:
      analyse_before_optimize(arg, data)
        
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
    # eliminating free variables(Lambda lifting or closure conversion)
    # http://en.wikipedia.org/wiki/Closure_conversion
    
    #Beta Conversion
    
    #-Conversion primarily consists of the process of substituting a bound variable in the body of a lambda abstraction 
    # by the argument passed to the function whenever it is applied. This process is called -reduction.
    #In the context of functional programming languages, inline expansion is usually followed by the beta-reduction transformation.

    
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
          return optimize(subst(statements(self.caller.body), bindings), data), True
        else:
          return optimize(statements(self.caller.body), data), True               
    else: 
      changed = False
      caller, changed1 = optimize_once(self.caller, data)
      args, changed2 = optimize_once(self.args, data)
      return self.__class__(caller, args), changed1 or changed2

  def pythonize(self, env):
    caller = pythonize(self.caller, env)
    defs = ()
    if isinstance(caller, Function):
      defs += (caller,)
      caller = caller.name
    defs1, args = pythonize_list(self.args, env)
    defs += defs1
    return collocate(defs, self.__class__(caller,args))
    
  def to_code(self, coder):
    if isinstance(self.caller, Lamda):
      return "(%s)"%coder.to_code(self.caller) + '(%s)'%', '.join([coder.to_code(x) for x in self.args])
    else:
      return coder.to_code(self.caller) + '(%s)'%', '.join([coder.to_code(x) for x in self.args])        

  def __eq__(x, y):
    return classeq(x, y) and x.caller==y.caller and x.args==y.args
  
  def __repr__(self):
    return '%r(%s)'%(self.caller, ', '.join([repr(x) for x in self.args]))
  
class Var(Element):
  def __init__(self, name):
    self.name = name
        
  def alpha_convert(self, env):
    return env[self]
    
  def cps_convert(self, compiler, cont):
    return cont(self, fcont)
  
  def cps_convert_unify(x, y, compiler, cont):
    return Begin(
      Deref(x),
      Deref(y),
      If(Isinstance(x, LogicVar),
         Return(Clambda(v, cont(v, 
                    Clamda(v, 
                           SetBindings(x, y),
                           Return(v))))),
         If(Isinstance(y, LogicVar),
            Return(Clambda(v, cont(v, 
                       Clamda(v, 
                              SetBindings(x, y),
                              Return(v))))),
         )))
  
    
  def assign_convert(self, alpha_env, env):
    if self in env:
      return contents(env[self])
    else: return self
    
  def analyse_before_optimize(self, data):
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
      
  def pythonize(self, env):
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
    
class LogicVar(Element):
  def __init__(self, name):
    self.name = name
  
  def __call__(self, args):
    raise TypeError
  
  def alpha_convert(self, env):
    return self
    
  def cps_convert_unify(self, other, cont):
    return Unify(self,other, cont)
  
  def pythonize(self, env):
    return self
      
  def to_code(self, coder):
    return  "LogicVar('%s')"%self.name
    
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  def __repr__(self):
    return self.name # enough in tests
  
class Return(Element):
  
  def __init__(self, *args):
    self.args = args
  
  def alpha_convert(self, env):
    return Return(*tuple(env.alpha_convert(arg) for arg in self.args))    
    
  def assign_convert(self, alpha_env, env):
    return Return(*tuple(assign_convert(arg, alpha_env, env) for arg in self.args))
    
  def analyse_before_optimize(self, data):  
    for arg in self.args:
      analyse_before_optimize(arg, data)
        
  def code_size(self):
    return sum([code_size(x) for x in self.args])

  def side_effects(self):
    return False
        
  def subst(self, bindings):  
    return Return(*tuple(subst(arg, bindings) for arg in self.args))
    
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
    return Return(*result), changed
  
  def pythonize(self, env):
    defs, args = pythonize_list(self.args, env)
    return collocate(defs, Return(*args))
    
  def to_code(self, coder):
    if coder.lambda_stack and isinstance(coder.lambda_stack[-1], Function):
      return  'return %s' % ', '.join([coder.to_code(x) for x in self.args])
    else:
      return  ', '.join([coder.to_code(x) for x in self.args])
    
  def __eq__(x, y):
    return classeq(x, y) and x.args==y.args
  
  def __repr__(self):
    return 'il.Return(%s)'%', '.join([repr(x) for x in self.args])
  
class Assign(Element):
  is_statement = True
  def __init__(self, var, exp):
    self.var, self.exp =  var, exp
  
  def alpha_convert(self, env):
    try: converted_var = env[self.var]
    except VariableNotBound:
      converted_var = env.bindings[self.var] = env.new_var(self.var)
    env.lefts.add(converted_var)
    return Assign(converted_var, env.alpha_convert(self.self))
    
  def assign_convert(self, alpha_env, env):
    # var = value, exp.exp should be a single var, 
    # which is the continuation param which ref to the value    
    return set_contents(env[self.var], self.exp)
  
  def analyse_before_optimize(self, data):  
    analyse_before_optimize(self.exp, data)
      
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
    
  def pythonize(self, env):
    # var = value, self.self should be a single var, 
    # which is the continuation param which ref to the value
    return self
    
  def to_code(self, coder):
    return  '%s = %s' % (coder.to_code(self.var), coder.to_code(self.exp))
    
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.exp==y.exp
  
  def __repr__(self):
    return 'il.Assign(%r, %r)'%(self.var, self.exp)
  
class If(Element):
  def __init__(self, test, then, else_):
    self.test, self.then, self.else_ = test, then, else_
    
  def alpha_convert(self, env):
    return If(env.alpha_convert(self.test), env.alpha_convert(self.then), 
                 env.alpha_convert(self.else_))

  def assign_convert(self, alpha_env, env):
    return If(assign_convert(self.test, alpha_env, env), 
                 assign_convert(self.then, alpha_env, env), 
                 assign_convert(self.else_, alpha_env, env))
    
  def analyse_before_optimize(self, data):  
    analyse_before_optimize(self.test, data)
    analyse_before_optimize(self.then, data)
    analyse_before_optimize(self.else_, data)
    
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
    test, test_changed = optimize_once(self.test, data)
    then, then_changed = optimize_once(self.then, data)
    else_, else__changed = optimize_once(self.else_, data)
    result = If(test, then, else_)
    #if isinstance(result.test, Let):
      #result = Let(result.bindings, If(Begin(let.body), 
                                             #result.then, result.else_))
    return result, changed or test_changed or then_changed or else__changed

  def pythonize(self, env):
    defs, (test, then, else_) = pythonize_list((self.test, self.then, self.else_), env)
    return collocate(defs, If(test, then, else_))
    
  def to_code(self, coder):
    if coder.lambda_stack and isinstance(coder.lambda_stack[-1], Function):
      return 'if %s: \n%s\nelse:\n%s' % (coder.to_code(self.test), coder.indent(coder.to_code(self.then)), 
                                       coder.indent(coder.to_code(self.else_)))        
    else:
      return '%s if %s else %s' % (coder.to_code(self.then), coder.to_code(self.test), 
                                       coder.to_code(self.else_))        
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
    
  def alpha_convert(self, env):
    return If2(env.alpha_convert(self.test), env.alpha_convert(self.then))
    
  def assign_convert(self, alpha_env, env):
    return If(assign_convert(self.test, alpha_env, env), 
                 assign_convert(self.then, alpha_env, env))
    
  def analyse_before_optimize(self, data):  
    analyse_before_optimize(self.test, data)
    analyse_before_optimize(self.then, data)
    
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
    
  def pythonize(self, env):
    defs, (test, then) = pythonize_list((self.test, self.then), env)
    return collocate(defs, If2(test, then))
    
  def to_code(self, coder):
    return 'if %s: \n%s\n' % (coder.to_code(self.test), coder.indent(coder.to_code(self.then)))

  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.then==y.then
  
  def __repr__(self):
    return 'il.If2(%r, %r)'%(self.test, self.then)

class Unify(Element):
  def __init__(self, left, right, cont):
    self.left, self.right, self.cont, self.fcont =  left, right, cont,fcont
    
  def alpha_convert(self, env):
    return Unify(env.alpha_convert(self.left), env.alpha_convert(self.right),
                 env.alpha_convert(self.cont), env.alpha_convert(self.fcont))
    
  def assign_convert(self, alpha_env, env):
    return Unify(assign_convert(self.left, alpha_env, env), assign_convert(self.right, alpha_env, env),
                 assign_convert(self.cont, alpha_env, env), assign_convert(self.fcont, alpha_env, env))
    
  def analyse_before_optimize(self, data):  
    analyse_before_optimize(self.left, data)
    analyse_before_optimize(self.right, data)
    analyse_before_optimize(self.cont, data)
    analyse_before_optimize(self.fcont, data)

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
    
  def pythonize(self, env):
    defs, (left, right, cont) = pythonize_list((self.left, self.right, self.cont, self.fcont), env)
    return collocate(defs, Unify(left, right, cont))
    
  def to_code(self, coder):
    return 'unify(%s, %s, %s, %s)' % (coder.to_code(self.left), coder.to_code(self.right), 
                                     coder.to_code(self.cont), coder.to_code(self.fcont))
    
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

  def to_code(self, coder):
    return '%s%s%s'%(coder.to_code(self.args[0]), 
                        coder.to_code(self.caller), 
                        coder.to_code(self.args[1]))
    
class BinaryOperation(Element):
  def __init__(self, name, operator, have_side_effects=True):
    self.name, self.operator = name, operator
    self.have_side_effects = have_side_effects
  
  def alpha_convert(self, env):
    return self
  
  def analyse_before_optimize(self, data):  
    return self
  
  def subst(self, bindings):  
    return self

  def optimize_once(self, data):
    return self, False
    
  def pythonize(self, env):
    return self
    
  def to_code(self, coder):
    return self.operator
      
  def __call__(self, args):
    return BinaryOperationApply(self, args)
  
  def __eq__(x, y):
      return classeq(x, y) and x.operator==y.operator
    
  def __hash__(self): return hash(self.operator)
  
  def __repr__(self):
    return '%s'%self.name

add = BinaryOperation('add', '+', False)

class Begin(Element):
  is_statement = True
  
  def __init__(self, statements):
    self.statements = statements
    
  def subst(self, bindings):  
    return Begin(tuple(subst(x, bindings) for x in self.statements))
          
  def optimize_once(self, data):
    changed = False
    result = []
    for x in self.statements:
      x, x_changed = optimize_once(x, data)
      result.append(x)
      changed = changed or x_changed
    return statements(tuple(result)), changed
        
  def to_code(self, coder):
    return  '\n'.join([coder.to_code(x) for x in self.statements])
      
  def __eq__(x, y):
      return classeq(x, y) and x.statements==y.statements
  
  def __repr__(self):
    return 'il.begin(%s)'%', '.join([repr(x) for x in self.statements])

def begin(*exps):
  assert isinstance(exps, tuple)
  if len(exps)==1: 
    return exps[0]
  else:
    return Begin(exps)
  
def vop(name, arity):
  class Vop(Element): pass
  Vop.__name__ = name
  Vop.arity = arity
  return Vop

class GetItem(Element):
  arity = 2
  
  def __repr__(self):
    return '%r[%r]'%(self.args)
  
Not = vop('Not', 1)
Len = vop('Len', 1)
SetParseState = vop('SetParseState', 1)
AssignFromList = vop('AssignFromList', -1)
ListAppend = vop('ListAppend', 2)
GetValue = vop('GetValue', 1)
SetFailCont = vop('SetFailCont', 1)
AppendFailCont = vop('AppendFailCont', -1) 
'''il.Assign(fc, get_failcont)
  SetFailCont(
    Clambda(v, 
      SetFailCont(fc),
      statements
  ))'''  

GetParseState = vop('parse_state', 0)
parse_state = GetParseState()
EmptyList = vop('empty_list', 0)
empty_list = EmptyList()
GetFailCont = vop('failcont', 0)  
failcont = GetFailCont()


def binary(name, symbol):
  class Binary(Element): 
    def __repr__(self):
      return '(%s%s%s)'%(repr(self.args[0]), self.__class__.symbol, repr(self.args[1]))
  Binary.__name__ = name
  Binary.arity = 2
  Binary.symbol = symbol
  return Binary    
    
Lt = binary('lt', '<')
Le = binary('le', '<=')
Eq = vop('eq', 2)
Ne = vop('ne', 2)
Ge = binary('ge', '>=')
Gt = binary('gt', '>')


