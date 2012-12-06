from dao.base import classeq

from dao.compilebase import MAX_EXTEND_CODE_SIZE, to_code_list, lambda_side_effects
from dao.compilebase import VariableNotBound, CompileTypeError

from element import pythonize_args, optimize_args
from element import Element, element, begin, Return, Begin
from element import NONE, unknown, make_tuple, Tuple, Atom

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
      
  def optimize(self, data):
    return self.new(self.params, self.body.optimize(data))
  
  def optimize(self, data):
    old_assign_bindings = data.assign_bindings.copy()
    result = self.new(self.params, self.body.optimize(data))
    data.assign_bindings = old_assign_bindings
    return result
  
  def optimize_apply(self, data, args):
    #1. ((lambda () body))  =>  body 
    if len(self.params)==0:
      return self.body.optimize(data)
    
    #2. (lamda x: ...x...)(y) => (lambda : ... y ...)() 
    bindings = {}
    new_params, new_args = (), ()
    for i, p in enumerate(self.params):
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
        return Apply(self.new(new_params, self.body.subst(bindings).optimize(data)), 
                        tuple(arg.optimize(data) for arg in new_args))
      else:
        if len(new_params)!=len(self.params):
          Apply(self.new(new_params, self.body.subst(bindings).optimize(data)), 
                tuple(arg.optimize(data) for arg in new_args))   
        else:
          return Apply(self.new(new_params, self.body.optimize(data)), optimize_args(new_args, data))
    else:
      if bindings:
        return self.body.subst(bindings).optimize(data)
      else:
        return self.body.optimize(data)
  
  def insert_return_statement(self):
    return Return(self)
  
  def pythonize(self, env, compiler):
    body_exps, body_has_any_statement = self.body.pythonize(env, compiler)
    global_vars = self.find_assign_lefts()-set(self.params)
    global_vars = set([x for x in global_vars 
                       if isinstance(x, Var) 
                       and not isinstance(x, LocalVar) 
                       and not isinstance(x, SolverVar)])
    if global_vars:
      body_exps = (GlobalDecl(global_vars),)+body_exps
    if not body_has_any_statement:
      return (self.new(self.params, begin(*body_exps)),), False
    else:
      name = compiler.new_var(LocalVar('function'))
      body = begin(*body_exps).insert_return_statement()
      return (Function(name, self.params, body), name), True 
    
  def to_code(self, coder):
    head = "lambda %s: " % ', '.join(to_code_list(coder, self.params))
    result = head + '%s'%self.body.to_code_if_in_lambda_body(coder)
    return result
  
  def free_vars(self):
    return self.body.free_vars()-set(self.params)
  
  def bool(self):
    return True
  
  def __eq__(x, y):
    return classeq(x, y) and x.params==y.params and x.body==y.body
  
  def __hash__(self): return hash(id(self))
  
  def __repr__(self):
    return 'il.Lamda((%s), %s)'%(', '.join([repr(x) for x in self.params]),
                              repr(self.body))

def clamda(v, *body):
  body = tuple(element(x) for x in body)
  return Clamda(v, begin(*body))

class Clamda(Lamda):
  def __init__(self, v, body):
    self.params = (v, )
    self.body = body
    self.name = None
    
  def new(self, params, body):
    return self.__class__(params[0], body)
  
  def optimize_apply(self, data, args):
    param, arg = self.params[0], args[0]
    if not arg.side_effects():
      body = self.body.subst({param: arg}).optimize(data)
      return body
    else:
      ref_count = data.ref_count.get(param, 0)
      if ref_count==0:
        return begin(arg, self.body).optimize(data)
      else:
        return begin(Assign(param, arg), self.body).optimize(data)

  def __repr__(self):
    return 'il.Clamda(%r, %s)'%(self.params[0], repr(self.body))

class Done(Clamda):
  def __init__(self, param):
    self.params = (param,)
    self.body = param
    
  def new(self, params, body):
    return self.__class__(self.params[0])
  
  def __call__(self, *args):
    return self.body.subst({self.params[0]:args[0]})
  
  def replace_assign(self, bindings):
    return self
  
  def __repr__(self):
    return 'il.Done(%r, %s)'%(self.params[0], repr(self.body))

class Function(Lamda):
  '''recursive Function'''
  is_statement = True
  is_function = True
  
  def __init__(self, name, params, body):
    Lamda.__init__(self, params, body)
    self.name = name
  
  def new(self, params, body):
    return self.__class__(self.name, params, body)
  
  def optimize(self, data):
    old_assign_bindings = data.assign_bindings
    data.assign_bindings = {}
    result = self.new(self.params, self.body.optimize(data))
    data.assign_bindings = old_assign_bindings
    return result
  
  def optimize_apply(self, data, args):
    old_assign_bindings = data.assign_bindings
    data.assign_bindings = {}
    result = Lamda.optimize_apply(self, data, args)
    data.assign_bindings = old_assign_bindings
    return result
  
  def pythonize(self, env, compiler):
    body_exps, has_any_statement = self.body.pythonize(env, compiler)
    global_vars = self.find_assign_lefts()-set(self.params)
    global_vars = set([x for x in global_vars 
                       if isinstance(x, Var) 
                       and not isinstance(x, LocalVar)
                       and not isinstance(x, SolverVar) ])
    if global_vars:
      body_exps = (GlobalDecl(global_vars),)+body_exps
    if not body_exps[-1].is_statement:
      body_exps = body_exps[:-1] + (Return(body_exps[-1]),)
    else:
      body_exps = body_exps[:-1] + (body_exps[-1].insert_return_statement(),)
    return (self.new(self.params, begin(*body_exps)), self.name), True
    
  def to_code(self, coder):
    head = "def %s(%s):\n" % (self.name, ', '.join(to_code_list(coder, self.params)))
    result =  head + coder.indent(self.body.to_code(coder))
    return result
  
  def __repr__(self):
    return 'il.Function(%s, (%s), %s)'%(self.name, ', '.join([repr(x) for x in self.params]),
                                 repr(self.body))    

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
  
  def optimize_apply(self, data, args):
    old_assign_bindings = data.assign_bindings
    assigns = []
    free_vars = self.free_vars()
    for var, value in data.assign_bindings.items():
      if var in free_vars:
        assigns.append(Assign(var, value))
        old_assign_bindings[var]
    data.assign_bindings = {}
    result = begin(*(tuple(assigns)+(CFunction(self.name, self.params[0], 
        self.body.subst({self.params[0]:args[0]}).optimize(data))(NONE),)))
    data.assign_bindings = old_assign_bindings
    return result
  
  def __repr__(self):
    return 'il.CFunction(%r, %r, %s)'%(self.name, self.params[0],repr(self.body))
  
class RulesFunction(Function):
  def __init__(self, name, params, body):
    self.name, self.params, self.body = name, params, body
    
  def __call__(self, *args):
    return Apply(self, tuple(element(x) for x in args))
  
  def optimize_apply(self, data, args):
    old_assign_bindings = data.assign_bindings
    data.assign_bindings = {}
    result = Lamda.optimize_apply(self, data, args)
    data.assign_bindings = old_assign_bindings
    return result
  
  def to_code(self, coder):
    head = "def %s(%s, %s):\n" % (self.name, 
                                   self.params[0].to_code(coder), 
                                   self.params[1].to_code(coder))
    result =  head + coder.indent(self.body.to_code(coder))
    return result
  
class RulesDict(Element):
  def __init__(self, arity_body_map):
    self.arity_body_map = arity_body_map
    
  def optimization_analisys(self, data):
    try: self.seen
    except:
      self.seen = True
      data.occur_count[self] = data.occur_count.setdefault(self, 0)+1
      for arity, body in self.arity_body_map.items():
        body.optimization_analisys(data)
  
  def subst(self, bindings):
    return RulesDict({arity:body.subst(bindings) for arity, body in self.arity_body_map.items()})
  
  def side_effects(self):
    return False
  
  def free_vars(self):
    result = set()
    for arity, body in self.arity_body_map.items():
      result |= body.free_vars()
    return result
  
  def optimize(self, data):
    for arity, body in self.arity_body_map.items():
      self.arity_body_map[arity] = body.optimize(data) 
    return self
  
  def pythonize(self, env, compiler):
    exps = []
    has_statement = False
    arity_body_map = {}
    for arity, function in self.arity_body_map.items():
      exps1, has_statement1 = function.pythonize(env, compiler)
      exps += exps1[:-1]
      arity_body_map[arity] = exps1[-1]
      has_statement = has_statement or has_statement1
    exps.append(RulesDict(arity_body_map))
    return tuple(exps), has_statement

  def bool(self):
    return True
    
  def to_code(self, coder):
    return '{%s}'%', '.join('%s: %s'%(arity, funcname.to_code(coder))
                            for arity, funcname in self.arity_body_map.items())
  
  def __repr__(self):
    return 'RulesDict(%s)'%self.arity_body_map

class MacroLamda(Lamda):
  def pythonize(self, env, compiler):
    body_exps, body_has_any_statement = self.body.pythonize(env, compiler)
    global_vars = self.find_assign_lefts()-set(self.params)
    global_vars = set([x for x in global_vars 
                       if isinstance(x, Var) 
                       and not isinstance(x, LocalVar)
                       and not isinstance(x, SolverVar)])
    if global_vars:
      body_exps = (GlobalDecl(global_vars),)+body_exps
    if not body_has_any_statement:
      return (MacroFunction(self.new(self.params, begin(*body_exps))),), False
    else:
      name = compiler.new_var(LocalVar('function'))
      body = begin(*body_exps).insert_return_statement()
      return (Function(name, self.params, body), MacroFunction(name)), True 
  
class MacroFunction(Element):
  def __init__(self, function):
    self.function = function
    
  def to_code(self, coder):
    return 'MacroFunction(%s)'%self.function
  
  def __repr__(self):
    return 'MacroFunction(%s)'%self.function
  
class GlobalDecl(Element):
  def __init__(self, args):
    self.args = args
    
  def to_code(self, coder):
    return "global %s" % (', '.join([x.to_code(coder) for x in self.args]))
  
  def __repr__(self):
    return 'GlobalDecl(%s)'%self.args
  
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
      
  def free_vars(self):
    result = self.caller.free_vars()
    for exp in self.args:
      result |= exp.free_vars()
    return result
  
  def optimize(self, data):
    if isinstance(self.caller, Var):
      if self.caller not in data.recursive_call_path:
        caller = self.caller.optimize(data)
        if isinstance(caller, Lamda):
          data.recursive_call_path.append(self.caller)      
          result = caller.optimize_apply(data, self.args)
          data.recursive_call_path.pop()  
          return result
        else:
          return self.__class__(caller, optimize_args(self.args, data))
      else: 
        return self.__class__(self.caller, optimize_args(self.args, data))
    elif isinstance(self.caller, Lamda):
      args = optimize_args(self.args, data)
      return self.caller.optimize_apply(data, args)
    else:
      return self.__class__(self.caller.optimize(data), optimize_args(self.args, data))

  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def pythonize(self, env, compiler):
    exps, has_statement = self.caller.pythonize(env, compiler)
    caller = exps[-1]
    exps = exps[:-1]
    exps2, args, has_statement2 = pythonize_args(self.args, env, compiler)
    return exps+exps2+(self.__class__(caller,args),), has_statement or has_statement2
    
  def to_code(self, coder):
    if isinstance(self.caller, Lamda):
      return "(%s)"%self.caller.to_code(coder) + '(%s)'%', '.join([x.to_code(coder) for x in self.args])
    else:
      return self.caller.to_code(coder, ) + '(%s)'%', '.join([x.to_code(coder) for x in self.args])        
  
  def bool(self):
    return unknown
  
  def __eq__(x, y):
    return classeq(x, y) and x.caller==y.caller and x.args==y.args
  
  def __repr__(self):
    return '%r(%s)'%(self.caller, ', '.join([repr(x) for x in self.args]))

class ExpressionWithCode(Element):
  def __init__(self, exp, function):
    self.exp = exp
    self.function = function
  
  def optimization_analisys(self, data):  
    self.function.optimization_analisys(data)
    
  def side_effects(self):
    return False
  
  def subst(self, bindings):
    return ExpressionWithCode(self.exp, self.function.subst(bindings))
  
  def code_size(self):
    return 1

  def free_vars(self):
    return self.exp.free_vars()
  
  def optimize(self, data):
    return ExpressionWithCode(self.exp, self.function.optimize(data))
        
  def pythonize(self, env, compiler):
    exps, has_statement = self.function.pythonize(env, compiler)
    if has_statement:
      return (exps[0], ExpressionWithCode(self.exp, exps[1])), True
    else:
      return (ExpressionWithCode(self.exp, exps[0]),), False
      
  def __eq__(x, y):
    return classeq(x, y) and x.exp==y.exp
    
  def to_code(self, coder):
    return "ExpressionWithCode((%s), (%s))"%(self.exp.to_code(coder), self.function.to_code(coder))
  
  def __repr__(self):
    return "ExpressionWithCode(%r, %r)"%(self.exp, self.function)

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
      
  def optimize(self, data):
    if self in data.assign_bindings:
      return data.assign_bindings[self]
    return self
      
  def replace_assign(self, data):
    try:
      return data.assign_bindings[self]
    except:
      return self
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def pythonize(self, env, compiler):
    return (self,), False
      
  def to_code(self, coder):
    return self.name
    
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  def __call__(self, *args):
    args = tuple(element(arg) for arg in args)
    return Apply(self, args)
  
  def free_vars(self):
    return set([self])
  
  def bool(self):
    return unknown  
  
  def __hash__(self): return hash(self.name)

  def __repr__(self):
    return self.name #enough in tests

class RecursiveVar(Var): pass

class LocalVar(Var): pass

class SolverVar(Var):
  def __init__(self, name):
    self.name = 'solver.'+name
    
  def __repr__(self):
    return 'il.%s'%self.name.split('.')[1]

class LogicVar(Element):
  is_statement = False
  
  def __init__(self, name):
    self.name = name
  
  def assign_convert(self, env, compiler):
    return self
  
  def find_assign_lefts(exp):
    return set()
  
  def optimization_analisys(self, data): 
    return
  
  def subst(self, bindings):  
    return self
  
  def free_vars(self):
    return set()
  
  def optimize(self, data):
    return self
    
  def replace_assign(self, data):
    return self
  
  def pythonize(self, env, compiler):
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
  
  def assign_convert(self, env, compiler):
    return SetContent(env[self.var], self.exp.assign_convert(env, compiler))
  
  def find_assign_lefts(self):
    return set([self.var])
  
  def optimization_analisys(self, data):  
    self.exp.optimization_analisys(data)
  
  def insert_return_statement(self):
    return begin(self, Return(self.var))
  
  def code_size(self):
    return code_size(self.exp)+2
    
  def side_effects(self):
    return True
    
  def subst(self, bindings):  
    return Assign(self.var, self.exp.subst(bindings))
        
  def free_vars(self):
    return self.exp.free_vars()|set([self.var])
  
  def optimize(self, data):
    exp = self.exp.optimize(data)
    #if self.var in self.exp.free_vars():
      #return Assign(self.var, self.exp)
    if isinstance(exp, Atom) or isinstance(exp, Var) or isinstance(exp, ExpressionWithCode) or isinstance(exp, RulesDict):
      data.assign_bindings[self.var] = exp
      return
    if isinstance(exp, Lamda) and not isinstance(self.var, RecursiveVar):
      if self.var not in exp.free_vars():
        data.assign_bindings[self.var] = exp
        return None
    else:
      #exp.side_effects():
      if self.var in data.assign_bindings:
        del data.assign_bindings[self.var]
      return Assign(self.var, exp)
    #else:
      #data.assign_bindings[self.var] = exp
      #if isinstance(self.var, RecursiveVar):
        #return Assign(self.var, exp)
  
  def pythonize(self, env, compiler):
    exps, has_statement = self.exp.pythonize(env, compiler)
    if exps[-1].is_statement:
      return exps+(Assign(self.var, NONE),), True
    else:
      return exps[:-1]+(Assign(self.var, exps[-1]),), True
    
  def to_code(self, coder):
    return  '%s = %s' % (self.var.to_code(coder), self.exp.to_code(coder))
    
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.exp==y.exp
  
  def __repr__(self):
    return 'il.Assign(%r, %r)'%(self.var, self.exp)
  

def while_(test, *exps):
  return While(element(test), begin(*[x for x in exps]))
  
class While(Element):
  def __init__(self, test, body):
    self.test, self.body = test, body
    
  def assign_convert(self, env, compiler):
    return While(self.test.assign_convert(env, compiler), 
                 self.body.assign_convert(env, compiler))

  def find_assign_lefts(self):
    return self.body.find_assign_lefts()
  
  def optimization_analisys(self, data):  
    self.test.optimization_analisys(data)
    self.body.optimization_analisys(data)
  
  def free_vars(self):
    return self.test.free_vars() | self.body.free_vars()
  
  def code_size(self):
    return 3 + self.test.code_size() + \
           self.body.code_size()
  
  def side_effects(self):
    return not self.test.side_effects() and\
           not self.body.side_effects()
    
  def subst(self, bindings):  
    return While(self.test.subst(bindings),
              self.body.subst(bindings))
    
  def optimize(self, data):
    free_vars = self.free_vars()
    assigns = []
    for var, value in data.assign_bindings.items():
      if var in free_vars:
        assigns.append(Assign(var, value))
        del data.assign_bindings[var]
    result = begin(*(tuple(assigns) + (While(self.test.optimize(data), self.body.optimize(data)),)))
    return result

  def insert_return_statement(self):
    result = While(self.test, 
              self.body.insert_return_statement())
    result.is_statement = True
    return result
  
  def replace_return_with_yield(self):
    result = While(self.test, 
              self.body.replace_return_with_yield())
    result.is_statement = True
    return result
  
  def pythonize(self, env, compiler):
    test, has_statement1 = self.test.pythonize(env, compiler)
    body, has_statement2 = self.body.pythonize(env, compiler)
    result = While(test[-1], begin(*body))
    return test[:-1]+(result,), True
    
  def to_code(self, coder):
    return 'while %s:\n%s\n' % (self.test.to_code(coder), 
                                  coder.indent(self.body.to_code(coder)))
           
  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.body==y.body
  
  def __repr__(self):
    return 'il.While(%r, %r)'%(self.test, self.body)

def for_(var, range, *exps):
  return For(element(var), element(range), begin(*[x for x in exps]))

class For(Element):
  def __init__(self, var, range, body):
    self.var, self.range, self.body = var, range, body
    
  def assign_convert(self, env, compiler):
    return For(self.var.assign_convert(env, compiler), 
               self.range.assign_convert(env, compiler), 
               self.body.assign_convert(env, compiler))

  def find_assign_lefts(self):
    return self.body.find_assign_lefts()
  
  def optimization_analisys(self, data):  
    self.var.optimization_analisys(data)
    self.range.optimization_analisys(data)
    self.body.optimization_analisys(data)
    
  def code_size(self):
    return 3 + self.var.code_size() + self.range.code_size() + self.body.code_size()
  
  def side_effects(self):
    return not self.var.side_effects() and\
           not self.range.side_effects() and\
           not self.body.side_effects()
    
  def subst(self, bindings):  
    return For(self.var.subst(bindings),
               self.range.subst(bindings),
               self.body.subst(bindings))
    
  def free_vars(self):
    return self.var.free_vars() | self.range.free_vars() | self.body.free_vars()
  
  def optimize(self, data):
    free_vars = self.free_vars()
    assigns = []
    for var, value in data.assign_bindings.items():
      if var in free_vars:
        assigns.append(Assign(var, value))
        del data.assign_bindings[var]
    return begin(*(tuple(assigns) + (For(self.var, self.range.optimize(data), self.body.optimize(data)),)))

  def insert_return_statement(self):
    return For(self.var, self.range, self.body.insert_return_statement())
  
  def replace_return_with_yield(self):
    return For(self.var, self.range, self.body.replace_return_with_yield())
  
  def pythonize(self, env, compiler):
    var, has_statement1 = self.var.pythonize(env, compiler)
    range, has_statement1 = self.range.pythonize(env, compiler)
    body, has_statement2 = self.body.pythonize(env, compiler)
    return (For(var[-1], range[-1], begin(*body)),), True
    
  def to_code(self, coder):
    return 'for %s in %s:\n%s\n' % (self.var.to_code(coder), 
                                    self.range.to_code(coder), 
                                  coder.indent(self.body.to_code(coder)))
           
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.range==y.range and x.body==y.body
  
  def __repr__(self):
    return 'il.For(%r, %r, %r)'%(self.var, self.range, self.body)

