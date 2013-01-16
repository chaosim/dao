from dao.base import classeq

from dao.compilebase import MAX_EXTEND_CODE_SIZE
from dao.compilebase import VariableNotBound, CompileTypeError

from element import pythonize_args, optimize_args
from element import Element, begin, Return, Begin
from element import NONE, unknown, make_tuple, Tuple, ConstAtom, Tuple, List

def lamda(params, *body):
  return Lamda(params, begin(*body))

class Lamda(Element):
  def __init__(self, params, body):
    self.params, self.body = params, body
    self.has_pythonized = False
    
  def new(self, params, body):
    return self.__class__(params, body)
  
  def __call__(self, *args):
    return Apply(self, args)
  
  def find_assign_lefts(self):
    return self.body.find_assign_lefts()
  
  def analyse(self, compiler):
    compiler.lamda_stack.append(self)
    self.body.analyse(compiler)
    compiler.lamda_stack.pop()
        
  def code_size(self):
      return self.body.code_size()+len(self.params)+2
    
  def side_effects(self):
      return False
    
  def subst(self, bindings):
    result = self.new(self.params, self.body.subst(bindings))
    return result
      
  def optimize(self, env, compiler):
    env = env.extend()
    for var in self.free_vars()|self.find_assign_lefts():
      if isinstance(var, ConstLocalVar):
        continue
      try:
        assign = env[var]
      except: continue
      env[var] = VarAssignBox(var)
      assign.dont_remove()
    body = self.body.optimize(env, compiler)
    for var in self.find_assign_lefts():
      if isinstance(var, LocalVar):
        continue
      try:
        assign = env[var]
      except: continue
      assign.dont_remove()
    result = self.new(self.params, body)
    return result
  
  def optimize_apply(self, env, compiler, args):
    #1. ((lambda () body))  =>  body 
    if len(self.params)==0:
      return self.body.optimize(env, compiler)
    
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
        ref_count = compiler.ref_count.get(p, 0)
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
        return Apply(self.new(new_params, self.body.subst(bindings).optimize(env, compiler)), 
                        tuple(arg.optimize(env, compiler) for arg in new_args))
      else:
        if len(new_params)!=len(self.params):
          Apply(self.new(new_params, self.body.subst(bindings).optimize(env, compiler)), 
                tuple(arg.optimize(env, compiler) for arg in new_args))   
        else:
          return Apply(self.new(new_params, self.body.optimize(env, compiler)), 
                       optimize_args(new_args, env, compiler))
    else:
      if bindings:
        return self.body.subst(bindings).optimize(env, compiler)
      else:
        return self.body.optimize(env, compiler)
  
  def insert_return_statement(self):
    return Return(self)
  
  def pythonize(self, env, compiler):
    if self.has_pythonized:
      return (self.name,), False
    body_exps, body_has_any_statement = self.body.pythonize(env, compiler)
    global_vars = begin(*body_exps).find_assign_lefts()-set(self.params)
    global_vars = set([x for x in global_vars 
                       if isinstance(x, Var) 
                       and not isinstance(x, LocalVar) 
                       and not isinstance(x, SolverVar)])
    if global_vars:
      body_exps = (GlobalDecl(global_vars),)+body_exps
    if not body_has_any_statement:
      return (self.new(self.params, begin(*body_exps)),), False
    else:
      self.has_pythonized = True
      name = compiler.new_var(LocalVar('function'))
      body = begin(*body_exps).insert_return_statement()
      self.name = name
      return (Function(name, self.params, body), name), True 
    
  def to_code(self, compiler):
    head = "lambda %s: " % ', '.join(tuple(x.to_code(compiler) for x in self.params))
    result = head + '%s'%self.body.to_code(compiler)
    return result
  
  def free_vars(self):
    return self.body.free_vars()-set(self.params)
  
  def bool(self):
    return True
  
  def __eq__(x, y):
    return classeq(x, y) and x.params==y.params and x.body==y.body
  
  def __hash__(self): return hash(id(self))
  
  def __repr__(self):
    return 'il.Lamda((%s), \n%s)'%(', '.join([repr(x) for x in self.params]),
                              repr(self.body))

class RulesLamda(Lamda):
  def __init__(self, params, body):
    self.has_pythonized = False
    self.params, self.body = params, body
    
  def __call__(self, *args):
    return Apply(self, tuple(element(x) for x in args))
  
  def optimize_apply(self, env, compiler, args):
    result = Lamda.optimize_apply(self, env, compiler, args)
    return result
  
  def to_code(self, compiler):
    head = "lambda %s, %s: " % (self.params[0].to_code(compiler), 
                               self.params[1].to_code(compiler))
    result =  head + self.body.to_code(compiler)
    return result
  
def clamda(v, *body):
  return Clamda(v, begin(*body))

class Clamda(Lamda):
  def __init__(self, v, body):
    self.has_pythonized = False
    self.params = (v, )
    self.body = body
    self.name = None
    
  def new(self, params, body):
    return self.__class__(params[0], body)
  
  def optimize_apply(self, env, compiler, args):
    param, arg = self.params[0], args[0]
    if not arg.side_effects():
      body = self.body.subst({param: arg}).optimize(env, compiler)
      return body
    else:
      ref_count = compiler.ref_count.get(param, 0)
      if ref_count==0:
        return begin(arg, self.body).optimize(env, compiler)
      else:
        return begin(Assign(param, arg), self.body).optimize(env, compiler)

  def __call__(self, arg):
    if arg.side_effects():
      return begin(Assign(self.params[0], arg), self.body)
    else:
      result = self.body.subst({self.params[0]:arg})
      return result
  
  def __repr__(self):
    return 'il.Clamda(%r, \n%s)'%(self.params[0], repr(self.body))

class EqualCont:
  is_statement = False
  
  def __call__(self, body):
    return body
  
  def subst(self, bindings):
    return self
  
  def analyse(self, compiler):
    return
        
  def code_size(self):
      return 1
    
  def side_effects(self):
      return False
    
  def optimize(self, env, compiler):
      return self
    
  def pythonize(self, env, compiler):
    return (self,), False
  
  def to_code(self, compiler):
    return 'lambda v:v'
  
  def __repr__(self):
    return 'EqualCont'
  
equal_cont = EqualCont()

class Done(Clamda):
  def __init__(self, param):
    self.has_pythonized = False
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
  
  def __init__(self, name, params, body):
    Lamda.__init__(self, params, body)
    self.name = name
  
  def new(self, params, body):
    return self.__class__(self.name, params, body)
  
  def optimize(self, env, compiler):
    env = env.extend()
    for var in self.free_vars():
      try:
        env[var]._removed = False
      except: pass
      env[var] = VarAssignBox(var)
    body = self.body.optimize(env, compiler)
    for var in self.free_vars():
      try:
        assign = env[var]
      except: continue
      assign.dont_remove()
    result = self.new(self.params, body)
    return result
    
  def optimize_apply(self, env, compiler, args):
    result = Lamda.optimize_apply(self, env, compiler, args)
    return result
  
  def pythonize(self, env, compiler):
    if self.has_pythonized:
      return (self.name,), False
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
    
    self.has_pythonized = True
    return (self.new(self.params, begin(*body_exps)), self.name), True
    
  def to_code(self, compiler):
    head = "def %s(%s):\n" % (self.name, ', '.join(tuple(x.to_code(compiler) for x in self.params)))
    result =  head + compiler.indent(self.body.to_code(compiler))
    return result
  
  def __repr__(self):
    return 'il.Function(%s, (%s), \n%s)'%(self.name, ', '.join([repr(x) for x in self.params]),
                                 repr(self.body))    

def cfunction(name, v, *body):
  return CFunction(name, v, begin(*body))

class CFunction(Function):
  is_statement = True
  is_function = True
  
  def __init__(self, name, v, body):
    Function.__init__(self,  name, (v,), body)
    
  def new(self, params, body):
    return self.__class__(self.name, params[0], body)
  
  def optimize_apply(self, env, compiler, args):
    new_env = env.extend()
    for var in self.find_assign_lefts()|self.free_vars():
      if isinstance(var, ConstLocalVar):
        continue      
      try:
        assign = env[var]
        new_env[var] = VarAssignBox(var)
      except:
        continue
      assign.dont_remove()
    body = self.body.subst({self.params[0]:args[0]}) 
    body = body.optimize(new_env, compiler)
    for var in self.free_vars():
      if isinstance(var, ConstLocalVar):
        continue      
      try:
        assign = new_env[var]
        #new_env[var] = VarAssignBox(var)
        #self.recursive_vars.add(var)
      except:
        continue
      assign.dont_remove()
    result = CFunction(self.name, self.params[0], body)(NONE)
    #for var in new_env.bindings:
      #env[var]._removed = False
    #compiler.recusive_vars_stack.pop()
    return result
  
  def __repr__(self):
    return 'il.CFunction(%r, %r, \n%s)'%(self.name, self.params[0],repr(self.body))
  
class RulesDict(Element):
  def __init__(self, arity_body_map):
    self.arity_body_map = arity_body_map
    self.to_coded = False
    
  def analyse(self, compiler):
    try: self.seen
    except:
      self.seen = True
      compiler.occur_count[self] = compiler.occur_count.setdefault(self, 0)+1
      for arity, body in self.arity_body_map.items():
        body.analyse(compiler)
  
  def subst(self, bindings):
    self.arity_body_maparity_body_map = {arity:body.subst(bindings) for arity, body in self.arity_body_map.items()}
    return self
  
  
  def side_effects(self):
    return False
  
  def free_vars(self):
    result = set()
    for arity, body in self.arity_body_map.items():
      result |= body.free_vars()
    return result
  
  def optimize(self, env, compiler):
    for var in self.arity_body_map.values():
      env[var].dont_remove()
    return self
  
  def pythonize(self, env, compiler):
    return (self,), False

  def bool(self):
    return True
    
  def to_code(self, compiler):
    if self.to_coded:
      return self.name.to_code(compiler)
    else:
      self.to_coded = True
      return '{%s}'%', '.join('%s: %s'%(arity, funcname.to_code(compiler))
                            for arity, funcname in self.arity_body_map.items())
  
  def __repr__(self):
    return 'RulesDict(%s)'%self.arity_body_map

class MacroLamda(Lamda):
  def optimize_apply(self, env, compiler, args):
    #args = (args[0], Tuple(*args[1:]))
    result = Lamda.optimize_apply(self, env, compiler, args)
    return result
  
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
    
  def to_code(self, compiler):
    return 'MacroFunction(%s)'%self.function
  
  def __repr__(self):
    return 'MacroFunction(%s)'%self.function
  
class GlobalDecl(Element):
  def __init__(self, args):
    self.args = args
  
  def side_effects(self):
    return False
  
  def to_code(self, compiler):
    return "global %s" % (', '.join([x.to_code(compiler) for x in self.args]))
  
  def __repr__(self):
    return 'GlobalDecl(%s)'%self.args
  
class Apply(Element):
  is_statement = False
  
  def __init__(self, caller, args):
    self.caller, self.args = caller, args
  
  def find_assign_lefts(exp):
    return set()
  
  def analyse(self, compiler):  
    compiler.called_count[self.caller] = compiler.called_count.setdefault(self.caller, 0)+1
    self.caller.analyse(compiler)
    for arg in self.args:
      arg.analyse(compiler)
        
  def code_size(self):
    return self.caller.code_size()+sum([x.code_size() for x in self.args])
              
  def side_effects(self):
    if isinstance(self.caller, Lamda):
      if self.caller.body.side_effects(): return True
    elif isinstance(self.caller, Var): return True
    elif self.caller.has_side_effects: return True
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
  
  def optimize(self, env, compiler):
    args = optimize_args(self.args, env, compiler)
    if isinstance(self.caller, Var):
      if self.caller not in compiler.recursive_call_path:
        caller = self.caller.optimize(env, compiler)
        if isinstance(caller, Lamda):
          compiler.recursive_call_path.append(self.caller)      
          result = caller.optimize_apply(env, compiler, args)
          compiler.recursive_call_path.pop()  
          return result
        else:
          return self.__class__(caller, args)
      else: 
        return self.__class__(self.caller, args)
    elif isinstance(self.caller, Lamda):
      return self.caller.optimize_apply(env, compiler, args)
    else:
      caller = self.caller.optimize(env, compiler)
      if isinstance(caller, Lamda):
        return caller.optimize_apply(env, compiler, args)
      else:
        return self.__class__(caller, args)

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
    
  def to_code(self, compiler):
    if isinstance(self.caller, Lamda):
      return "(%s)"%self.caller.to_code(compiler) + '(%s)'%', '.join([x.to_code(compiler) for x in self.args])
    else:
      return self.caller.to_code(compiler, ) + '(%s)'%', '.join([x.to_code(compiler) for x in self.args])        
  
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
  
  def analyse(self, compiler):  
    self.function.analyse(compiler)
    
  def side_effects(self):
    return False
  
  def subst(self, bindings):
    return ExpressionWithCode(self.exp, self.function.subst(bindings))
  
  def code_size(self):
    return 1

  def free_vars(self):
    return self.function.free_vars()
  
  def optimize(self, env, compiler):
    return ExpressionWithCode(self.exp, self.function.optimize(env, compiler))
        
  def pythonize(self, env, compiler):
    exps, has_statement = self.function.pythonize(env, compiler)
    if has_statement:
      return (exps[0], ExpressionWithCode(self.exp, exps[1])), True
    else:
      return (ExpressionWithCode(self.exp, exps[0]),), False
      
  def __eq__(x, y):
    return classeq(x, y) and x.exp==y.exp
    
  def to_code(self, compiler):
    return "ExpressionWithCode((%s), (%s))"%(self.exp.to_code(compiler), self.function.to_code(compiler))
  
  def __repr__(self):
    return "ExpressionWithCode(%r, %r)"%(self.exp, self.function)

class Var(Element):
  is_statement = False
  
  def __init__(self, name):
    self.name = name
        
  def find_assign_lefts(self):
    return set()
  
  def analyse(self, compiler):
    compiler.ref_count[self] = compiler.ref_count.setdefault(self, 0)+1    
    
  def code_size(self):
    return 1
        
  def side_effects(self):
    return False
        
  def subst(self, bindings):  
    try: return bindings[self]
    except: return self
  
  def optimize(self, env, compiler):
    try: 
      assign = env[self]
    except: 
      return self
    return assign.right_value()
      
  def replace_assign(self, compiler):
    try:
      return env[self]
    except:
      return self
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def pythonize(self, env, compiler):
    return (self,), False
      
  def to_code(self, compiler):
    return self.name
    
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  def __call__(self, *args):
    return Apply(self, args)
  
  def free_vars(self):
    return set([self])
  
  def bool(self):
    return unknown  
  
  def __hash__(self): return hash(self.name)

  def __repr__(self):
    return self.name 

class RecursiveVar(Var): pass

class LocalVar(Var): pass

class ConstLocalVar(LocalVar): pass

class SolverVar(Var):
  def __init__(self, name):
    self.name = 'solver.'+name
    
  def __repr__(self):
    return 'il.%s'%self.name.split('.')[1]

class LogicVar(Element):
  is_statement = False
  
  def __init__(self, name):
    self.name = name
  
  def find_assign_lefts(exp):
    return set()
  
  def analyse(self, compiler): 
    return
  
  def subst(self, bindings):  
    return self
  
  def free_vars(self):
    return set()
  
  def optimize(self, env, compiler):
    return self
    
  def replace_assign(self, compiler):
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
  
  def to_code(self, compiler):
    return  "LogicVar('%s')"%self.name
  
  def __repr__(self):
    return "LogicVar(%s)"%self.name 

class DummyVar(LogicVar):
  def to_code(self, compiler):
    return  "DummyVar('%s')"%self.name

class VarAssignBox:
  def __init__(self, var):
    self.var = var
    self._removed = unknown
    self.items = set([self])
    self.dependency = set()
  
  def right_value(self):
    return self.var
  
  def remove(self):
    self._removed = True
  
  def dont_remove(self):
    self._removed = False
    
  def removed(self): 
    return self._removed
    
  def __repr__(self):
    return 'VAB(%r)'%self.right_value()
  
class LoopAssignBox:
  def __init__(self, assign, loop_exp):
    self.loop_exp = loop_exp
    self.assign = assign
    self.var = assign.var
    self._removed = unknown
    self.items = set([self])
    self.dependency = set()
  
  def free_vars(self):
    return self.assign.right_value().free_vars()
  
  def pythonize(self, env, compiler):
    if self._removed==False:
      return self.var.pythonize(env, compiler) 
    else:
      return self.assign.right_value().pythonize(env, compiler)   
  
  def right_value(self):
    return self
  
  def remove(self):
    self._removed = True
    self.assign.remove()
  
  def dont_remove(self):
    self._removed = False
    self.assign.dont_remove()
    
  def removed(self): 
    return self._removed
    
  def __repr__(self):
    return 'LAB(%r)'%self.assign
  
  
class ThenAssignBox:
  def __init__(self, item, if_exp):
    self.item = item
    self.var = item.var
    self.then_removed = False
    self.items = set([self])
    self.dependency = set()
    self.if_exp = if_exp
  
  def right_value(self):
    return self.item.right_value()
  
  def remove(self):
    self.then_removed = True
    
  def else_remove(self):
    if self.then_removed:
      self.item.remove()
 
  def dont_remove(self):
    self.item.dont_remove()
    
  def __repr__(self):
    return 'TB(%s)'%self.item
  
class ElseAssignBox:
  def __init__(self, item):
    self.item = item
    self.var = item.var
    self.if_exp = self.item.if_exp
    self.items = set([self])
    self.dependency = set()

  def right_value(self):
    return self.item.right_value()
  
  def remove(self):
    self.item.else_remove()  
  
  def dont_remove(self):
    self.item.dont_remove()
    
  def __repr__(self):
    return 'EB(%s)'%self.item

class GroupAssignBox:
  def __init__(self, var, *items):
    self.var = var
    items1 = []
    for item in items:
      if isinstance(item, GroupAssignBox):
        items1 += item.items
      else: 
        items1.append(item)
    self.items = items1
    value = items1[0].right_value()
    for item in items[1:]:
      if item.right_value()!=value:
        self._right_value = self.var
        break
    else:
      self._right_value = value
  
  def right_value(self):
    return self._right_value
  
  def remove(self):
    for item in self.items:
      item.remove() 
      
  def dont_remove(self):
    for item in self.items:
      item.dont_remove()
      
  def __repr__(self):
    return 'GB(%s)'%self.items

class Assign(Element):
  is_statement = True
  
  def __init__(self, var, exp):
    self.var, self.exp =  var, exp
    self._removed = unknown
    self.items = set([self])
    self.dependency = set()
  
  def find_assign_lefts(self):
    return set([self.var])
  
  def analyse(self, compiler): 
    self.exp.analyse(compiler)
  
  def insert_return_statement(self):
    return begin(self, Return(self.var))
  
  def code_size(self):
    return code_size(self.exp)+2
    
  def side_effects(self):
    return True
    
  def subst(self, bindings):  
    return Assign(self.var, self.exp.subst(bindings))
        
  def free_vars(self):
    return self.exp.free_vars()
  
  def right_value(self):
    return self.exp
  
  def remove(self):
    self._removed = True
    
  def dont_remove(self):
    self._removed = False
      
  def removed(self):
    if self._removed==True:
      return True
    if self._removed==False:
      return False
    if self.dependency:
      for item in self.dependency:
        if item.removed()==False: 
          return False
    return True
    
  def optimize(self, env, compiler):
    try:
      assign_box = env[self.var]
    except:
      assign_box = None
    if isinstance(assign_box, LoopAssignBox):
      assign_box.dont_remove()
      right_value = assign_box.assign.right_value()
      env[self.var] = VarAssignBox(self.var)
    exp = self.exp.optimize(env, compiler)
    result = Assign(self.var, exp)
    try:
      env.bindings[self.var].remove()
      del env.bindings[self.var]
    except: pass    
    if isinstance(exp, ConstAtom) or isinstance(exp, ExpressionWithCode):
      env[self.var] = result
    elif isinstance(exp, RulesDict):
      env[self.var] = result
      exp.name = self.var
      result._removed = False
    elif isinstance(exp, Lamda):
      if isinstance(self.var, RecursiveVar):
        result._removed = False
      elif self.var.name=='solver.fail_cont':
        result._removed = False
      else: 
        env[self.var] = result
    else:
      result._removed = False
      assign_box = VarAssignBox(self.var)
      env[self.var] = assign_box
      for var in exp.free_vars():
        try:
          assign = env[var]
        except:
          assign = None
        if assign is not None:
          for item in assign.items:
            item.dependency.add(assign_box)
    return result
  
  def pythonize(self, env, compiler):
    if not self.var.name.startswith('solver.'):
      if isinstance(self.exp, Function):
        self.exp.name = self.var
        fun = self.exp
      elif isinstance(self.exp, Lamda):
        fun = Function(self.var, self.exp.params, self.exp.body)
      else: 
        fun = None
      if fun is not None:
        result = fun.pythonize(env, compiler)
        if isinstance(result[0][-1], Var):
          result = result[0][:-1], result[1]
        return result
    exps, has_statement = self.exp.pythonize(env, compiler)
    if exps[-1].is_statement:
      return exps+(Assign(self.var, NONE),), True
    else:
      return exps[:-1]+(Assign(self.var, exps[-1]),), True
    
  def to_code(self, compiler):
    if isinstance(self.exp, RulesDict) and self.exp.to_coded:
      return ''
    return  '%s = %s' % (self.var.to_code(compiler), self.exp.to_code(compiler))
    
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.exp==y.exp
  
  def __repr__(self):
    return 'il.Assign(%r, %r)'%(self.var, self.exp)

class UnoptimizabaleAssign(Assign):
  def __init__(self, var, exp):
    self.var, self.exp =  var, exp
    self._removed = False
    #self.items = set([self])
    #self.dependency = set()
    
  def subst(self, bindings):  
    return UnoptimizabaleAssign(self.var, self.exp.subst(bindings))
        
  def optimize(self, env, compiler):
    self.exp = self.exp.optimize(env, compiler)
    return self
     
class AssignFromList(Element):
  is_statement = True
  
  def __init__(self, *args):
    self.vars = args[:-1]
    self.value = args[-1]
  
  def side_effects(self):
    return True
    
  def analyse(self, compiler):
    for var in self.vars:
      var.analyse(compiler)
    self.value.analyse(compiler)
    
  def subst(self, bindings):  
    return AssignFromList(*(tuple(var.subst(bindings) for var in self.vars)+(self.value.subst(bindings),)))
  
  def code_size(self):
    return 1  
  
  def free_vars(self):
    result = set(self.vars)
    result |= self.value.free_vars()
    return result
  
  def optimize(self, env, compiler):
    value = self.value.optimize(env, compiler)
    if isinstance(value, Tuple) or isinstance(value, List):
      if len(value.item)!=len(self.vars):
        raise DaoCompileError
      else:
        return begin(*tuple(Assign(var, v) 
                     for var, v in zip(self.vars, value.item)
                     )).optimize(env, compiler)
    elif isinstance(value, Var):
      env[value].dont_remove()
    return AssignFromList(*(self.vars+(value,)))
  
  def pythonize(self, env, compiler):
    value_exps, has_statement1 = self.value.pythonize(env, compiler)
    return value_exps[:-1]+(AssignFromList(*(self.vars+(value_exps[-1],))),), True
  
  def insert_return_statement(self):
    return Return(self)
  
  def replace_return_with_yield(self):
    return self
  
  def bool(self):
    return False
  
  def to_code(self, compiler):
    return "%s = %s" % (', '.join([x.to_code(compiler) for x in self.vars]), 
                      self.value.to_code(compiler))
  
  def __repr__(self):
    return 'il.AssignFromList(%r, %r)'%(self.vars, self.value)

def if_(test, then, else_):
  return If(element(test), element(then), element(else_))

class If(Element):
  def __init__(self, test, then, else_):
    self.test, self.then, self.else_ = test, then, else_
    if else_==pseudo_else: self.is_statement = True
    
  def find_assign_lefts(self):
    return self.then.find_assign_lefts() | self.else_.find_assign_lefts()
  
  def analyse(self, compiler):  
    self.test.analyse(compiler)
    self.then.analyse(compiler)
    self.else_.analyse(compiler)
    
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
    
  def free_vars(self):
    result = set()
    result |= self.test.free_vars()
    result |= self.then.free_vars()
    result |= self.else_.free_vars()
    return result
  
  def optimize(self, env, compiler):
    test = self.test.optimize(env, compiler)
    test_bool = test.bool()
    if test_bool==True:
      then = self.then.optimize(env, compiler)
      if isinstance(then, If) and then.test==test: # (if a (if a b c) d)
        then = then.then      
      return then
    elif test_bool==False:
      else_ = self.else_.optimize(env, compiler)
      if isinstance(else_, If) and else_.test==test: # (if a b (if a c d))
        else_ = else_.else_      
      return else_    
    then_bindings = {}
    else_bindings = {}
    for var, item in env.bindings.items():
      then_box = ThenAssignBox(item, self)
      then_bindings[var] = then_box
      else_bindings[var] = ElseAssignBox(then_box)
    env.bindings = then_bindings
    then = self.then.optimize(env, compiler)
    then_bindings = env.bindings
    env.bindings = else_bindings
    else_ = self.else_.optimize(env, compiler)
    else_bindings = env.bindings
    env.bindings = {}
    for var, value in then_bindings.items():
      if var in else_bindings:
        else_value = else_bindings[var]
        if isinstance(value, ThenAssignBox):
          if value.if_exp is self:
            if isinstance(else_value, ElseAssignBox) and else_value.if_exp is self:
              env.bindings[var] = value.item
            else:
              env.bindings[var] = GroupAssignBox(var, value.item, else_value)
          else:
            if isinstance(else_value, ElseAssignBox) and else_value.if_exp is self:
              env.bindings[var] = GroupAssignBox(var, value, else_value.item.item)
            else:
              env.bindings[var] = GroupAssignBox(var, value, else_value)
        else:
          if isinstance(else_value, ElseAssignBox) and else_value.if_exp is self:
            env.bindings[var] = GroupAssignBox(var, value, else_value.item.item)
          else:
            env.bindings[var] = GroupAssignBox(var, value, else_value)
        del else_bindings[var]
      else:
        env.bindings[var] = value
    for var, value in else_bindings.items():
      env.bindings[var] = value
    if isinstance(then, If) and then.test==test: # (if a (if a b c) d)
      then = then.then      
    if isinstance(else_, If) and else_.test==test: # (if a b (if a c d))
      else_ = else_.else_
    return If(test, then, else_)

  def insert_return_statement(self):
    result = If(self.test, 
              self.then.insert_return_statement(), 
              self.else_.insert_return_statement())
    result.is_statement = True
    return result
  
  def replace_return_with_yield(self):
    result = If(self.test, 
              self.then.replace_return_with_yield(), 
              self.else_.replace_return_with_yield())
    result.is_statement = True
    return result
  
  def pythonize(self, env, compiler):
    test, has_statement1 = self.test.pythonize(env, compiler)
    then, has_statement2 = self.then.pythonize(env, compiler)
    else_, has_statement3 = self.else_.pythonize(env, compiler)
    if_ = If(test[-1], begin(*then), begin(*else_))
    if_.is_statement = if_.is_statement or has_statement2 or has_statement3
    return test[:-1]+(if_,), has_statement1 or if_.is_statement
    
  def to_code(self, compiler):
    if self.is_statement:
      result = 'if %s: \n%s\n' % (self.test.to_code(compiler), 
                                  compiler.indent(self.then.to_code(compiler)))
      if self.else_!=pseudo_else:
        result += 'else:\n%s\n'% compiler.indent(self.else_.to_code(compiler)) 
      return result
    else:
      return '%s if %s else %s' % (self.then.to_code(compiler), 
                                   self.test.to_code(compiler), 
                                   self.else_.to_code(compiler))        
  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.then==y.then and x.else_==y.else_
  
  def __repr__(self):
    if self.else_!=pseudo_else:
      return 'il.If(%r, \n%r, \n%r)'%(self.test, self.then, self.else_)
    else:
      return 'il.If(%r, \n%r)'%(self.test, self.then)

def if2(test, then):
  return If(test, then, pseudo_else)

class PseudoElse(ConstAtom):
  def __init__(self):
    return
  
  def code_size(self):
    return 0
  
  def insert_return_statement(self):
    return self
  
  def replace_return_with_yield(self):
    return self
  
  def to_code(self, compiler):
    return ''
  
  def __eq__(x, y):
    return classeq(x, y)
  
  def __repr__(self):
    return 'il.pseudo_else'

pseudo_else = PseudoElse()

def while_(test, *exps):
  return While(test, begin(*[x for x in exps]))
  
class While(Element):
  def __init__(self, test, body):
    self.test, self.body = test, body
    
  def find_assign_lefts(self):
    return self.body.find_assign_lefts()
  
  def analyse(self, compiler):  
    self.test.analyse(compiler)
    self.body.analyse(compiler)
  
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
    
  def optimize(self, env, compiler):
    free_vars = self.free_vars()
    for var in free_vars:
      if isinstance(var, ConstLocalVar):
        continue
      try:
        assign = env[var]
      except: 
        continue
      env[var] = LoopAssignBox(assign, self)
    test = self.test.optimize(env, compiler)
    body = self.body.optimize(env, compiler)
    for var in free_vars:
      try:
        assign = env[var]
      except: continue
      if isinstance(assign, LoopAssignBox) and assign.loop_exp is self:
        env[var] = assign.assign
    result = While(test,body)
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
    
  def to_code(self, compiler):
    return 'while %s:\n%s\n' % (self.test.to_code(compiler), 
                                  compiler.indent(self.body.to_code(compiler)))
           
  def __eq__(x, y):
    return classeq(x, y) and x.test==y.test and x.body==y.body
  
  def __repr__(self):
    return 'il.While(%r, \n%r)'%(self.test, self.body)

def for_(var, range, *exps):
  return For(element(var), element(range), begin(*[x for x in exps]))

class For(Element):
  def __init__(self, var, range, body):
    self.var, self.range, self.body = var, range, body
    
  def find_assign_lefts(self):
    return self.body.find_assign_lefts()
  
  def analyse(self, compiler):  
    self.var.analyse(compiler)
    self.range.analyse(compiler)
    self.body.analyse(compiler)
    
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
  
  def optimize(self, env, compiler):
    free_vars = self.free_vars()
    assigns = []
    for var in free_vars:
      value = env[var]
      if value is None: continue
      assigns.append(Assign(var, value))
      del env[var]
    return begin(*(tuple(assigns) + (For(self.var, self.range.optimize(env, compiler), self.body.optimize(env, compiler)),)))

  def insert_return_statement(self):
    return For(self.var, self.range, self.body.insert_return_statement())
  
  def replace_return_with_yield(self):
    return For(self.var, self.range, self.body.replace_return_with_yield())
  
  def pythonize(self, env, compiler):
    var, has_statement1 = self.var.pythonize(env, compiler)
    range, has_statement1 = self.range.pythonize(env, compiler)
    body, has_statement2 = self.body.pythonize(env, compiler)
    return (For(var[-1], range[-1], begin(*body)),), True
    
  def to_code(self, compiler):
    return 'for %s in %s:\n%s\n' % (self.var.to_code(compiler), 
                                    self.range.to_code(compiler), 
                                  compiler.indent(self.body.to_code(compiler)))
           
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.range==y.range and x.body==y.body
  
  def __repr__(self):
    return 'il.For(%r, %r, %r)'%(self.var, self.range, self.body)

