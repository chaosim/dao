# -*- coding: utf-8 -*-

pyset = set
pytype = type

from dao.compiler import env as compiler_env
from dao.compiler import vop

_varcache = {}
def var(name):
  return _varcache.setdefault(name, Var(name))

class Var:
  def __init__(self, name): 
    self.name = name
    
  def __call__(self, *args):
    return CommandCall(self, *args)
  
  def alpha(self, compiler):
    var = compiler.alpha_env[self]
    if var is self:
      var = compiler.new_var(self.name)
    return var

class CommandCall:
  def __init__(self, operator, *operand):
    self.operator = operator
    self.operand = operand
    
  def __call__(self, *args):
    return CommandCall(self, *args)
  
  def alpha(self, compiler):
    return CommandCall(compiler.alpha(self.operator), *compiler.alpha(self.operand))
  
  def compile_to_cont(self, cont, compiler):
    function = compiler.new_var('function')
    if len(self.operand)==0:
      return compiler.cont(self.operator, clambda(function, vop.return_(vop.call(function, cont))))
    args = tuple(compiler.new_var('arg') for _ in self.operand)
    k = clambda(args[-1], vop.return_(vop.call(function, (cont,)+args),))
    for a, e in reversed(zip((function,)+args[:-1], self.operand)):
      k = clambda(a, compiler.cont(e, k))
    return compiler.cont(self.operator, k)
  
  def __repr__(self): 
    return '%s(%s)'%(self.operator, ','.join([repr(e) for e in self.operand]))

class SpecialForm:
  def __call__(self, *exps): return CommandCall(self, *exps)

class quote(SpecialForm):
  def __init__(self, exp): self.exp = exp
  
  def alpha(self, compiler):
    return compiler.alpha(self.exp)
  
  def compile_to_cont(self, cont, compiler):
    return cont(self.exp)
  def __eq__(self, other): return self.exp==other.exp
  def __repr__(self): 
      return "'%s"%self.exp

# do not restore the value of var in assign
# if need to restore, use define instead.

from dao.compiler.cont import SetCont

# assign var in the most inner env
class set(SpecialForm):
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
  def alpha(self, compiler):
    var = compiler.alpha_env[self.var]
    if var is self.var: var = compiler.new_var(var.name)
    compiler.alpha_env = compiler.alpha_env.extend({self.var:var})
    return set(var, compiler.alpha(self.exp))
  def compile_to_cont(self, cont, compiler):
    value = compiler.new_var('value')
    return compiler.cont(self.exp, 
      clambda(value, begin(vop.set(self.var, value), vop.return_(value, cont))))
  
  def __eq__(self, other): return self.var==other.var and self.exp==other.exp
  def __repr__(self): return "set(%s, %s)"%(self.var, self.exp)
assign = set

class begin(SpecialForm):
  name = 'begin'
  def __init__(self, *exps):
    self.exps = exps
  def alpha(self, compiler):
    return begin(*compiler.alpha_exps(self.exps))
  def compile_to_cont(self, cont, compiler): 
    return compiler.exps_cont(self.exps, cont)
  def __eq__(self, other): 
    return isinstance(other, begin) and self.exps==other.exps
  def __repr__(self):
    return 'begin(%s)'%(';'.join([repr(x) for x in self.exps]))
  
class if_(SpecialForm):
  symbol = 'if'
  def __init__(self, test, exp1, exp2=None):
    self.test, self.exp1, self.exp2 = test, exp1, exp2
  
  def alpha(self, compiler):
    return if_(compiler.alpha(self.test), compiler.alpha(self.exp1), compiler.alpha(self.exp2))
  
  def compile_to_cont(self, cont, compiler):
    value = compiler.new_var('value')
    return compiler.cont(self.test, clambda(value, 
            if_(value, compiler.cont(self.exp1, cont), 
                compiler.cont(self.exp2, cont))))
  def __eq__(self, other):
    return isinstance(other, if_) and self.test==other.test\
           and self.exp1==other.exp1 and self.exp2==other.exp2
  def __repr__(self):
    els = 'else: %s'%repr(self.exp2) if self.exp2 else ''
    return 'if %s: %s%s'%(self.test, self.exp1, els)

class let(SpecialForm):
  symbol = 'let'
  def __init__(self, bindings, *body):
    self.bindings, self.body = tuple(bindings), body
    
  def alpha(self, compiler):
    old_env = compiler.alpha_env
    subst, set_bindings = {}, []
    for var, exp in self.bindings: 
      new_var = compiler.new_var(var.name)
      set_bindings.append(set(new_var, compiler.alpha(exp)))
      subst[var] = new_var
    compiler.alpha_env = old_env.extend(subst)
    result = begin(*(tuple(set_bindings)+compiler.alpha_exps(self.body)))
    compiler.alpha_env = old_env
    return result
            
  def __eq__(self, other):
    return self.bindings==other.bindings and self.body==other.body
  
  def __repr__(self):
    return 'let %s: %s'%(repr(self.bindings), self.body)
  
class letr(let):
  def alpha(self, compiler):
    old_env = compiler.alpha_env
    subst, set_bindings = {}, []
    for var, exp in self.bindings: 
      subst[var] = compiler.new_var(var.name)
    compiler.alpha_env = old_env.extend(subst)
    for var, exp in self.bindings: 
      set_bindings.append(set(subst[var], compiler.alpha(exp)))
    result = begin(*(tuple(set_bindings)+compiler.alpha_exps(self.body)))
    compiler.alpha_env = old_env
    return result
  
  def __repr__(self):
    return 'letr %s: %s'%(repr(self.bindings), self.body)
  
class lambda_(SpecialForm):
  def __init__(self, vars, *body):
    self.vars, self.body = vars, body
    
  def alpha(self, compiler):
    subst = dict([(var, compiler.new_var(var.name)) for var in self.vars])
    compiler.alpha_env = compiler.alpha_env.extend(subst)
    return lambda_(tuple(subst[var] for var in self.vars), *compiler.alpha_exps(self.body))
  
  def compile_to_cont(self, cont, compiler):
    k = compiler.new_var('k')
    return vop.return_(lambda_((k,)+self.vars, compiler.exps_cont(self.body, k)), cont)
  
  def __eq__(self, other):
    return isinstance(other, lambda_) and self.vars==other.vars and self.body==other.body
  
  def __repr__(self):
    return '(lambda %s: %s)'%(repr(self.vars), self.body)

def clambda(var, *body):
  return lambda_((var,), *body)

# unify, unify_list, or_p, or, cond,

'''
or_p(a, b, c):
  begin(
    save_fc(x0),
    set_fcont(clambda(x3, restore_fc(x0), compile.cont(c, cont)
    save_fc(x1),
    set_fcont(clambda(x2, 
    restore_fc(x1), compiler.cont(b, cont))
    compiler.cont(a, cont)
    )
or_p(a, b, c):
  begin(
    save_fc(x0), 
    set_fcont(clambda(x2, 
    restore_fc(clambda(x3, restore_fc(x0), compile.cont(c, cont)), compiler.cont(b, cont))
    compiler.cont(a, cont)
    )

and_p(a,b):
  begin(
    compiler.cont(a, clambda(x, compiler.cont(b, cont))))
and_p(a,b,c):
  begin(
    compiler.cont(a, clambda(x, compiler.cont(b, clambda_(y, compiler.cont(c, cont))))))

  or(a,b):
  compiler.cont(a, 
    clambda(x: 
      if x: return_(true, cont)
      else: compiler.cont(b, cont)
      
  cond:
  compiler.cont(test0,
  clambda(x, if x: compiler.cont(body1, cont)
  else: compiler.cont(cond[1:])
  
  unify(x, y):
    x1 = deref(x)
    y1 = deref(y)
    if isinstance(x1, LogicVar):
      save_fc(fc1)
      set_fcont(clambda(x2, x1.binding = x1, return (False, fc1))
      x1.binding = y1
    elif isinstance(y1, LogicVar):
      save_fc(fc1)
      set_fcont(clambda(x2, y1.binding = y1, return (False, fc1))
      y1.binding = x1
    elif x1!=y1:
      return (False, fc1)
    else:  
      return (True, cont)
  unify_list(x,y), cont
    compiler.cont(unify(x[0], y[0]), compiler.cont(unify_list(x[1:], y[1:]), cont))) 
'''