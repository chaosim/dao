# -*- coding: utf-8 -*-

import dao.compiler.interlang as il

v, fc = il.Var('v'), il.Var('fc')

class Compiler:
  def __init__(self):
    pass
    
  def compile(self, exp, cont, fcont):
    try: 
      exp_compile = exp.compile
    except: 
      return cont(il.literal(exp), fcont)
    return exp_compile(self, cont, fcont)
  
  def compile_exps(self, exps, cont, fcont):
    if not exps: return il.clamda(v, fc, cont(il.tuple(), fc))
    if len(exps)==1:
      return self.compile(exps[0], cont, fcont)
    else:
      return self.compile(exps[0], il.clamda(v, fc, self.compile_exps(exps[1:], cont, fcont)), fcont)
 
#pyeval = eval
#pytype = type

#from dao.base import is_subclass, is_var
#from dao.compiler.env import GlobalEnvironment
#from dao.env import EmptyEnvironment
#from dao.special import clambda

#from dao.compiler.term import Var as CompileVar
#from dao.compiler import vop


#from dao.base import classeq

#α-conversion
#Alpha-conversion, sometimes known as alpha-renaming,[11] allows bound variable names to be changed. For example, alpha-conversion of λx.x might yield λy.y. Terms that differ only by alpha-conversion are called α-equivalent. Frequently in uses of lambda calculus, α-equivalent terms are considered to be equivalent.
#The precise rules for alpha-conversion are not completely trivial. First, when alpha-converting an abstraction, the only variable occurrences that are renamed are those that are bound to the same abstraction. For example, an alpha-conversion of λx.λx.x could result in λy.λx.x, but it could not result in λy.λx.y. The latter has a different meaning from the original.
#Second, alpha-conversion is not possible if it would result in a variable getting captured by a different abstraction. For example, if we replace x with y in λx.λy.x, we get λy.λy.y, which is not at all the same.
#In programming languages with static scope, alpha-conversion can be used to make name resolution simpler by ensuring that no variable name masks a name in a containing scope (see alpha renaming to make name resolution trivial).

#Some compilers include an alpha-conversion stage to rename all program variables such that variable names become unique. 
#(This simplifies subsequent processing somewhat.)
def alpha(exp):
  pass

# find logic variable(free variable)

# eliminating free variables(Lambda lifting or closure conversion)
#Algorithm

#The following algorithm is one way to lambda-lift an arbitrary program in a language which doesn't support closures as first-class objects:
#Rename the functions so that each function has a unique name.
#Replace each free variable with an additional argument to the enclosing function, and pass that argument to every use of the function.
#Replace every local function definition that has no free variables with an identical global function.
#Repeat steps 2 and 3 until all free variables and local functions are eliminated.

# http://en.wikipedia.org/wiki/Closure_conversion

#If the language has closures as first-class objects that can be passed as arguments or returned from other functions (closures), 
# the closure will need to be represented by a data structure that captures the bindings of the free variables.

#def compile(e, k, f, env):
  #if isinstance(e, int): return (k, e)
  #if isinstance(e, str): return (k, e)
  #if isinstance(e, Var): return (k, e)
  #if isinstance(e, tuple):
    #if e[0]=='if':
      #return compile(e[2],  ('lambda', (v,), ('if', v, compile(e[1], k, f, env), compile(e[2], k, f, env)))), f, env)
    #if e[0]=='andp':
      #return compile(e[1],  compile(e[2], k, f, env), k, env)
    #if e[0]=='orp':
      #return compile(e[1],  k, compile(e[2], k, f, env), env)
    #if e[0]=='lambda':
      #return (k, clambda(e[1]+[kk], compile(e[2], kk, f, env)))

def beta(exp):
  pass

#Beta Conversion

#-Conversion primarily consists of the process of substituting a bound variable in the body of a lambda abstraction 
# by the argument passed to the function whenever it is applied. This process is called -reduction.
#In the context of functional programming languages, inline expansion is usually followed by the beta-reduction transformation.

#def gencode(e):
  #return
 
##====================================

def xmake_compiler():
  global_env = GlobalEnvironment({})
  env = global_env.extend({})
  return Compiler(global_env, env)

def xcompile_to_cont(exp):
  compiler = make_compiler()
  exp = compiler.alpha(exp)
  return compiler.cont(exp, vop.done)
  
def xcompile(exp): 
  sexp = to_sexpression(exp)
  compiler = make_compiler()
  return compiler.compile(sexp)

class XCompiler:
  def __init__(self, global_env, env):
    self.global_env = global_env
    self.env = env
    self.srcvar2internal = {}
    self.internal2srcvar = {}
    self.alpha_env = EmptyEnvironment()
  
  def new_var(self, name_root):
    var = CompileVar(name_root)
    if  var in self.internal2srcvar:
      i = 1
      var = CompileVar('%s_%s'%(name_root, i))
      while var in self.internal2srcvar:
        i += 1
        var = CompileVar('%s_%s'%(name_root, i))
    self.internal2srcvar[var] = name_root  
    return var
  
  def alpha(self, exp):
    try: exp_alpha = exp.alpha
    except: return exp
    return exp_alpha(self)
    
  def alpha_exps(self, exps):
    return tuple(self.alpha(exp) for exp in exps)
    
  def cont(self, exp, cont):
    try: exp_compile_to_cont = exp.compile_to_cont
    except: 
      value = self.new_var('value')
      return vop.return_(exp, cont)
    return exp_compile_to_cont(cont, self)
    
  def exps_cont(self, exps, cont):
    if len(exps)==0: return vop.return_(None, cont)
    elif len(exps)==1: return self.cont(exps[0], cont)
    else:
      value = self.new_var('value')
      return self.cont(exps[0], clambda(value,self.exps_cont(exps[1:], cont)))
      
  def compile(self, exp):
    self.cont(exp, self.stop_cont)
    result = ''
    for cont in self.root_set:
      result += cont.code()
    return result

#===============================================