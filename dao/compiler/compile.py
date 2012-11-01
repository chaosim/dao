# -*- coding: utf-8 -*-

import dao.compiler.interlang as il

class CompileTypeError: 
  def __init__(self, exp):
    self.exp = exp
    
  def __repr__(self):
    return 'CompileTypeError: %s'%repr(self.exp)

class VariableNotBound: 
  def __init__(self, var):
    self.var = var
    
  def __repr__(self):
    return 'VariableNotBound: %s'%repr(self.var)

v, fc = il.Var('v'), il.Var('fc')

class Compiler:
  def __init__(self):
    pass
    
  def cps_convert(self, exp, cont, fcont):
    try: 
      exp_cps_convert = exp.cps_convert
      
    except: 
      if isinstance(exp, tuple) or  isinstance(exp, list) or\
         isinstance(exp, int) or isinstance(exp, float) or\
         isinstance(exp, str) or isinstance(exp, unicode):
        return cont(exp, fcont)
      else: raise CompileTypeError(exp)
      
    return exp_cps_convert(self, cont, fcont)
  
  def cps_convert_exps(self, exps, cont, fcont):
    if not exps: return il.Clamda(v, fc, cont(il.tuple(), fc))
    if len(exps)==1:
      return self.cps_convert(exps[0], cont, fcont)
    else:
      return self.cps_convert(exps[0], 
                  il.Clamda(v, fc, self.cps_convert_exps(exps[1:], cont, fcont)), fcont)
 
#α-conversion
#Alpha-conversion, sometimes known as alpha-renaming,[11] allows bound variable names to be changed. 
#For example, alpha-conversion of λx.x might yield λy.y. 
#Terms that differ only by alpha-conversion are called α-equivalent. Frequently in uses of lambda calculus, 
#α-equivalent terms are considered to be equivalent.
#The precise rules for alpha-conversion are not completely trivial. First, when alpha-converting an abstraction, 
#the only variable occurrences that are renamed are those that are bound to the same abstraction. 
#For example, an alpha-conversion of λx.λx.x could result in λy.λx.x, but it could not result in λy.λx.y. 
#The latter has a different meaning from the original.
#Second, alpha-conversion is not possible if it would result in a variable getting captured by a different abstraction. 
#For example, if we replace x with y in λx.λy.x, we get λy.λy.y, which is not at all the same.
#In programming languages with static scope, alpha-conversion can be used to make name resolution simpler 
#by ensuring that no variable name masks a name in a containing scope 
#(see alpha renaming to make name resolution trivial).

#Some compilers include an alpha-conversion stage to rename all program variables 
#such that variable names become unique. 
#(This simplifies subsequent processing somewhat.)

class AlphaConvertEnvironment:
  def __init__(self, outer=None, newvar_map=None):
    self.lefts = set() #left of assign after alpha convert
    self.bindings = {}
    self.outer = outer
    if outer is None: newvar_map = {}
    self.newvar_map = newvar_map
  
  def extend(self):
    return AlphaConvertEnvironment(self, self.newvar_map)
  
  def __getitem__(self, var):
    try:
      return self.bindings[var]
    except:
      if self.outer is not None:
        return self.outer[var]
      else:
        raise VariableNotBound(var)
     
  def new_var(self, var):
    try: 
      suffix = ''+repr(self.newvar_map[var.name])
      self.newvar_map[var.name] += 1
      return var.__class__(var.name+suffix)
    except:
      self.newvar_map[var.name] = 1
      return var
  
    # if the definition for same method of different class can be put in one place,
    # then the if_elif can be avoided, meanwhile the code is more understandable.
  def alpha_convert(self, exp):
    '''alpha convert expresson based on alpha equation, renaming all variables in different scopes to different names.
    calculating the references and assigns of variables in the meanwhile.
    # todo: code size and line number '''
    
    if isinstance(exp, il.Lamda):
      try:
        exp.is_alpha_converted
        return exp
      except: exp.is_alpha_converted = True
      
      new_env = self.extend()
      for p in exp.params: 
        new_env.bindings[p] = new_env.new_var(p)
      result = il.Lamda(tuple(new_env[p] for p in exp.params),
                   *tuple(new_env.alpha_convert(x) for x in exp.body))
      result.variables = new_env.bindings.values()
      result.lefts = new_env.lefts # prepare for assign convert
      return result
    
    if isinstance(exp, il.Var):
      return self[exp]
    
    elif  isinstance(exp, il.Apply):
      return il.Apply(env.alpha_convert(exp.caller), 
                   tuple(env.alpha_convert(arg) for arg in exp.args))
    
    elif  isinstance(exp, il.Return):
      return il.Return(tuple(env.alpha_convert(arg) for arg in exp.args))
    
    elif  isinstance(exp, il.Assign):
      converted_var = env.alpha_convert(exp.var)
      env.lefts.add(exp.var)
      return il.Assign(converted_var, env.alpha_convert(exp.exp))
    
    elif  isinstance(exp, il.If):
      return il.If(env.alpha_convert(exp.test), 
                   env.alpha_convert(exp.then), env.alpha_convert(exp.else_))
    
    elif  isinstance(exp, il.If2):
      return il.If2(env.alpha_convert(exp.test), env.alpha_convert(exp.then))
    
    #elif  isinstance(exp, il.Tuple):
      #return il.Tuple(exp.elements)
    
    elif isinstance(exp, il.Unify):
      return il.Unify(env.alpha_convert(exp.left), env.alpha_convert(exp.right),
                   env.alpha_convert(exp.cont), env.alpha_convert(exp.fcont))

    elif isinstance(exp, tuple):
      return tuple(env.alpha_convert(x) for x in exp)
    
    elif isinstance(exp, list):
      return [env.alpha_convert(x) for x in exp]
    
    elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
      return exp
    
    else: raise CompileTypeError(exp)

  def __repr__(self):
    result = ''
    while self is not None:
      result += repr(self.bindings)
      self = self.outer
    return result
       
def assign_convert(exp, alpha_env, env):
  # env is the AlphaConvertEnvironment after alpha convert
  
  if isinstance(exp, il.Lamda):
    try:
      exp.assign_convert
      return exp
    except: exp.assign_convert = True
    
    new_env = env.extend()
    for p in exp.lefts: 
      new_env.bindings[p] = alpha_env.new_var(p)
    make_cells = tuple((new_env[p], il.make_cell(p)) for p in exp.lefts)
    return il.Lamda(exp.params,
                 il.let(make_cells, tuple(assign_convert(x, alpha_env, new_env) for x in exp.body)))
  
  if isinstance(exp, il.Var):
    if exp in env:
      return il.contents(env[exp])
    else: return exp
    
  elif  isinstance(exp, il.Apply):
    return il.Apply(assign_convert(exp.caller, alpha_env, env), 
                 tuple(assign_convert(arg, alpha_env, env) for arg in exp.args))
  
  elif  isinstance(exp, il.Return):
    return il.Return(tuple(assign_convert(arg, alpha_env, env) for arg in exp.args))
  
  elif  isinstance(exp, il.Assign): # var = value, exp.exp should be a single var, 
                                    # which is the continuation param which ref to the value
    return il.set_contents(env[exp.var], exp.exp)
  
  elif  isinstance(exp, il.If):
    return il.If(assign_convert(exp.test, alpha_env, env), 
                 assign_convert(exp.then, alpha_env, env), assign_convert(exp.else_, alpha_env, env))
  
  elif  isinstance(exp, il.If2):
    return il.If2(assign_convert(exp.test, alpha_env, env), assign_convert(exp.then, alpha_env, env))
  
  elif  isinstance(exp, il.Tuple):
    return il.Tuple(exp.elements, alpha_env, env)
  
  elif isinstance(exp, il.Unify):
    return il.Unify(assign_convert(exp.left, alpha_env, env), assign_convert(exp.right, alpha_env, env),
                 assign_convert(exp.cont, alpha_env, env), assign_convert(exp.fcont, alpha_env, env))
  
  elif isinstance(exp, list):
    return [assign_convert(x, alpha_env, env) for x in exp]
  
  elif isinstance(exp, tuple):
    return tuple(assign_convert(x, alpha_env, env) for x in exp)
  
  elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
    return exp
  
  else: raise CompileTypeError(exp)

class Optimization:
  def __init__(self):
    self.ref_count = {}
    
def analyse_before_optimise(exp):
  result = Optimization()
  
  if isinstance(exp, il.Lamda):
    result.ref_count[exp] = result.ref_count.setdefault(exp, 0)+1
    return result
  
  if isinstance(exp, il.Var):
    exp.ref_count += 1
  
  elif  isinstance(exp, il.Apply):
    exp.caller.called_count += 1
    for arg in exp.args:
      analyse_before_optimise(arg)
  
  elif  isinstance(exp, il.Return):
    for arg in exp.args:
      analyse_before_optimise(arg)
  
  elif  isinstance(exp, il.Assign):
    analyse_before_optimise(exp.exp)
  
  elif  isinstance(exp, il.If):
    analyse_before_optimise(exp.test)
    analyse_before_optimise(exp.then)
    analyse_before_optimise(exp.else_)
  
  elif  isinstance(exp, il.If2):
    analyse_before_optimise(exp.test)
    analyse_before_optimise(exp.then)
  
  elif  isinstance(exp, il.Tuple):
    for arg in exp.elements:
      analyse_before_optimise(arg)
  
  elif isinstance(exp, il.Unify):
    analyse_before_optimise(exp.left)
    analyse_before_optimise(exp.right)
    analyse_before_optimise(exp.cont)
    analyse_before_optimise(exp.fcont)

  elif isinstance(exp, tuple):
    return tuple(env.alpha_convert(x) for x in exp)
  
  elif isinstance(exp, list):
    return [env.alpha_convert(x) for x in exp]
  
  elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
    return exp
    
  else: raise CompileTypeError(exp)
  
def optimize(exp):
  if isinstance(exp, il.Lamda):
    return nexp
  
  if isinstance(exp, il.Var):
    return exp
  
  elif  isinstance(exp, il.Apply):
    #1. ((lambda () body))  =>  body 
    if isinstance(exp.caller, il.Lamda):
      for p in exp.caller.params:
        if p.ref_count!=0:
          break
      else:
        result = exp.caller.body
        return result
      
      #2. (lamda x: ...x...)(y) => (lambda : ... y ...)() 
      # a(...y...), 且a(lamda ...x...: ...x...), then convert as above if code size is satisfied. 
      subst = {}
      new_params, new_args = (), ()
      for i, p in enumerate(exp.caller.params):
        if p.ref_count==1:
          subst[(i,p)] = exp.args[i]
        else:
          new_params += (p,)
          new_args += (exp.args[i],)
      if subst:
        return il.Apply(il.Lamda(new_params, exp.caller.body.subst(subst)), new_args)
        
    
  #3. Constant folding of primitive operations, including conditionals.
  
  #4. Substituting values that are defined in this module or in another module whose declaration file (see 
  #section 3.6) is being used in the compilation,
  
  #5. Removing unused arguments to procedures. Note that as a result of CPS conversion, arguments cannot 
  #be calls and thus cannot have side-effects. 
  
  #6. Removing side-effect free calls whose results are not used.
  
  #7. Various optimizations specific to primitive operations, involving multiple value returns, mutually recur- 
  #sive definitions, etc.
  
  #8. Transformations of conditional expressions to effect “boolean short-circuiting;” i.e. the evaluation of a 
  #conditional expression for an effect rather than a value. More specifically:     
  #a. (if (if a b c> d e) => (if a (if b d e> (if c d e>> 
  #But to avoid duplicating d and e the actual transformation is: 
  #(let ((x (lambda () d)) 
  #(y (lambda 0 e)>> 
  #(if a (if b d (x>> (if c d (y))>) 
  #The simpler version is used in most of the examples below. 
  #b. The results of tests are propagated: 
  #(if a (if a b c) d) => (if a b d) 
  #(if a b (if a c d)) => (if a b d) 
  #etc. 
  #c. (if (let (( .. .>> a> b c) => (let ((...)) (if a b c))   
  
  #a. (if (if a b c> d e) => (if a (if b d e> (if c d e>> 
  #But to avoid duplicating d and e the actual transformation is: 
  #(let ((x (lambda () d)) 
  #(y (lambda 0 e)>> 
  #(if a (if b d (x>> (if c d (y))>) 
  #The simpler version is used in most of the examples below. 
  #b. The results of tests are propagated: 
  #(if a (if a b c) d) => (if a b d) 
  #(if a b (if a c d)) => (if a b d) 
  #etc. 
  #c. (if (let (( ...>> a> b c) => (let ((...)) (if a b c)) 
  #e following two examples demonstrate how the transformations on conditionals perform boolean short- 
  #ting. In the first example, not is defined as a procedure (and is exactly how it is defined by Orbit). A simple 
  #itional expression using not is then simplified by the above transformations. 
  #not == (lambda (x) (if x nil t)) . Definition of the procedure NOT ,   
  #(if (not x) 0 1) 
  #=> (if (if x nil t) 0 1) ; Substitute the definition of NOT 
  #=> (if x (if nil 0 1) (if t 0 1)) ; (if (if .. .) .. .) 
  #=> (if x I 0) ; (if nil ab) => b, (if tab) => a 
  #The second example uses and and or, which are defined as macros by Orbit: 
  #(and x y) => (if x y nil) ; Definition of the macro AND 
  #(or x y) => (let ((p x) ; Definition of the macro OR 
  #(r (lambda 0 y>> > 
  #(if p p h-1)) 
  #The definition of or is complicated by the desire not to evalute x twice and to avoid variable conflicts between the 
  #introduced variable p and the expression y. Now a detailed example: 
  #(If (or (and x y) z) 0 1) 
  #=> (if (let ((p (if x y nil)) 
  #(r (lambda 0 z>>> 
  #(if p p b-1)) 
  #0 1) 
  #* Expand OR and AND , 
  #=> (let ((p (if x y nil))) ; Move the LET out of the IF 
  #(if (if p p ((lambda () 2)) 0 1))) ; and substitute R 
  #=> (let ((p (if x y nil) )) ; (if (if ..) .. .) 
  #(if p (if p 0 I) (if z 0 I))) 
  #=> (if (if x y nil) 0 (if z 0 1)) . Substitute P (it occurs just once) 
  #=> (let ((s (lambda () (if z 0 1)))) I (if (if ...) ...) 
  #(if x (if y 0 (s)) (if nil 0 63)))) 
  #=> (let ((s (lambda 0 (if z 0 1)))) ; (if nil a b) => a 
  #(if x (if y 0 (s)) (s))) 
  #The final result of this example may look less than optimal. However, since calling a known procedure with no 
  #arguments compiles as a jump, the above code is actually equivalent to: 
  #if X then if Y then 0 
  #else got0 S 
  #else got0 S 
  #S: if Z then 0 
  #else 1 
  #which is the code one would hope for when compiling (if (or (and x y) z) 0 1). Note that this code is 
  #produced without any special information about and or or. Any similar control construct that is translated into 
  #These transformations are in most respects the same as those described in [14] and [2], but they are performed on 
  #CPS code instead of source code (the examples above were not shown in CPS only to make them easier to read). 
  #The organization and simplicity that CPS and assignment conversion bring to the intermediate code allows a few 
  #fairly simple transformations such as these to produce highly optimized code. Work in progress on more complex 
  #optimizations (such as type inference) indicates that the structure of the intermediate code will continue to be 
  #beneficial. 

  elif  isinstance(exp, il.Return):
    return il.Return(tuple(env.alpha_convert(arg) for arg in exp.args))
  
  elif  isinstance(exp, il.Assign):
    converted_var = env.alpha_convert(exp.var)
    env.lefts.add(exp.var)
    return il.Assign(converted_var, env.alpha_convert(exp.exp))
  
  elif  isinstance(exp, il.If):
    return il.If(env.alpha_convert(exp.test), 
                 env.alpha_convert(exp.then), env.alpha_convert(exp.else_))
  
  elif  isinstance(exp, il.If2):
    return il.If2(env.alpha_convert(exp.test), env.alpha_convert(exp.then))
  
  elif  isinstance(exp, il.Tuple):
    return il.Tuple(exp.elements)
  
  elif isinstance(exp, tuple):
    return tuple(self.to_code(x) for x in exp)
  elif isinstance(exp, list):
    return [self.to_code(x) for x in exp]
  
  elif isinstance(exp, il.Unify):
    return il.Unify(env.alpha_convert(exp.left), env.alpha_convert(exp.right),
                 env.alpha_convert(exp.cont), env.alpha_convert(exp.fcont))
  
  elif isinstance(exp, tuple):
    return tuple(env.alpha_convert(x) for x in exp)
  
  elif isinstance(exp, list):
    return [env.alpha_convert(x) for x in exp]
  
  elif isinstance(exp, int) or isinstance(exp, float) or isinstance(exp, str) or isinstance(exp, unicode):
    return exp
  
    
  else: raise CompileTypeError(exp)
  
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

#Beta Conversion

#-Conversion primarily consists of the process of substituting a bound variable in the body of a lambda abstraction 
# by the argument passed to the function whenever it is applied. This process is called -reduction.
#In the context of functional programming languages, inline expansion is usually followed by the beta-reduction transformation.
def beta(exp):
  pass

class CodeGenerator: 
  def __init__(self, indent_space='  ', language='python'):
    self.language = language
    self.indent_space = indent_space
    self.var_index_map = {'function':0}
    self.var_index = 0
    
  def indent(self, code, level=1):
    lines = code.split('\n')
    lines = tuple(self.indent_space*level + line for line in lines)
    return '\n'.join(lines)
  
  def newvar(self, kind='function'):
    if kind=='function':
      self.var_index_map[kind] += 1
      return 'function'+repr(self.var_index_map[kind])
    self.var_index += 1
    return 'x'+repr(self.var_index)
    
  # if the definition for same method of different class can be put in one place,
  # then the if_elif can be avoided, meanwhile the code is more understandable.
  def to_code(self, exp):
    #if isinstance(exp, il.Clamda):
      #if exp.name is None: 
        #exp.name = self.newvar()
        #head = "def %s(%s):\n" % (exp.name, ', '.join(self.to_code_list(exp.params)))
        #return  head + self.indent('\n'.join(self.to_code_list(exp.body)))
      #else: return exp.name
    if isinstance(exp, il.Lamda):
      if exp.name is None: 
        exp.name = self.newvar()
        head = "def %s(%s):\n" % (exp.name, ', '.join(self.to_code_list(exp.params)))
        return  head + self.indent('\n'.join(self.to_code_list(exp.body)))
      else: return exp.name
    if isinstance(exp, il.BinaryOperationApply):
      return '%s%s%s'%(self.to_code(exp.args[0]), 
                          self.to_code(exp.caller), 
                          self.to_code(exp.args[1]))
    if isinstance(exp, il.BinaryOperation):
      return exp.operator
    elif  isinstance(exp, il.Apply):
      if isinstance(exp.caller, il.Clamda) and exp.caller.name is None:
        return self.to_code(exp.caller) + '\n' + \
               self.to_code(exp.caller)+'(%s)'%', '.join([self.to_code(x) for x in exp.args])
      else: 
        return exp.caller.name + '(%s)'%', '.join([self.to_code(x) for x in exp.args])
    elif  isinstance(exp, il.Return):
      return  'return %s' % ', '.join([self.to_code(x) for x in exp.args])
    elif  isinstance(exp, il.Assign):
      return  '%s = %s' % (self.to_code(exp.var), self.to_code(exp.exp))
    elif  isinstance(exp, il.If):
      return 'if %s: \n%s\nelse:\n%s' % (self.to_code(exp.test), self.indent(self.to_code(exp.then)), 
                                 self.indent(self.to_code(exp.else_)))
    elif  isinstance(exp, il.If2):
      return 'if %s: \n%s\n' % (self.to_code(exp.test), self.indent(self.to_code(exp.then)))
    #elif  isinstance(exp, il.Tuple):
      #if len(exp.element)!=1:
        #return "(%s)"%', '.join([self.to_code(x) for x in exp.elements])
      #else: 
        #return '(%s,)'%self.to_code(exp.elements[0])
    elif isinstance(exp, il.Unify):
      return 'unify(%s, %s, %s, %s)' % (self.to_code(exp.left), self.to_code(exp.right), 
                                       self.to_code(exp.cont), self.to_code(exp.fcont))
    elif isinstance(exp, il.Var):
      return exp.name
    elif isinstance(exp, il.LogicVar):
      return  "LogicVar(%s)"%exp.name
    #elif isinstance(exp, il.Literal):
      #return repr(exp.value)
    elif isinstance(exp, tuple):
      return '(%s)'%', '.join(tuple(self.to_code(x) for x in exp))
    elif isinstance(exp, list):
      return '(%s)'%', '.join(tuple(self.to_code(x) for x in exp))
    else:
      return repr(exp)
   
  def to_code_list(self, items):
    return [self.to_code(x) for x in items]  

