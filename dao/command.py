# -*- coding: utf-8 -*-

from dao.base import classeq, Element
from dao.compilebase import CompileTypeError, VariableNotBound
import dao.interlang as il

def element(exp):
  if isinstance(exp, Element):
    return exp
  else:
    try: 
      return type_map[type(exp)](exp)
    except: 
      raise CompileTypeError(exp)

class Atom(Element):
  def __init__(self, item):
    self.item = item
    
  def alpha(self, env, compiler):
    return self
  
  def cps(self, compiler, cont):
    return cont(self.interlang())
    
  def quasiquote(self, compiler, cont):
    return cont(self.interlang())
  
  def subst(self, bindings):
    return self
  
  def interlang(self):
    return il.Atom(self.item)
  
  def __eq__(x, y):
    return x.__class__==y.__class__ and x.item==y.item
  
  def to_code(self, compiler):
    return '%s(%s)'%(self.__class__.__name__, repr(self.item))
  
  def __repr__(self):
    return '%s'%self.item

class Integer(Atom): 
  def __eq__(x, y):
    return Atom.__eq__(x, y) or (isinstance(y, int) and x.item==y)
  
  def interlang(self):
    return il.Integer(self.item)
  
  
class Float(Atom): 
  def __eq__(x, y):
    return Atom.__eq__(x, y) or (isinstance(y, float) and x.item==y)
  
  def interlang(self):
    return il.Float(self.item)
  
class String(Atom): 
  def __eq__(x, y):
    return Atom.__eq__(x, y) or (isinstance(y, str) and x.item==y)
  
  def interlang(self):
    return il.String(self.item)
  
class List(Atom): 
  def __eq__(x, y):
    return Atom.__eq__(x, y) or (isinstance(y, list) and x.item==y)
  
  def interlang(self):
    return il.List(self.item)
  
class Dict(Atom): 
  def __eq__(x, y):
    return Atom.__eq__(x, y) or (isinstance(y, dict) and x.item==y)
  
  def interlang(self):
    return il.Dict(self.item)
  
class Bool(Atom): 
  def __eq__(x, y):
    return Atom.__eq__(x, y) or (isinstance(y, bool) and x.item==y)
  
  def interlang(self):
    return il.Bool(self.item)
  

class Symbol(Atom): 
  def __eq__(x, y):
    return classeq(x, y) and x.item==y.item
  
  def interlang(self):
    return il.Symbol(self.item)

class Klass(Atom):
  def __repr__(self):
    return 'Klass(%s)'%(self.item)
  
  def interlang(self):
    return il.Klass(self.item)

class PyFunction(Atom):
  def __repr__(self):
    return 'PyFunction(%s)'%(self.item)
  
  def interlang(self):
    return il.PyFunction(self.item)
  
TRUE = Bool(True)
FALSE = Bool(False)
NONE = Atom(None)

def make_tuple(value):
  return Tuple(*tuple(element(x) for x in value))

class Tuple(Atom): 
  def __init__(self, *items):
    self.item = items
  
  def interlang(self):
    return il.Tuple(*tuple(x.interlang() for x in self.item))
  
  def to_code(self, compiler):
    return '%s(%s)'%(self.__class__.__name__,', '.join([repr(x) for x in self.item]))
  
  def __iter__(self):
    return iter(self.item)
  
  def __repr__(self):
    return '%s(%s)'%(self.__class__.__name__, self.item)

class Var(Element):  
  def __init__(self, name):
    self.name = name
        
  def __call__(self, *args):
    return Apply(self, tuple(element(arg) for arg in args))
  
  def alpha(self, env, compiler):
    return env[self]
    
  def subst(self, bindings):  
    try: return bindings[self]
    except: return self
      
  def cps(self, compiler, cont):
    return cont(self.interlang())
  
  def cps_convert_unify(x, y, compiler, cont):
    try: 
      y.cps_convert_unify
    except:
      x = x.interlang()
      y = y.interlang()      
      x1 = compiler.new_var(il.ConstLocalVar(x.name))
      return il.begin(
        il.Assign(x1, il.Deref(x)), #for LogicVar, could be optimized when generate code.
        il.If(il.IsLogicVar(x1),
           il.begin(il.SetBinding(x1, y),
                 il.append_failcont(compiler, il.DelBinding(x1)),
                 cont(il.TRUE)),
                il.If(il.Eq(x1, y), cont(il.TRUE), il.failcont(il.TRUE))))
    x = x.interlang()
    y = y.interlang()      
    x1 = compiler.new_var(il.ConstLocalVar(x.name))
    y1 = compiler.new_var(il.ConstLocalVar(y.name))
    return il.begin(
      il.Assign(x1, il.Deref(x)), #for LogicVar, could be optimized when generate code.
      il.Assign(y1, il.Deref(y)),
      il.If(il.IsLogicVar(x1),
         il.begin(il.SetBinding(x1, y1),
               il.append_failcont(compiler, il.DelBinding(x1)),
               cont(il.TRUE)),
         il.begin(
           il.If(il.IsLogicVar(y1),
              il.begin(il.SetBinding(y1, x1),
                    il.append_failcont(compiler, il.DelBinding(y1)),
                    cont(il.TRUE)),
              il.If(il.Eq(x1, y1), cont(il.TRUE), il.failcont(il.TRUE))))))
  
  def cps_call(self, compiler, cont, args):
    # see The 90 minute Scheme to C compiler by Marc Feeley
    raise CompileTypeError(self)
  
    function = compiler.new_var(il.ConstLocalVar('function'))
    vars = tuple(compiler.new_var(il.ConstLocalVar('a'+repr(i))) for i in range(len(args)))
    body = il.Apply(function, (cont,)+vars)
    for var, item in reversed(zip(vars, args)):
      body = item.cps(compiler, il.clamda(var, body)) 
    v = compiler.new_var(il.ConstLocalVar('v'))
    macro_args1 = tuple(il.ExpressionWithCode(arg, 
                                      il.Lamda((), arg.cps(compiler, il.clamda(v, v)))) 
                                for arg in args)
    macro_args2 = il.macro_args(macro_args1)
    return self.cps(compiler, il.clamda(function,
                  il.If(il.IsMacro(function),
                        il.If(il.IsMacroRules(function),
                              il.Apply(function, (cont, macro_args2)),
                              il.Apply(function, (cont,)+macro_args1)),
                        body)))
  
  def interlang(self):
    return il.Var(self.name)
  
  def free_vars(self):
    return set([self])
  
  def to_code(self, compiler):
    return "DaoVar('%s')"%self.name
  
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  def hash(self):
    return hash(self.name)
  
  def __repr__(self):
    return "%s('%s')"%(self.__class__.__name__, self.name)

class Const(Var):
  def interlang(self):
    return il.ConstLocalVar(self.name)
  
class LamdaVar(Var):
  def cps_call(self, compiler, cont, args):
    #function = compiler.new_var(il.ConstLocalVar('function'))
    function = self.interlang()
    vars = tuple(compiler.new_var(il.ConstLocalVar('a'+repr(i))) 
                 for i in range(len(args)))
    body = il.Apply(function, (cont,)+vars)
    for var, item in reversed(zip(vars, args)):
      body = item.cps(compiler, il.clamda(var, body)) 
    v = compiler.new_var(il.ConstLocalVar('v'))
    return self.cps(compiler, il.clamda(function,body))
    
class MacroVar(Var): 
  def cps_call(self, compiler, cont, args):
    function = self.interlang()
    k = compiler.new_var(il.ConstLocalVar('cont'))
    v = compiler.new_var(il.ConstLocalVar('v'))
    #macro_args = tuple(il.Lamda((), arg.cps(compiler, il.clamda(v, v))) 
                                #for arg in args)
    macro_args = tuple(il.Lamda((k,), arg.cps(compiler, k)) for arg in args)
    return self.cps(compiler, 
                            il.clamda(function, il.Apply(function, (cont,)+macro_args)))
  
class ConstLamdaVar(LamdaVar, Const): 
  def interlang(self):
    return il.ConstLocalVar(self.name)  

class ConstMacroVar(MacroVar, Const): 
  def interlang(self):
    return il.ConstLocalVar(self.name)

class RecursiveFunctionVar(ConstLamdaVar):
  def interlang(self):
    return il.RecursiveVar(self.name)

class RecursiveMacroVar(ConstMacroVar): 
  def interlang(self):
    return il.RecursiveVar(self.name)
  
class LogicVar(Var):  
  def alpha(self, env, compiler):
    return self
  
  def interlang(self):
    return il.LogicVar(self.name)
  
  def cps(self, compiler, cont):
    return cont(il.LogicVar(self.name))
  
  def to_code(self, compiler):
    return "DaoLogicVar('%s')"%self.name
  
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  def __repr__(self):
    return "DaoLogicVar('%s')"%self.name 
  
class DummyVar(LogicVar):
  def interlang(self):
    return il.DummyVar(self.name)
  
  def cps(self, compiler, cont):
    return cont(il.Deref(il.DummyVar(self.name)))

  def to_code(self, compiler):
    return "DaoDummyVar('%s')"%self.name
  
def cons(head, tail):
  return Cons(element(head), element(tail))

class Cons(Element): 
  def __init__(self, head, tail):
    self.head, self.tail = head, tail
    
  def alpha(self, env, compiler):
    return Cons(self.head.alpha(env, compiler),
                self.tail.alpha(env, compiler))
  
  def cps(self, compiler, cont): 
    return cont(self.interlang())
  
  def interlang(self):
    return il.Cons(self.head.interlang(), self.tail.interlang())
  
  def cps_convert_unify(x, y, compiler, cont):
    return cps_convert_unify(x, y, compiler, cont)
  
  def unify_rule_head(self, other, env, subst):
    if self.__class__!=other.__class__: return
    for _ in unify_rule_head(self.head, other.head, env, subst):
      for _ in unify_rule_head(self.tail, other.tail, env, subst):
        yield True
          
  def copy_rule_head(self, env):
    head = copy_rule_head(self.head, env)
    tail = copy_rule_head(self.tail, env)
    if head==self.head and tail==self.tail: return self
    return Cons(head, tail)

  def getvalue(self, env):
    head = getvalue(self.head, env)
    tail = getvalue(self.tail, env)
    if head==self.head and tail==self.tail:
      return self
    return Cons(head, tail)
  
  def copy(self, memo): 
    return Cons(copy(self.head, memo), copy(self.tail, memo))
  
  def __eq__(self, other): 
    return self.__class__==other.__class__ \
           and self.head==other.head and self.tail==other.tail
     
  def __iter__(self):
    tail = self 
    while 1:
      yield tail.head
      if tail.tail is nil: return
      elif isinstance(tail.tail, Cons): 
        tail = tail.tail
      else: 
        yield tail.tail
        return
      
  def __len__(self): 
    return len([e for e in self])
  
  def __repr__(self): 
    return 'L(%s)'%' '.join([repr(e) for e in self])

class Nil(Element): 
  def alpha(self, env, compiler):
    return self
  
  def interlang(self):
    return il.nil
  
  def __len__(self): 
    return 0
  
  def __iter__(self): 
    if 0: yield
    
  def __repr__(self): return 'nil'
  
nil = Nil()

def conslist(*elements):
  result = nil
  for term in reversed(elements): result = Cons(element(term), result)
  return result

def cons2tuple(item):
  if not isinstance(item, Cons) and not isinstance(item, list) \
     and not isinstance(item, tuple): 
    return item
  return tuple(cons2tuple(x) for x in item)

def cps_convert_unify_two_var(x, y, compiler, cont):
  x = x.interlang()
  y = y.interlang()      
  x1 = compiler.new_var(il.ConstLocalVar(x.name))
  y1 = compiler.new_var(il.ConstLocalVar(y.name))
  return il.begin(
    il.Assign(x1, il.Deref(x)), #for LogicVar, could be optimized when generate code.
    il.Assign(y1, il.Deref(y)),
    il.If(il.IsLogicVar(x1),
       il.begin(il.SetBinding(x1, y1),
             il.append_failcont(compiler, il.DelBinding(x1)),
             cont(il.TRUE)),
       il.begin(
         il.If(il.IsLogicVar(y1),
            il.begin(il.SetBinding(y1, x1),
                  il.append_failcont(compiler, il.DelBinding(y1)),
                  cont(il.TRUE)),
            il.If(il.Unify(x1, y1), cont(il.TRUE), il.failcont(il.TRUE))))))

def cps_convert_unify_one_var(x, y, compiler, cont):
  x = x.interlang()
  y = y.interlang()      
  x1 = compiler.new_var(il.ConstLocalVar(x.name))
  return il.begin(
    il.Assign(x1, il.Deref(x)), #for LogicVar, could be optimized when generate code.
    il.If(il.IsLogicVar(x1),
       il.begin(il.SetBinding(x1, y),
             il.append_failcont(compiler, il.DelBinding(x1)),
             cont(il.TRUE)),
            il.If(il.Unify(x1, y), cont(il.TRUE), il.failcont(il.TRUE))))

def cps_convert_unify(x, y, compiler, cont):
  if isinstance(x, Var):
    if isinstance(y, Var):
      return cps_convert_unify_two_var(x, y, compiler, cont)
    else:
      return cps_convert_unify_one_var(x, y, compiler, cont)
  else:
    if isinstance(y, Var):
      return cps_convert_unify_two_var(y, x, compiler, cont)
    else:
      if isinstance(x, Cons) and isinstance(y, Cons):
        v = compiler.new_var(il.ConstLocalVar('v'))
        return cps_convert_unify(x.head, y.head, compiler, il.clamda(v, 
                    cps_convert_unify(x.tail, y.tail, compiler, cont)))
      else:
        if x==y:
          return cont(il.TRUE)
        else:
          return il.failcont(il.FALSE)

class Apply(Element):
  def __init__(self, caller, args):
    self.caller, self.args = caller, args

  def alpha(self, env, compiler):
    return self.__class__(self.caller.alpha(env, compiler), 
                 tuple(arg.alpha(env, compiler) for arg in self.args))
  
  def cps(self, compiler, cont):
    # see The 90 minute Scheme to C compiler by Marc Feeley
    return self.caller.cps_call(compiler, cont, self.args)

  def subst(self, bindings):  
    return self.__class__(self.caller.subst(bindings), 
                 tuple(arg.subst(bindings) for arg in self.args))
      
  def __repr__(self):
    return '%r(%s)'%(self.caller, ', '.join([repr(x) for x in self.args]))
  
class Command(Element): pass

class CommandCall(Element): 
  def __init__(self, function, args):
    self.function, self.args = function, args
    
  def subst(self, bindings):
    return self.__class__(self.function, 
                 tuple(arg.subst(bindings) for arg in self.args))
  
  def quasiquote(self, compiler, cont):
    result = compiler.new_var(il.LocalVar('result'))
    vars = tuple(compiler.new_var(il.ConstLocalVar('a'+repr(i))) for i in range(len(self.args)))
    body = (il.Assign(result, il.empty_list),)+tuple(
      il.If(il.Isinstance(var, il.Klass('UnquoteSplice')),
                  il.AddAssign(result, il.Call(il.Symbol('list'), il.Attr(var, il.Symbol('item')))),
                  il.ListAppend(result, var),
                  ) for var in vars)+(
      cont(il.Call(il.Klass(self.__class__.__name__), il.QuoteItem(self.function), il.MakeTuple(result))),)
    fun = il.begin(*body)
    for var, arg in reversed(zip(vars, self.args)):
      fun = arg.quasiquote(compiler, il.clamda(var, fun))
    return fun
    
  def __eq__(x, y):
    return classeq(x, y) and x.function==y.function and x.args==y.args
  
  def __repr__(self):
    return '%r(%s)'%(self.function, ', '.join([repr(x) for x in self.args]))

class Special(Command):
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    args = tuple(element(arg) for arg in args)
    return SpecialCall(self, args)
  
  def __repr__(self):
    return self.function.__name__

special = Special

def quasiquote_args(self, args):
  if not args: yield ()
  elif len(args)==1: 
    for x in self.quasiquote(args[0]):
      try: yield x.unquote_splice
      except: yield (x,)
  else:
    for x in self.quasiquote(args[0]):
      for y in self.quasiquote_args(args[1:]):
        try: x = x.unquote_splice
        except: x = (x,)
        yield x+y
          
class SpecialCall(CommandCall):
  def __init__(self, command, args):
    self.command = command
    self.function = command.function
    self.args = args
    
  def alpha(self, env, compiler):
    return self.__class__(self.command,
                 tuple(arg.alpha(env, compiler) for arg in self.args))
  
  def cps(self, compiler, cont):
    return self.function(compiler, cont, *self.args)
  
  def to_code(self, compiler):
    return '%s(%s)'%(self.function.__name__, ', '.join([x.to_code(compiler) for x in self.args]))
    
  def free_vars(self):
    result = set()
    for arg in self.args:
      result |= arg.free_vars()
    return result
  
  def __repr__(self):
    return '%s(%s)'%(self.function.__name__, 
                     ', '.join(tuple(repr(x) for x in self.args)))

class BuiltinFunction(Command):
  is_statement = False
  def __init__(self, name, function):
    self.name, self.function = name, function
    
  def __call__(self, *args):
    args = tuple(element(arg) for arg in args)
    return BuiltinFunctionCall(self, args)
  
  def cps(self, compiler, cont):
    return il.Lamda((params), self.function.function(*params))
  
  def analyse(self, compiler):  
    return self
  
  def subst(self, bindings):
    return self
  
  def optimize(self, env, compiler):
    return self
  
  def pythonize(self, env, compiler):
    return (self, ), False
  
  def __repr__(self):
    return self.name
  
class BuiltinFunctionCall(CommandCall):
  is_statement = False
  def alpha(self, env, compiler):
    return self.__class__(self.function,
                 tuple(arg.alpha(env, compiler) for arg in self.args))
  
  def cps(self, compiler, cont):
    #see The 90 minute Scheme to C compiler by Marc Feeley
    args = self.args
    vars = tuple(compiler.new_var(il.ConstLocalVar('a'+repr(i))) for i in range(len(args)))
    fun = cont(self.function.function(*vars))
    for var, arg in reversed(zip(vars, args)):
      fun = arg.cps(compiler, il.Clamda(var, fun))
    return fun

  def analyse(self, compiler):
    # unquote to interlang level
    return
  
  def optimize(self, env, compiler):
    return self
  
  def interlang(self):
    return self
  
  def free_vars(self):
    result = set()
    for arg in self.args:
      result |= arg.free_vars()
    return result
  
  def pythonize(self, env, compiler):
    return (self,), False
  
  def to_code(self, compiler):
    return '%s(%s)'%(self.function.name, ', '.join([x.to_code(compiler) for x in self.args]))
     
  def __repr__(self):
    return '%s(%s)'%(self.function.name, ', '.join([repr(x) for x in self.args]))

def assign(var, exp):
  return Assign(var, element(exp))

class MultiAssignToConstError: 
  def __init__(self, const):
    self.const = const
    
  def __repr__(self):
    return repr(self.const)

class Assign(CommandCall):
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
    
  def subst(self, bindings):
    return Assign(self.var, self.exp.subst(bindings))
    
  def alpha(self, env, compiler):
    try: 
      var = env[self.var]
    except VariableNotBound:
      env[self.var] = var = compiler.new_var(self.var)
      if isinstance(var, Const):
        var.assigned = True
      return Assign(var, self.exp.alpha(env, compiler))
    if isinstance(var, Const) and var.assigned:
      raise MultiAssignToConstError(var)
    return Assign(var, self.exp.alpha(env, compiler))
  
  def cps(self, compiler, cont):
    v = compiler.new_var(il.ConstLocalVar('v'))
    return self.exp.cps(compiler, 
              il.clamda(v, il.Assign(self.var.interlang(), v), cont(v)))
  
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.exp==y.exp
  
  def to_code(self, compiler):
    return repr(self)
  
  def __repr__(self):
    return 'assign(%r, %r)'%(self.var, self.exp)

def direct_interlang(*exps):
  return DirectInterlang(il.begin(*exps))

class DirectInterlang(Element):
  def __init__(self, body):
    self.body = body
  
  def alpha(self, env, compiler):
    return self
  
  def cps(self, compiler, cont):
    return cont(self.body)
    
@special
def expression_with_code(compiler, cont, exp):
  v = compiler.new_var(il.ConstLocalVar('v'))
  return cont(il.ExpressionWithCode(exp, il.Lamda((), exp.cps(compiler, il.clamda(v, v)))))

type_map = {int:Integer, float: Float, str:String, unicode: String, 
            tuple: make_tuple, list:List, dict:Dict, 
            bool:Bool, type(None): Atom,
            type(lambda:1):PyFunction}

