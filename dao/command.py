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
    
  def alpha_convert(self, env, compiler):
    return self
  
  def cps_convert(self, compiler, cont):
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
  
  def alpha_convert(self, env, compiler):
    return env[self]
    
  def subst(self, bindings):  
    try: return bindings[self]
    except: return self
      
  def cps_convert(self, compiler, cont):
    return cont(il.Var(self.name))
  
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
  
  def cps_convert_call(self, compiler, cont, args):
    # see The 90 minute Scheme to C compiler by Marc Feeley
    function = compiler.new_var(il.ConstLocalVar('function'))
    vars = tuple(compiler.new_var(il.ConstLocalVar('a'+repr(i))) for i in range(len(args)))
    body = il.Apply(function, (cont,)+vars)
    for var, item in reversed(zip(vars, args)):
      body = item.cps_convert(compiler, il.clamda(var, body)) 
    v = compiler.new_var(il.ConstLocalVar('v'))
    macro_args = il.macro_args([il.ExpressionWithCode(arg, 
                                      il.Lamda((), arg.cps_convert(compiler, il.clamda(v, v)))) 
                                for arg in args])
    return self.cps_convert(compiler, il.clamda(function,
                  il.If(il.IsMacroFunction(function),
                        il.Apply(function, (cont, macro_args)),
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
    return "Var('%s')"%self.name 

class Const(Var):
  def interlang(self):
    return il.ConstLocalVar(self.name)
  
  def __repr__(self):
    return "Const('%s')"%self.name 

class LogicVar(Var):  
  def alpha_convert(self, env, compiler):
    return self
  
  def interlang(self):
    return il.LogicVar(self.name)
  
  def cps_convert(self, compiler, cont):
    return cont(il.Deref(il.LogicVar(self.name)))
  
  def to_code(self, compiler):
    return "DaoLogicVar('%s')"%self.name
  
  def __eq__(x, y):
    return classeq(x, y) and x.name==y.name
  
  def __repr__(self):
    return "DaoLogicVar('%s')"%self.name 
  
class DummyVar(LogicVar):
  def interlang(self):
    return il.DummyVar(self.name)
  
  def cps_convert(self, compiler, cont):
    return cont(il.Deref(il.DummyVar(self.name)))

  def to_code(self, compiler):
    return "DaoDummyVar('%s')"%self.name
  
class Apply(Element):
  def __init__(self, caller, args):
    self.caller, self.args = caller, args

  def alpha_convert(self, env, compiler):
    return self.__class__(self.caller.alpha_convert(env, compiler), 
                 tuple(arg.alpha_convert(env, compiler) for arg in self.args))
  
  def cps_convert(self, compiler, cont):
    # see The 90 minute Scheme to C compiler by Marc Feeley
    return self.caller.cps_convert_call(compiler, cont, self.args)

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
    
  def cps_convert(self, compiler, cont):
    return self.function(compiler, cont, *self.args)
  
  def alpha_convert(self, env, compiler):
    return self.__class__(self.command,
                 tuple(arg.alpha_convert(env, compiler) for arg in self.args))
  
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
  
  def cps_convert(self, compiler, cont):
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
  def alpha_convert(self, env, compiler):
    return self.__class__(self.function,
                 tuple(arg.alpha_convert(env, compiler) for arg in self.args))
  
  def cps_convert(self, compiler, cont):
    #see The 90 minute Scheme to C compiler by Marc Feeley
    args = self.args
    vars = tuple(compiler.new_var(il.ConstLocalVar('a'+repr(i))) for i in range(len(args)))
    fun = cont(self.function.function(*vars))
    for var, arg in reversed(zip(vars, args)):
      fun = arg.cps_convert(compiler, il.Clamda(var, fun))
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
    
  def alpha_convert(self, env, compiler):
    try: 
      var = env[self.var]
    except VariableNotBound:
      env[self.var] = var = compiler.new_var(self.var)
      if isinstance(var, Const):
        var.assigned = True
      return Assign(var, self.exp.alpha_convert(env, compiler))
    if isinstance(var, Const) and var.assigned:
      raise MultiAssignToConstError(var)
    return Assign(var, self.exp.alpha_convert(env, compiler))
  
  def cps_convert(self, compiler, cont):
    v = compiler.new_var(il.ConstLocalVar('v'))
    return self.exp.cps_convert(compiler, 
              il.clamda(v, il.Assign(self.var.interlang(), v), cont(v)))
  
  def __eq__(x, y):
    return classeq(x, y) and x.var==y.var and x.exp==y.exp
  
  def to_code(self, compiler):
    return repr(self)
  
  def __repr__(self):
    return 'assign(%r, %r)'%(self.var, self.exp)

@special
def expression_with_code(compiler, cont, exp):
  v = compiler.new_var(il.ConstLocalVar('v'))
  return cont(il.ExpressionWithCode(exp, il.Lamda((), exp.cps_convert(compiler, il.clamda(v, v)))))

type_map = {int:Integer, float: Float, str:String, unicode: String, 
            tuple: make_tuple, list:List, dict:Dict, 
            bool:Bool, type(None): Atom,
            type(lambda:1):PyFunction}
