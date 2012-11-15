# -*- coding: utf-8 -*-

from dao.base import classeq
import dao.interlang as il
from dao.compile import cps_convert, alpha_convert
from dao.interlang import cps_convert_exps

v, fc = il.Var('v'), il.Var('fc')

class Command: pass

class special(Command):
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    return SpecialCall(self.function, args)
  
  def __repr__(self):
    return self.function.__name__

class CommandCall: 
  def __init__(self, function, args):
    self.function, self.args = function, args
    
  def __eq__(x, y):
    return classeq(x, y) and x.function==y.function and x.args==y.args

class SpecialCall(CommandCall):
    
  def cps_convert(self, compiler, cont):
    return self.function(compiler, cont, *self.args)
  
  def alpha_convert(self, env, compiler):
    return self.__class__(self.function,
                 tuple(alpha_convert(arg, env, compiler) for arg in self.args))
    
  def __repr__(self):
    return '%s(%s)'%(self.function.__name__, ', '.join(tuple(repr(x) for x in self.args)))

from dao.compilebase import VariableNotBound

class Assign(Command):
  def __call__(self, var, value):
    return AssignCall(var, value)
  
  def __repr__(self):
    return 'assign'

assign = Assign()

class AssignCall(SpecialCall):
  def __init__(self, var, value):
    self.var, self.exp = var, value
    
  def alpha_convert(self, env, compiler):
    try: var = env[self.var]
    except VariableNotBound:
      env[self.var] = var = compiler.new_var(self.var)
    return AssignCall(var, alpha_convert(self.exp, env, compiler))
  
  def cps_convert(self, compiler, cont):
    var, exp = self.var, self.exp
    return cps_convert(compiler, exp, il.Clamda(v, il.Assign(var, v), cont(v)))
    
  def __repr__(self):
    return 'assign(%r, %r)'%(self.var, self.exp)

@special
def quote(compiler, cont, exp):
    return cont(exp)
  
@special
def begin(compiler, cont, *exps):
    return cps_convert_exps(compiler, exps, cont)
    
@special
def if_(compiler, cont, test, then, else_):
  if else_ is None:
    return cps_convert(compiler, test, 
            il.Clamda(v, il.If2(v, cps_convert(compiler, then, cont))))
  else:
    return cps_convert(compiler, test, 
           il.Clamda(v, il.If(v, cps_convert(compiler, then, cont), 
                                    cps_convert(compiler, else_, cont))))

@special
def succeed(compiler, cont):
  return cont(True)

succeed = succeed()

@special
def fail(compiler, cont):
  return il.failcont(True)

fail = fail()

@special
def cut(compiler, cont):
  return il.Begin(il.SetFailCont(il.cut_cont), 
                  il.Clamda(v, cont(v)))

@special
def not_p(compiler, cont, clause):
  fc = compiler.new_var(il.Var('old_fail_cont'))
  return il.begin(il.Assign(fc, il.failcont), 
                  il.SetFailCont(cont),
                  cps_convert(compiler, clause, fc))
  
#@special
#def cut_or(compiler, cont):
  #return il.Begin(il.SetFailCont(il.cut_or_cont), 
                  #il.Clamda(v, cont(v)))

def append_fail_cont(compiler, exp):
  v1 =  compiler.new_var(v)
  fc1 = compiler.new_var(fc)
  return il.Begin((
    il.Assign(fc1, il.failcont),
    il.SetFailCont(
      il.Clamda(v1, 
                il.SetFailCont(fc1),
                exp))
    ))
  
@special
def or_(compiler, cont, clause1, clause2):
  cut_or_cont = compiler.new_var(il.Var('cut_or_cont'))
  or_cont = il.Clamda(v, il.SetCutOrCont(cut_or_cont), cont(v))
  return il.begin(
  il.Assign(cut_or_cont, il.cut_or_cont),
  il.SetCutOrCont(il.failcont),  
  append_fail_cont(compiler, cps_convert(compiler, clause2, or_cont)),
  cps_convert(compiler, clause1, or_cont))

@special
def first_p(compiler, cont, clause1, clause2):
  first_cont = il.Clamda(v, il.SetFailCont(fc), cont(v))
  return il.Begin(
    il.Assign(fc, il.failcont),
    il.AppendFailCont(cps_convert(compiler, clause2, first_cont)),
    cps_convert(compiler, clause1, first_cont))

@special
def if_p(compiler, cont, condition, action):
  return cps_convert(compiler, condition,  il.Clamda(v, cps_convert(compiler, action, cont)))

@special
def unify(compiler, cont, x, y):
  try: 
    x_cps_convert_unify = x.cps_convert_unify
  except:
    try: y_cps_convert_unify = y.cps_convert_unify
    except:
      if x==y: return cont(True)
      else: return il.failcont(True)
    return y_cps_convert_unify(x, cont)
  return x_cps_convert_unify(y, cont)

class BuiltinFunction(Command):
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    return BuiltinFunctionCall(self.function, args)
  
  def cps_convert(self, compiler, cont):
    return il.Lamda((params), self.function(params))
  
class BuiltinFunctionCall(CommandCall):
  def alpha_convert(self, env, compiler):
    return self.__class__(self.function,
                 tuple(alpha_convert(arg, env, compiler) for arg in self.args))
  
  def cps_convert(self, compiler, cont):
    #see The 90 minute Scheme to C compiler by Marc Feeley
    args = self.args
    vars = tuple(il.Var('a'+repr(i)) for i in range(len(args)))
    fun = cont(self.function(vars))
    for var, arg in reversed(zip(vars, args)):
      fun = cps_convert(compiler, arg, il.Clamda(var, fun))
    return fun
     
add = BuiltinFunction(il.add)

LogicVar = il.LogicVar

lamda = il.Lamda

let = il.let

def letrec(bindings, *body):
  params = tuple(p for p, _ in bindings)
  args = tuple(a for _, a in bindings)
  assigns = tuple(assign(k, v) for k, v in bindings)
  return begin(*(assigns+(lamda(params, *body)(*params),)))

@special
def set_parse_state(compiler, cont, parse_state):
  x = il.Var('x')
  return il.Clamda(v, 
                   il.Assign(x, il.parse_state),
                   il.set_parse_state(parse_state),
                   il.AppendFailCont(il.set_parse_state(x)),
                   cont(v))

@special
def get_parse_state(compiler, cont):
  return il.Clamda(v, cont(il.parse_state))

@special
def set_parse_state(compiler, cont, state):
  return il.Clamda(v,
            il.set_parse_state(state), 
            v)

@special
def char(compiler, cont, argument):
  text, pos = il.Var('text'), il.Var('pos')
  if isinstance(argument, str):
    return il.Clamda(v,
      il.AssignFromList(text, pos, il.parse_state),
      il.If2(pos>=il.Len(text), il.failcont(v)),
      il.If(il.Eq(argument, text[pos]),
            il.begin(il.AppendFailCont(il.SetParseState((text, pos))),
                     il.SetParseState((text, pos+1)),
                     cont(text[pos])),
            il.failcont(v)))
  
  elif isinstance(argument, il.Var):
    return il.Clamda(v,
      il.AssignFromList(text, pos, il.parse_state),
      il.If2(pos>=il.Len(text), il.failcont(v)),
      il.Assign(argument, il.Deref(argument)),
      il.If(il.Isinstance(argument, 'str'),
            il.If(il.Eq(argument, text[pos]),
                  il.begin(il.AppendFailCont(il.SetParseState((text, pos))),
                           il.SetParseState((text, pos+1)),
                           cont(text[pos])),
                  il.failcont(v)),
            il.If(il.Isinstance(argument, 'LogicVar'),
                  il.begin(il.SetParseState((text, pos+1)),
                           il.SetBinding(argument, text[pos]),
                           il.AppendFailCont(
                              il.SetParseState((text, pos)),
                              il.DelBinding(argument)),
                           cont(text[pos])),
                  il.RaiseTypeError(argument))))
      
  # elif isinstance(argument, il.LogicVar) #how about this? It should be include above.
  else: raise CompileTypeError(argument)

@special
def Eoi(compiler, cont):
  '''end of parse_state'''
  return il.If(il.Eq(il.parse_state[1], il.Len(il.parse_state[0])),
          cont(v),
          il.failcont(v))
    
eoi = Eoi()

@special 
def callcc(compiler, cont, fun):
  # have not been done.
  ''' call with current continuation '''
  return il.Clamda(v, *fun(cont, cont))

@special
def findall(compiler, cont, goal, template=None, bag=None):
  if bag is None:
    return il.begin(
      il.AppendFailCont(cont(v)),
      cps_convert(compiler, goal, il.Clamda(v, il.failcont(v)))
      )
  else:
    result = il.Var('findall_result') # variable capture
    return il.begin(
       il.Assign(result, il.empty_list()),
       il.AppendFailCont(
          cps_convert(compiler, unify(bag, result), cont)),
        cps_convert(compiler, goal, 
          il.Clamda(v, 
            il.ListAppend(result, il.GetValue(template)),
            il.failcont(v)))
        )
  
greedy, nongreedy, lazy = 0, 1, 2

def may(item, mode=greedy):
  if mode==greedy: return _greedy_may(item)
  elif mode==nongreedy: return _may(item)
  else: return _lazy_may(item)

@special
def _may(compiler, cont, item):
  return cps_convert(compiler, clause, cont, il.Clamda(v,  cont(v)))

@special
def _lazy_may(compiler, cont, item):
  return il.Clamda(v, cont(v, cps_convert(compiler, item, cont)))

@special
def _greedy_may(compiler, cont, item):
  return cps_convert(compiler, item, il.Clamda(v, cont(v)), 
                                      il.Clamda(v, cont(v)))

# infinite recursive, maxizism recursive level
# solutions: trampoline
@special
def repeat(compiler, cont):
  function = il.Var('function')
  return il.begin(il.SetFailCont(function), 
                  il.CFunction(function, v, cont(v)))

repeat = repeat()

@special
def _any(compiler, cont, item):
  any_cont = il.Var('any_cont')
  return il.CFunction(any_cont, v, 
                il.AppendFailCont(cont(v)),
                cps_convert(compiler, item, any_cont))(None)

  
@special
def _lazy_any(compiler, cont, item):
  function = il.Var('function')
  return  il.Clamda(v, cont(v, 
              il.CFunction(function, v, 
                           cps_convert(compiler, item, 
                                                il.Clamda(v, cont(v, function)), 
                                                fcont))))
                             
@special
def _greedy_any(compiler, cont, item):
  function = il.Var('function')
  return il.CFunction(function, v, 
                      cps_convert(compiler, item, function, cont)(None, cont)
                      )

from dao.interlang import LogicVar