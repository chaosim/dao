# -*- coding: utf-8 -*-

from dao.base import classeq
import dao.interlang as il
from dao.compilebase import CompileTypeError
from dao.interlang import cps_convert_exps

from dao.interlang import TRUE, FALSE, NONE

v0, fc0 = il.Var('v'), il.Var('fc')

class Command: pass

class special(Command):
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    args = tuple(il.element(arg) for arg in args)
    return SpecialCall(self.function, args)
  
  def __repr__(self):
    return self.function.__name__

class CommandCall(il.Element): 
  def __init__(self, function, args):
    self.function, self.args = function, args
    
  def subst(self, bindings):
    return self.__class__(self.function, 
                 tuple(arg.subst(bindings) for arg in self.args))
    
  def __eq__(x, y):
    return classeq(x, y) and x.function==y.function and x.args==y.args
  
  def __repr__(self):
    return '%r(%s)'%(self.function, ', '.join([repr(x) for x in self.args]))

class SpecialCall(CommandCall):
    
  def cps_convert(self, compiler, cont):
    return self.function(compiler, cont, *self.args)
  
  def alpha_convert(self, env, compiler):
    return self.__class__(self.function,
                 tuple(arg.alpha_convert(env, compiler) for arg in self.args))
    
  def __repr__(self):
    return '%s(%s)'%(self.function.__name__, 
                     ', '.join(tuple(repr(x) for x in self.args)))

from dao.compilebase import VariableNotBound

class Assign(Command):
  def __call__(self, var, value):
    return AssignCall(il.element(var), il.element(value))
  
  def __repr__(self):
    return 'assign'

assign = Assign()

class AssignCall(SpecialCall):
  def __init__(self, var, exp):
    self.var, self.exp = var, exp
    
  def subst(self, bindings):
    return AssignCall(self.var, self.exp.subst(bindings))
    
  def alpha_convert(self, env, compiler):
    try: var = env[self.var]
    except VariableNotBound:
      env[self.var] = var = compiler.new_var(self.var)
    return AssignCall(var, self.exp.alpha_convert(env, compiler))
  
  def cps_convert(self, compiler, cont):
    v = compiler.new_var(v0)
    return self.exp.cps_convert(compiler, il.clamda(v, il.Assign(self.var, v), cont(NONE)))
    
  def __repr__(self):
    return 'assign(%r, %r)'%(self.var, self.exp)

@special
def quote(compiler, cont, exp):
    return cont(exp)

from dao.compilebase import Environment, Compiler

@special
def eval_(compiler, cont, exp):
  return exp.cps_convert(compiler, il.Done()).cps_convert(compiler, cont)

@special
def callcc(compiler, cont, function):
  return function(cont)

@special
def begin(compiler, cont, *exps):
    return cps_convert_exps(compiler, exps, cont)
    
@special
def if_(compiler, cont, test, then, else_):
  v = compiler.new_var(v0)
  if else_ is None:
    return test.cps_convert(compiler, 
            il.Clamda(v, il.if2(v, then.cps_convert(compiler, cont))))
  else:
    return test.cps_convert(compiler, 
           il.Clamda(v, il.If(v, then.cps_convert(compiler, cont), 
                                 else_.cps_convert(compiler, cont))))

@special
def succeed(compiler, cont):
  return cont(TRUE)

succeed = succeed()

@special
def fail(compiler, cont):
  return il.failcont(TRUE)

fail = fail()

@special
def cut(compiler, cont):
  v = compiler.new_var(v0)
  return il.Begin(il.SetFailCont(il.cut_cont), 
                  il.Clamda(v, cont(v)))

@special
def not_p(compiler, cont, clause):
  fc = compiler.new_var(il.Var('old_fail_cont'))
  return il.begin(il.Assign(fc, il.failcont), 
                  il.SetFailCont(cont),
                  clause.cps_convert(compiler, fc))
  
#@special
#def cut_or(compiler, cont):
  #return il.Begin(il.SetFailCont(il.cut_or_cont), 
                  #il.Clamda(v, cont(v)))
  
@special
def or_(compiler, cont, clause1, clause2):
  v = compiler.new_var(v0)
  cut_or_cont = compiler.new_var(il.Var('cut_or_cont'))
  or_cont = il.clamda(v, il.SetCutOrCont(cut_or_cont), cont(v))
  return il.begin(
    il.Assign(cut_or_cont, il.cut_or_cont),
    il.SetCutOrCont(il.failcont),  
    il.append_fail_cont(compiler, clause2.cps_convert(compiler, or_cont)),
    clause1.cps_convert(compiler, or_cont))

@special
def first_p(compiler, cont, clause1, clause2):
  v = compiler.new_var(v0)
  fc = compiler.new_var(fc)
  first_cont = il.Clamda(v, il.SetFailCont(fc), cont(v))
  return il.Begin(
    il.Assign(fc, il.failcont),
    il.AppendFailCont(clause2.cps_convert(compiler, first_cont)),
    cps_convert(clause1.compiler, first_cont))

@special
def if_p(compiler, cont, condition, action):
  v = compiler.new_var(v0)
  return condition.cps_convert(compiler, 
                    il.Clamda(v, action.cps_convert(compiler, cont)))

@special
def unify(compiler, cont, x, y):
  try: 
    x_cps_convert_unify = x.cps_convert_unify
  except:
    try: y_cps_convert_unify = y.cps_convert_unify
    except:
      if x==y: return cont(TRUE)
      else: return il.failcont(TRUE)
    return y_cps_convert_unify(x, compiler, cont)
  return x_cps_convert_unify(y, compiler, cont)

class BuiltinFunction(Command):
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    args = tuple(il.element(arg) for arg in args)
    return BuiltinFunctionCall(self.function, args)
  
  def cps_convert(self, compiler, cont):
    return il.Lamda((params), self.function(*params))
  
class BuiltinFunctionCall(CommandCall):
  def alpha_convert(self, env, compiler):
    return self.__class__(self.function,
                 tuple(arg.alpha_convert(env, compiler) for arg in self.args))
  
  def cps_convert(self, compiler, cont):
    #see The 90 minute Scheme to C compiler by Marc Feeley
    args = self.args
    vars = tuple(compiler.new_var(il.Var('a'+repr(i))) for i in range(len(args)))
    fun = cont(self.function(*vars))
    for var, arg in reversed(zip(vars, args)):
      fun = arg.cps_convert(compiler, il.Clamda(var, fun))
    return fun
     
  def __repr__(self):
    return '%s(%s)'%(self.function.name, ', '.join([repr(x) for x in self.args]))

add = BuiltinFunction(il.add)
eq = BuiltinFunction(il.Eq)
sub = BuiltinFunction(il.sub)

LogicVar = il.LogicVar

lamda = il.lamda

def let(bindings, *body):
  bindings = tuple((var, il.element(value)) for var, value in bindings)
  return Let(bindings, begin(*body))

class Let(il.Element):
  def __init__(self, bindings, body):
    self.bindings = bindings
    self.body = body
  
  def alpha_convert(self, env, compiler):
    new_env = env.extend()
    for var, value in self.bindings:
      new_env.bindings[var] = compiler.new_var(var)
    alphaed = self.body.alpha_convert(new_env, compiler) 
    bindings = {new_env[var]:value for var, value in self.bindings}
    return alphaed.subst(bindings)
  
  def subst(self, bindings):
    bindings = tuple((var.subst(bindings), value.subst(bindings))
                     for var, value in self.bindings)
    body = self.body.subst(bindings)
    return Let(bindings, body)
  
  def __repr__(self):
    return 'Let(%r, %r)'%(self.bindings, self.body)
    

#TODO__letrec

def letrec(bindings, *body):
  return Letrec(bindings, body)

class Letrec(il.Element):
  def __init__(self, bindings, body):
    self.bindings = bindings
    self.body = body
  
  def alpha_convert(self, env, compiler):
    new_env = env.extend()
    for var, value in self.bindings:
      new_env.bindings[var] = compiler.new_var(var)
    body = begin(*(tuple(assign(var, value) for var, value in self.bindings)+self.body))
    return body.alpha_convert(new_env, compiler) 
  
  def __repr__(self):
    return 'Letrec(%r, %r)'%(self.bindings, self.body)

@special
def set_parse_state(compiler, cont, parse_state):
  old_parse_state = compiler.new_var(il.Var('old_parse_state'))
  return il.begin(il.Assign(old_parse_state, il.parse_state),
                  il.SetParseState(parse_state),
                  il.append_fail_cont(compiler, il.SetParseState(old_parse_state)),
                  cont(TRUE))
@special
def settext(compiler, cont, text):
  old_parse_state = compiler.new_var(il.Var('old_parse_state'))
  return il.begin(il.Assign(old_parse_state, il.parse_state),
                  il.SetParseState(il.Tuple(text, il.Integer(0))),
                  il.append_fail_cont(compiler, il.SetParseState(old_parse_state)),
                  cont(TRUE))

@special
def parse_state(compiler, cont):
  return il.Clamda(v, cont(il.parse_state))

@special
def char(compiler, cont, argument):
  #v = compiler.new_var(v0)
  text, pos = il.Var('text'), il.Var('pos')
  if isinstance(argument, il.String):
    return il.Begin((
      il.AssignFromList(text, pos, il.parse_state),
      il.if2(il.Ge(pos, il.Len(text)), il.Return(il.failcont(NONE))),
      il.If(il.Eq(argument, il.GetItem(text, pos)),
            il.begin(il.append_fail_cont(compiler, 
                            il.SetParseState(il.Tuple(text, pos))),
                     il.SetParseState(il.Tuple(text, il.add(pos, il.Integer(1)))),
                     il.Return(cont(il.GetItem(text, pos)))),
            il.Return(il.failcont(NONE)))
    ))
  
  elif isinstance(argument, il.Var):
    return il.Begin((
      il.AssignFromList(text, pos, il.parse_state),
      il.if2(il.Ge(pos,il.Len(text)), il.Return(il.failcont(v))),
      il.Assign(argument, il.Deref(argument)),
      il.If(il.Isinstance(argument, 'str'),
            il.If(il.Eq(argument, il.GetItem(text, pos)),
                  il.begin(il.append_fail_cont(compiler, 
                                  il.SetParseState(il.Tuple(text, pos))),
                           il.SetParseState(il.Tuple(text, il.add(pos, 1))),
                           cont(il.GetItem(text, pos))),
                  il.Return(il.failcont(NONE))),
            il.If(il.Isinstance(argument, 'LogicVar'),
                  il.begin(il.SetParseState(il.Tuple(text, il.add(pos,1))),
                           il.SetBinding(argument, il.GetItem(text, pos)),
                           il.append_fail_cont(compiler, 
                              il.SetParseState(il.Tuple(text, pos)),
                              il.DelBinding(argument)),
                           il.Return(cont(il.GetItem(text, pos)))),
                  il.RaiseTypeError(argument)))
    ))
      
  # elif isinstance(argument, il.LogicVar) #how about this? It should be include above.
  else: raise CompileTypeError(argument)

@special
def Eoi(compiler, cont):
  '''end of parse_state'''
  return il.If(il.Eq(il.GetItem(il.parse_state, il.Integer(1)), 
                     il.Len(il.GetItem(il.parse_state, il.Integer(0)))),
          cont(TRUE),
          il.failcont(FALSE))
    
eoi = Eoi()

#@special 
#def callcc(compiler, cont, fun):
  #v = compiler.new_var(v0)
  ## have not been done.
  #''' call with current continuation '''
  #return il.Clamda(v, fun(cont, cont))

@special
def findall(compiler, cont, goal, template=NONE, bag=None):
  v = compiler.new_var(v0)
  if bag is None:
    return il.begin(
      il.AppendFailCont(cont(NONE)),
      cps_convert(compiler, goal, il.Clamda(v, il.failcont(v)))
      )
  else:
    result = il.Var('findall_result') # variable capture
    return il.begin(
       il.Assign(result, il.empty_list()),
       il.AppendFailCont(
          cps_convert(compiler, unify(bag, result), cont)),
        cps_convert(compiler, goal, 
          il.clamda(v, 
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
  v = compiler.new_var(v0)
  return cps_convert(compiler, clause, cont, il.Clamda(v,  cont(v)))

@special
def _lazy_may(compiler, cont, item):
  v = compiler.new_var(v0)
  return il.Clamda(v, cont(v, cps_convert(compiler, item, cont)))

@special
def _greedy_may(compiler, cont, item):
  v = compiler.new_var(v0)
  return cps_convert(compiler, item, il.Clamda(v, cont(v)), 
                                      il.Clamda(v, cont(v)))

# infinite recursive, maxizism recursive level
# solutions: trampoline
@special
def repeat(compiler, cont):
  v = compiler.new_var(v0)
  function = compiler.new_var(il.Var('function'))
  return il.begin(il.SetFailCont(function), 
                  il.cfunction(function, v, cont(v)))

repeat = repeat()

def any(item, mode=nongreedy):
  if mode==greedy: return _greedy_any(item)
  elif mode==nongreedy: return _any(item)
  else: return _lazy_any(item)
  
@special
def _any(compiler, cont, item):
  any_cont = compiler.new_var(il.Var('any_cont'))
  fc = compiler.new_var(il.Var('old_fail_cont'))
  v = compiler.new_var(il.Var('v'))
  return il.cfunction(any_cont, v,
                il.Assign(fc, il.failcont),
                il.SetFailCont(il.clamda(v, 
                  il.SetFailCont(fc),
                  cont(v))),
                item.cps_convert(compiler, any_cont))(TRUE)

  
@special
def _lazy_any(compiler, cont, item):
  fcont = compiler.new_var(il.Var('fcont'))
  lazy_any_cont = compiler.new_var(il.Var('lazy_any_cont'))
  lazy_any_fcont = compiler.new_var(il.Var('lazy_any_fcont'))
  v = compiler.new_var(il.Var('v'))
  return  il.begin(
    il.Assign(fcont, il.failcont),
    il.cfunction(lazy_any_cont, v,
        il.SetFailCont(lazy_any_fcont),
        cont(v)),
    il.cfunction(lazy_any_fcont, v,
        il.SetFailCont(fcont),
        cps_convert(compiler, item, lazy_any_cont)),    
    lazy_any_cont(TRUE))
                             
@special
def _greedy_any(compiler, cont, item):
  fcont = compiler.new_var(il.Var('fcont'))
  greedy_any_fcont = compiler.new_var(il.Var('greedy_any_fcont'))
  greedy_any_cont = compiler.new_var(il.Var('greedy_any_cont'))
  return il.begin(
    il.Assign(fcont, il.failcont),
    il.cfunction(greedy_any_fcont, v,
        il.SetFailCont(fcont),
        cont(v)),    
    il.cfunction(greedy_any_cont, v,
        il.SetFailCont(greedy_any_fcont),
         cps_convert(compiler, item, greedy_any_cont)),
    greedy_any_cont(TRUE))

from dao.interlang import LogicVar