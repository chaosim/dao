# -*- coding: utf-8 -*-

import dao.interlang as il

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

class SpecialCall(CommandCall):
    
  def cps_convert(self, compiler, cont):
    return self.function(compiler, cont, *self.args)
  
  def __repr__(self):
    return '%s(%s)'%(self.function.__name__, ', '.join(tuple(repr(x) for x in self.args)))
  
@special
def quote(compiler, cont, exp):
    return cont(exp)
  
@special
def assign(compiler, cont, var, exp):
    return compiler.cps_convert(exp, il.Clamda(v, il.Assign(var, v), il.Return(v)))
  
@special
def begin(compiler, cont, *exps):
    return compiler.cps_convert_exps(exps, cont)
    
@special
def if_(compiler, cont, test, then, else_):
  if else_ is None:
    return compiler.cps_convert(test, 
            il.Clamda(v, il.If2(v, compiler.cps_convert(then, cont))))
  else:
    return compiler.cps_convert(test, 
           il.Clamda(v, il.If(v, compiler.cps_convert(then, cont), 
                                    compiler.cps_convert(else_, cont))))

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
  return il.Begin(il.Assign(fc, il.failcont), 
                  il.SetFailCont(cont),
                  compiler.cps_convert(clause, fc))
  
#@special
#def cut_or(compiler, cont):
  #return il.Begin(il.SetFailCont(il.cut_or_cont), 
                  #il.Clamda(v, cont(v)))

@special
def or_(compiler, cont, clause1, clause2):
  or_cont = il.Clamda(v, il.SetCutOrCont(cut_or_cont), cont(v))
  return il.Begin(
  il.Assign(cut_or_cont, il.cut_or_cont),
  il.SetCutOrCont(failcont),  
  il.AppendFailCont(compiler.cps_convert(clause2, or_cont)),
  compiler.cps_convert(clause1, or_cont))

@special
def first_p(compiler, cont, clause1, clause2):
  first_cont = il.Clamda(v, il.SetFailCont(fc), cont(v))
  return il.Begin(
    il.Assign(fc, il.failcont),
    il.AppendFailCont(compiler.cps_convert(clause2, first_cont)),
    compiler.cps_convert(clause1, first_cont))

@special
def if_p(compiler, cont, condition, action):
  return compiler.cps_convert(condition,  il.Clamda(v, compiler.cps_convert(action, cont)))

@special
def unify(compiler, cont, x, y):
  try: 
    x_cps_convert_unify = x.cps_convert_unify
  except:
    try: y_cps_convert_unify = y.cps_convert_unify
    except:
      if x==y: return cont(True)
      else: return il.failcont(False)
    return y_cps_convert_unify(x, cont)
  return x_cps_convert_unify(y, cont)

class BuiltinFunction(Command):
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    return BuiltinFunctionCall(self.function, args)
  
  def cps_convert(self, compiler, cont):
    return il.Lamda((params), il.Return(self.function(params)))
  
class BuiltinFunctionCall(CommandCall):
  def cps_convert(self, compiler, cont):
    #see The 90 minute Scheme to C compiler by Marc Feeley
    args = self.args
    vars = tuple(il.Var('a'+repr(i)) for i in range(len(args)))
    fun = il.Return(cont(self.function(vars)))
    for var, arg in reversed(zip(vars, args)):
      fun = compiler.cps_convert(arg, il.Clamda(var, fun))
    return fun
     
add = BuiltinFunction(il.add)

LogicVar = il.LogicVar

lamda = il.Lamda

def let(bindings, *body):
  params = tuple(p for p, _ in bindings)
  args = tuple(a for _, a in bindings)
  return lamda(params, *body)(*args)

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
                   il.Return(cont(v)))

@special
def get_parse_state(compiler, cont):
  return il.Clamda(v, il.Return(cont(il.Return(il.parse_state))))

@special
def set_parse_state(compiler, cont, state):
  return il.Clamda(v,
            il.set_parse_state(state), 
            il.Return(v))

@special
def char(compiler, cont, argument):
  text, pos = il.Var('text'), il.Var('pos')
  if isinstance(argument, str):
    return il.Clamda(v,
      il.assign_from_list(text, pos, il.parse_state),
      il.If2(pos>=il.Len(text), il.Return(il.failcont(v))),
      il.If(il.eq(argument, il.GetItem(text, pos)),
            il.begin(il.AppendFailCont(il.set_parse_state((text, pos))),
                     il.SetParseState((text, pos+1)),
                     il.Return(cont(text[pos]))),
            il.Return(il.failcont(v))))
  
  elif isinstance(argument, il.Var):
    return il.Clamda(v,
      il.assign_from_list(text, pos, il.get_parse_state()),
      il.If2(pos>=il.Len(text), il.Return(il.failcont(v))),
      il.Unify(argument, text[pos], 
               il.Clamda(v,
                         il.AppendFailCont(il.set_parse_state((text, pos))),
                         il.set_parse_state((text, pos+1)),
                         il.Return(cont(text[pos]))), 
               il.Return(il.failcont(v))))
      
  # elif isinstance(argument, il.LogicVar) #how about this? It should be include above.
  else: raise CompileTypeError(argument)

@special
def Eoi(compiler, cont):
  '''end of parse_state'''
  return il.Clamda(v,
    il.If(il.parse_state[1]<il.Len(il.parse_state[0]),
          il.Return(cont(v)),
          il.Return(il.get_failcont(v))))
    
eoi = Eoi()

@special 
def callcc(compiler, cont, fun):
  # have not been done.
  ''' call with current continuation '''
  return il.Clamda(v, il.Return(*fun(cont, cont)))

@special
def findall(compiler, cont, goal, template=None, bag=None):
  if bag is None:
    return il.Begin(
      il.AppendFailCont(cont(v)),
      compiler.cps_convert(goal, il.Clamda(v, il.Return(il.failcont(v))))
      )
  else:
    result = il.Var('result') # variable capture
    return il.Begin(
       il.Assign(result, il.empty_list()),
       il.AppendFailCont(
          il.Return(il.Unify(bag, result, cont)(v, fc)),
          cont(v)),
        compiler.cps_convert(goal, 
          il.Clamda(v, 
            il.list_append(result, il.getvalue(template)),
            il.Return(il.failcont(v))))
        )
  
greedy, nongreedy, lazy = 0, 1, 2

def may(item, mode=greedy):
  if mode==greedy: return _greedy_may(item)
  elif mode==nongreedy: return _may(item)
  else: return _lazy_may(item)

@special
def _may(compiler, cont, item):
  return compiler.cps_convert(clause, cont, il.Clamda(v,  il.Return(cont(v))))

@special
def _lazy_may(compiler, cont, item):
  return il.Clamda(v, il.Return(cont(v, compiler.cps_convert(item, cont))))

@special
def _greedy_may(compiler, cont, item):
  return compiler.cps_convert(item, il.Clamda(v, il.Return(cont(v))), 
                                      il.Clamda(v, il.Return(cont(v))))

# infinite recursive, maxizism recursive level
# solutions: trampoline
@special
def repeat(compiler, cont):
  function = il.Var('function')
  return il.begin(il.SetFailCont(function), 
                  il.CFunction(function, v, il.Return(cont(v))))

repeat = repeat()

@special
def _any(compiler, cont, item):
  function = il.Var('function')
  #v1, fc1 = il.Var('v1'), il.Var('fc1')
  return il.Clamda(v, 
                   il.CFunction(function, v, 
                                compiler.cps_convert(item, function,
                                                     il.Clamda(v, il.Return(cont(v))))
                                )(v)
                   )

@special
def _lazy_any(compiler, cont, item):
  function = il.Var('function')
  return  il.Clamda(v, il.Return(cont(v, 
              il.CFunction(function, v, fc, 
                           compiler.cps_convert(item, 
                                                il.Clamda(v, cont(v, function)), 
                                                fcont)))))
                             
@special
def _greedy_any(compiler, cont, item):
  function = il.Var('function')
  return il.CFunction(function, v, 
                      il.Return(compiler.cps_convert(item, function, cont)(None, cont))
                      )

