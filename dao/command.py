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
    
  def cps_convert(self, compiler, cont, fcont):
    return self.function(compiler, cont, fcont, *self.args)
  
  def __repr__(self):
    return '%s(%s)'%(self.function.__name__, ', '.join(tuple(repr(x) for x in self.args)))
  
@special
def quote(compiler, cont, fcont, exp):
    return cont(exp, fcont)
  
@special
def assign(compiler, cont, fcont, var, exp):
    return compiler.cps_convert(exp, 
            il.Clamda(v, fc, il.Assign(var, v), il.Return(v, fc)), fcont)
  
@special
def begin(compiler, cont, fcont, *exps):
    return compiler.cps_convert_exps(exps, cont, fcont)
    
@special
def if_(compiler, cont, fcont, test, then, else_):
  if else_ is None:
    return compiler.cps_convert(test, 
           il.Clamda(v, fc, il.If2(v, compiler.cps_convert(then, cont, fcont))))
  else:
    return compiler.cps_convert(test, 
           il.Clamda(v, fc, il.If(v, compiler.cps_convert(then, cont, fcont), 
                                    compiler.cps_convert(else_, cont, fcont))),
           fcont)

@special
def succeed(compiler, cont, fcont):
  return cont

succeed = succeed()

@special
def fail(compiler, cont, fcont):
  return fcont

fail = fail()

@special
def cut(compiler, cont, fcont):
  return il.Clamda(v, fc, cont(v, compiler.cut_rules_fcont[-1]))

@special
def not_p(compiler, cont, fcont, clause):
  return compiler.cps_convert(clause, fcont, cont)
  
@special
def cut_or(compiler, cont, fcont):
  return il.Clamda(v, fc, cont(v, compiler.cut_or_fcont[-1]))

@special
def or_(compiler, cont, fcont, clause1, clause2):
  compiler.cut_or_fcont.append(fcont)
  result = compiler.cps_convert(clause1, cont, 
                                il.Clamda(v, fc, 
                     compiler.cps_convert(clause2, cont, fcont)))
  compiler.cut_or_fcont.pop()
  return result

@special
def first_p(compiler, cont, fcont, clause1, clause2):
  cont1 = il.Clamda(v, fc, cont(v, fcont))
  return compiler.cps_convert(clause1, cont1, 
                              il.Clamda(v, fc, 
                                        compiler.cps_convert(clause2, cont1, fcont)))

@special
def if_p(compiler, cont, fcont, condition, action):
  return compiler.cps_convert(condition,  il.Clamda(v, fc, compiler.cps_convert(action, cont, fcont)), action)

@special
def unify(compiler, cont, fcont, x, y):
  try: 
    x_cps_convert_unify = x.cps_convert_unify
  except:
    try: y_cps_convert_unify = y.cps_convert_unify
    except:
      if x==y: return cont
      else: return fcont
    return y_cps_convert_unify(x, cont, fcont)
  return x_cps_convert_unify(y, cont, fcont)

class BuiltinFunction(Command):
  def __init__(self, function):
    self.function = function
    
  def __call__(self, *args):
    return BuiltinFunctionCall(self.function, args)
  
  def cps_convert(self, compiler, cont, fcont):
    return il.Lamda((params), il.Return(self.function(params)))
  
class BuiltinFunctionCall(CommandCall):
  def cps_convert(self, compiler, cont, fcont):
    #see The 90 minute Scheme to C compiler by Marc Feeley
    args = self.args
    vars = tuple(il.Var('a'+repr(i)) for i in range(len(args)))
    fun = il.Return(cont(self.function(vars), fc))
    for var, arg in reversed(zip(vars, args)):
      fun = compiler.cps_convert(arg, il.Clamda(var, fc, fun), fcont)
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
def set_parse_state(compiler, cont, fcont, parse_state):
  x = il.Var('x')
  return il.Clamda(v, fc, 
                   il.Assign(x, il.get_parse_state()),
                   il.set_parse_state(parse_state),
                   il.Return(cont(v, il.Clamda(v, fc, 
                    il.set_parse_state(x),
                    il.Return(fcont(v, fc))))))

@special
def get_parse_state(compiler, cont, fcont):
  return il.Clamda(v, fc, cont(il.Return(il.get_parse_state()), fcont))

def restore_parse_state(state, fcont):
  return il.Clamda(v, fc,
            il.set_parse_state(state), 
            il.Return(v, fcont))

@special
def char(compiler, cont, fcont, argument):
  text, pos = il.Var('text'), il.Var('pos')
  if isinstance(argument, str):
    return il.Clamda(v, fc,
      il.assign_from_list(text, pos, il.get_parse_state()),
      il.If2(pos>=il.Len(text), il.Return(v, fc)),
      il.If(il.eq(argument, il.getitem(text, pos)),
            il.begin(il.set_parse_state((text, pos+1)),
                     il.Return(cont(text[pos], 
                               restore_parse_state((text, pos), fc)))),
            il.Return(v, fc))
      )
  elif isinstance(argument, il.Var):
    return il.Clamda(v, fc,
      il.assign_from_list(text, pos, il.get_parse_state()),
      il.If2(pos>=il.Len(text), il.Return(fcont(v, fc))),
      cps_convert(unify(argument, text[pos], 
               il.Clamda(v, fc,
                         il.begin(il.set_parse_state((text, pos+1)),
                         il.Return(cont(text[pos], 
                                   restore_parse_state((text, pos), fc))))),
               fc)(v, fc))
      )
  # elif isinstance(argument, il.LogicVar) #how about this? It should be include above.
  else: raise CompileTypeError(argument)

@special
def Eoi(compiler, cont, fcont):
  '''end of parse_state'''
  return il.Clamda(v, fc,
    il.If(il.get_parse_state()[1]<il.Len(il.get_parse_state()[0]),
          il.Return(cont(v, fc)),
          il.Return(fcont(v, fc))))
    
eoi = Eoi()

@special 
def callcc(compiler, cont, fcont, fun):
  # have not been done.
  ''' call with current continuation '''
  return il.Clamda(v, fc, il.Return(*fun(cont, cont)))

@special
def findall(compiler, cont, fcont, goal, template=None, bag=None):
  found = il.Var('found')
  if bag is None:
    return il.Clamda(v, fc, 
      let([(found, False)],
          compiler.cps_convert(goal, 
            il.Clamda(v, fc,
              il.If(il.Not(found), 
                    il.Return(fc(v, fc)),
                    il.Return(cont(v, fcont)))), 
            il.Clamda(v, fc, 
                      il.Assign(found, True),
                      il.Return(fc(v, fc))))))
  else: # have not been done.
    result = []
    for c, x in solver.exp_run_cont(goal, cont):
      result.append(getvalue(template, solver.env))
    for x in bag.unify(result, solver.env):
      return cont, True
    
@special
def findall(compiler, cont, fcont, goal, template=None, bag=None):
  if bag is None:
    return il.Clamda(v, fc, 
      compiler.cps_convert(goal, 
            il.Clamda(v, fc, il.Return(fc(v, fc))), 
            il.Clamda(v, fc, il.Return(cont(v, fcont)))))
  else:
    result = il.Var('result') # variable capture
    return il.Clamda(v, fc,
      il.Assign(result, il.empty_list()),
      compiler.cps_convert(goal, 
            il.Clamda(v, fc, 
              il.list_append(result, il.getvalue(template)),
              il.Return(fc(v, fc))), 
            il.Clamda(v, fc, 
              il.Return(il.Unify(bag, result, cont, fcont)(v, fc)))))
  
greedy, nongreedy, lazy = 0, 1, 2

def may(item, mode=greedy):
  if mode==greedy: return _greedy_may(item)
  elif mode==nongreedy: return _may(item)
  else: return _lazy_may(item)

@special
def _may(compiler, cont, fcont, item):
  return compiler.cps_convert(clause, cont, il.Clamda(v, fc,  il.Return(cont(v, fcont))))

@special
def _lazy_may(compiler, cont, fcont, item):
  return il.Clamda(v, fc, il.Return(cont(v, compiler.cps_convert(item, cont, fcont))))

@special
def _greedy_may(compiler, cont, fcont, item):
  return compiler.cps_convert(item, il.Clamda(v, fc, il.Return(cont(v, fcont))), 
                                      il.Clamda(v, fc, il.Return(cont(v, fcont))))

# infinite recursive, maxizism recursive level
# solutions: trampoline
@special
def repeat(compiler, cont, fcont):
  function = il.Var('function')
  return il.CFunction(function, v, fc, cont(v, function))

repeat = repeat()

@special
def _any(compiler, cont, fcont, item):
  function = il.Var('function')
  #v1, fc1 = il.Var('v1'), il.Var('fc1')
  return il.Clamda(v, fc, 
                   il.CFunction(function, v, fc, 
                                compiler.cps_convert(item, function,
                                                     il.Clamda(v, fc, il.Return(cont(v, fc))))
                                )(v, fcont)
                   )

@special
def _lazy_any(compiler, cont, fcont, item):
  function = il.Var('function')
  return  il.Clamda(v, fc, il.Return(cont(v, 
              il.CFunction(function, v, fc, 
                           compiler.cps_convert(item, 
                                                il.Clamda(v, fc, cont(v, function)), 
                                                fcont)))))
                             
@special
def _greedy_any(compiler, cont, fcont, item):
  function = il.Var('function')
  return il.CFunction(function, v, fc, 
                      il.Return(compiler.cps_convert(item, function, cont)(None, cont))
                      )

