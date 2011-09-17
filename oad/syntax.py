# -*- coding: utf-8 -*-

from oad.builtins import *


##__all__ = ['var', 'v', 'do', 'loop', 'put']


builtins_dict = {
  'write': format.write}

_var_cache = {}
def varcache(name):
  try: return _var_cache[name]
  except: 
    _var_cache[name] = Var(name)
    return _var_cache[name]
  
def object_from_name(name):
  try: return builtins_dict[name]
  except: return varcache(name)
  
class Mixin: pass

from oad.term import Var

class Object:
  def __getattr__(self, attr):
    try: return self.__dict__[attr]
    except: return self.getattr(attr)
  
class VarListLeader(Object):
  def __getattr__(self, attr):
    return VarList([varcache(attr)])

class VarList(list): 
  def __getattr__(self, attr):
    self.append(varcache(attr))
    return self
  
var = VarListLeader()

class LocalVarLeader(Object):
  def __init__(self): return
  def __getattr__(self, attr):
    return varcache(attr)
my = LocalVarLeader()
  
class SingleVarLeader:
  def __getattr__(self, attr):
    return varcache(attr)
  
v = SingleVarLeader()

class DoLeader(Object):
  def __init__(self, forms=None):
    pass
  def getattr(self, attr):
    return DoCallWaitArguemnt(object_from_name(attr))
  def at(self, *arguments):
    if len(arguments)>3: raise SyntaxError
    return ForDoLeader(arguments)
  def __item__(self, *arguments):
    return DoForm(arguments)

do = DoLeader()

class DoCallWaitArguemnt(Object):
  def __init__(self, caller):
    self.caller = caller
    self.forms = []
  def __call__(self, *arguments):
    self.forms.append(self.caller(*arguments))
    return DoForm(self.forms, [], [])
  def getattr(self, attr):
    self.forms.append(self.caller)
    return DoCallWaitArguemnt(object_from_name(attr))
  def when(self, *conditions):
    self.forms.append(self.caller)
    return DoForm(self.forms, conditions, [])
  def until(self, *conditions):
    self.forms.append(self.caller)
    return DoForm(self.forms, [], list(conditions))
  
class DoForm(Object):
  def __init__(self, forms, when_conditions, until_conditions):
    self.forms = forms
    self.when_conditions = when_conditions
    self.until_conditions = until_conditions
  def when(self, *conditions):
    self.when_conditions = list(conditions)
    return self
  def until(self, *conditions):
    self.until_conditions = list(conditions)
    return self
  def getattr(self, attr):
    return DoCallWaitArguemnt(object_from_name(attr))
  
class LoopLeader(Object):
  def __call__(self, *arguments):
    if len(arguments)!=1: raise SyntaxError
    return LoopTimes(arguments[0])
  def __getitem__(self, *stmts):
    return LoopForeverForm(list(stmts))
  def getattr(self, function):
    return LoopCallWaitArgument(function)
loop = LoopLeader()

class LoopTimes(Object):
  def __init__(self, times):
    self.times = times
  def __getitem__(self, *stmts):
    return LoopTimesForm(self.times, list(stmts))
  def getattr(self, attr):
    return LoopTimesCallWaitArguemnt(self.times, [], object_from_name(attr))
  
class LoopTimesForm(Object):
  def __init__(self, times, forms):
    self.times = times
    self.forms = forms
  def __getitem__(self, *stmts):
    return LoopTimesForm(times, stmts)
  
class LoopForeverForm(Object):
  def __init__(self, forms):
    self.forms = forms
    
class LoopTimesCallWaitArguemnt(Object):
  def __init__(self, times, forms, caller):
    self.times = times
    self.caller = caller
    self.forms = forms
  def __call__(self, *arguments):
    self.forms.append(self.caller(*arguments))
    return LoopTimesForm(self.times, self.forms)
  def getattr(self, attr):
    self.forms.append(self.caller)
    return LoopTimesCallWaitArguemnt(self.times, self.forms, object_from_name(attr))
  
class AssignLeader(Object):
  def __init__(self, forms=None):
    self.scope = 'any_scope'
  def getattr(self, attr):
    if attr=='my': 
      self.scope = 'local'
      return self
    elif attr=='out':
      self.scope = 'out'
      return self
    else: return SingleAssignWaitValue(self.scope, varcache(attr))
  def __xor__(self, other):
    if not isinstance(other, int): raise SyntaxError
    else: self.var_scope = ('out', int)
  def __getitem__(self, *arguments):
    return MultipleAssignWaitValue(self.scope, [get_assign_var(a) for a in arguments])

def get_assign_var(var): return var

put = AssignLeader()

class SingleAssignWaitValue(Object):
  def __init__(self, scope, var):
    self.scope, self.var = scope, var
  def __eq__(self, value):
    return SingleAssign(self.scope, self.var, value)

class MultipleAssignWaitValue(Object):
  def __init__(self, scope, var_list):
    self.scope, self.var_list = scope, var_list
  def __eq__(self, value):
    return MultipleAssign(self.var_list, value)

class SingleAssign(Object):
  def __init__(self, scope, var, value):
    self.scope, self.var, self.value = scope, var, value
  def __eq__(self, other):
    return self.scope==other.scope and self.var==other.var and self.value==other.value
    
class MultipleAssign(Object):
  def __init__(self, var_list, value):
    self.var_list, self.value = var_list, value
  def __eq__(self, other):
    return self.var_list==other.var_list and self.value==other.value
    
# letform -> let/{}.do[...]
# letform -> let/{}(.form)*

class LetLeader(Object):
  def __init__(self): pass
  def __div__(self, other):
    return LetBindings(other)
let = LetLeader()

class LetBindings(Object):
  def __init__(self, bindings):
    self.bindings = bindings
  def getattr(self, attr):
    if attr=='end': raise SyntaxError
    return LetFormWaitArgument(self.bindings, object_from_name(attr))
  def __div__(self, other):
    pass
  
class LetFormWaitArgument(Object):
  def __init__(self, bindings, caller):
    self.bindings = bindings
    self.caller = caller
    self.forms = []
  def getattr(self, attr):
    if attr=='end':
      self.forms.append(self.caller)
      return LetForm(self.bindings, self.forms)
    else:
      self.forms.append(self.caller)
      return self

class LetForm(Object):
  def __init__(self, bindings, forms):
    self.bindings, self.forms = bindings, forms
  
    