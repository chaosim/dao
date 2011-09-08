# -*- coding: utf-8 -*-

class Mixin: pass

from oad.term import Var

__all__ = ['var', 'v', 'do']

class VarListLeader:
  def __getattr__(self, attr):
    return VarList([Var(attr)])

class VarList(list): 
  def __getattr__(self, attr):
    self.append(Var(attr))
    return self
  
var = VarListLeader()

class SingleVarLeader:
  def __getattr__(self, attr):
    return Var(attr)
  
v = SingleVarLeader()

class DoLeader:
  def __getattr__(self, attr):
    if attr=='at': 
      return ForDoLeader()
    else:
      return DoCallWaitArguemnt([Var(attr)])
  def __call__(self, *arguments):
    return DoStatement()

do = DoLeader()

class DoCallWaitArguemnt:
  def __init__(self, caller):
    self.caller = caller
  def __call__(self, *arguments):
    self.statement.append(self.form(*arguments))
  def __getattr__(self, attr):
    if attr in 'statement': return     
    return DoCallWaitArguemnt([Var(attr)])
class DoStatement:
  def __getattr__(self, attr):
    if attr=='when': 
      return DoWhenWaitCondition
    elif attr=='until':
      return DoUntilWaitCondition

class DoWhenWaitCondition:
  def __call__(self, *conditions):
    return DoWhenStatement(self.stmts, conditions) 
  
class DoUntilWaitCondition:
  def __call__(self, *conditions):
    return DoUntilStatement(self.stmts, conditions) 
  
class LoopLeader:
  def __call__(self, *arguments):
    if len(arguments)==1: return LoopTimes(arguments)
    if len(arguments)==0: return LoopForever()
  
class LoopTimes:
  def __call__(self, *arguments):
    self.statements = arguments
    
class LoopForever:
  def __call__(self, *arguments):
    self.statements = arguments