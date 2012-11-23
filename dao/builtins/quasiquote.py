from dao.command import CommandCall

from dao.interlang import element
from dao import interlang as il

# quasiquote and backquote

def quasiquote(item):
  return Quasiquote(element(item))

class Quasiquote(CommandCall):
  def __init__(self, item):
    self.item = item
    
  def alpha_convert(self, env, compiler):
    return Quasiquote(self.item.alpha_convert(env, compiler))
  
  def cps_convert(self, compiler, cont):
    return self.item.quasiquote(compiler, cont)
      
  def quasiquote(self, compiler, cont):
    return cont(self)
   
  def __repr__(self):
    return ',@%s'%repr(self.item)

class DaoSyntaxError: pass

def unquote(item):
  return Unquote(element(item))

class Unquote(CommandCall):
  def __init__(self, item):
    self.item = item
    
  def alpha_convert(self, env, compiler):
    return Unquote(self.item.alpha_convert(env, compiler))
  
  def cps_convert(compiler, cont, arg):
    raise DaoSyntaxError

  def quasiquote(self, compiler, cont):
    return self.item.cps_convert(compiler, cont)
  
  def optimization_analisys(self, data):  
    return  
  
  def to_code(self, coder):
    return ''
  
  def __repr__(self):
    return ',@%s'%repr(self.item)
  
def unquote_splice(item):
  return UnquoteSplice(element(item))

class UnquoteSplice(CommandCall):
  def __init__(self, item):
    self.item = item
    
  def alpha_convert(self, env, compiler):
    return UnquoteSplice(self.item.alpha_convert(env, compiler))
  
  def cps_convert(compiler, cont, arg):
    raise DaoSyntaxError

  def quasiquote(self, compiler, cont):
    v = compiler.new_var(il.LocalVar('v'))
    return self.item.cps_convert(compiler, il.clamda(v, cont(il.UnquoteSplice(v))))
  
  def __repr__(self):
    return ',@%s'%repr(self.item)