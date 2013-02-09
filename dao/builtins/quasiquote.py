from dao.command import CommandCall, element
from dao import interlang as il

# quasiquote and backquote

def quasiquote(item):
  return Quasiquote(element(item))

class Quasiquote(CommandCall):
  def __init__(self, item):
    self.item = item
    
  def alpha(self, env, compiler):
    return Quasiquote(self.item.alpha(env, compiler))
  
  def cps(self, compiler, cont):
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
    
  def alpha(self, env, compiler):
    return Unquote(self.item.alpha(env, compiler))
  
  def cps(compiler, cont, arg):
    raise DaoSyntaxError

  def quasiquote(self, compiler, cont):
    return self.item.cps(compiler, cont)
  
  def analyse(self, compiler):  
    return  
  
  def to_code(self, compiler):
    return ''
  
  def __repr__(self):
    return ',@%s'%repr(self.item)
  
def unquote_splice(item):
  return UnquoteSplice(element(item))

class UnquoteSplice(CommandCall):
  def __init__(self, item):
    self.item = item
    
  def alpha(self, env, compiler):
    return UnquoteSplice(self.item.alpha(env, compiler))
  
  def cps(compiler, cont, arg):
    raise DaoSyntaxError

  def quasiquote(self, compiler, cont):
    v = compiler.new_var(il.ConstLocalVar('v'))
    return self.item.cps(compiler, il.clamda(v, cont(il.UnquoteSplice(v))))
  
  def __repr__(self):
    return ',@%s'%repr(self.item)