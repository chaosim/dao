import os

from dao.command import special
from dao.builtins.special import begin
from dao import interlang as il

# intput and output

#def format(format_string, *args):
  #return format_string%args

#@builtin.function()
#def read(file):
  #return file.read()

#@builtin.function()
#def readline(file):
  #return file.readline()

#@builtin.function()
#def readlines(file):
  #return file.readlines()

@special
def prin_(compiler, cont, argument):
  v = compiler.new_var(il.ConstLocalVar('v'))
  return argument.cps_convert(compiler, 
           il.clamda(v, il.Prin(v), cont(il.NONE)))

def prin(*args):
  return begin(*tuple(prin_(arg) for arg in args))

@special
def println_(compiler, cont, argument):
  v = compiler.new_var(il.ConstLocalVar('v'))
  return argument.cps_convert(compiler, 
           il.Clamda(v, il.PrintLn(v), cont(il.NONE)))

def println(*args):
  return begin(*tuple(println_(arg) for arg in args))

#@builtin.function()
#def println(*args):
  #for arg in args: print arg,
  #print

#@builtin.function()
#def write(file, *args):
  #if isinstance(file, str):
    #file = open(file)
  #for arg in args: file.write('%s'%arg)
  
#@builtin.function()
#def writeln(file, *args):
  #if isinstance(file, str):
    #file = open(file)
  #for arg in args: file.write('%s'%arg)
  #file.write('\n')
