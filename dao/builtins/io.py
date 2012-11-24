import os
from dao import interlang as il
from dao.command import BuiltinFunction

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

prin = BuiltinFunction('prin', il.Prin)
println = BuiltinFunction('println', il.PrintLn)

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
