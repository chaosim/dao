import os

from dao.command import special, BuiltinFunction
from dao.builtins.special import begin
from dao import interlang as il

# intput and output

format = BuiltinFunction('format', il.Format)
concat = BuiltinFunction('concat', il.Concat)

open_file = BuiltinFunction('open', il.OpenFile)
close_file = BuiltinFunction('close', il.CloseFile)
read = BuiltinFunction('read', il.ReadFile)
readline = BuiltinFunction('readline', il.Readline)
readlines = BuiltinFunction('readlines', il.Readlines)

@special
def prin_(compiler, cont, argument):
  v = compiler.new_var(il.ConstLocalVar('v'))
  return argument.cps(compiler, 
           il.clamda(v, il.Prin(v), cont(il.NONE)))

def prin(*args):
  return prin_(concat(*args))

@special
def println_(compiler, cont, argument):
  v = compiler.new_var(il.ConstLocalVar('v'))
  return argument.cps(compiler, 
           il.clamda(v, il.PrintLn(v), cont(il.NONE)))

def println(*args):
  return println_(concat(*args))

@special
def write_(compiler, cont, file, argument):
  v1 = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  return file.cps(compiler, 
    il.clamda(v1, argument.cps(compiler, 
           il.clamda(v2, il.WriteFile(v1, v2), cont(il.NONE)))))

def write(file, *args):
  return write_(file, concat(*args))
