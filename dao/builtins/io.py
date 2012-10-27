import os
from dao import builtin

# intput and output

@builtin.function()
def format(format_string, *args):
  return format_string%args

@builtin.function()
def read(file):
  return file.read()

@builtin.function()
def readline(file):
  return file.readline()

@builtin.function()
def readlines(file):
  return file.readlines()

@builtin.function('prin', 'print')
def prin(*args):
  for arg in args: 
    print arg,

@builtin.function()
def println(*args):
  for arg in args: print arg,
  print

@builtin.function()
def write(file, *args):
  if isinstance(file, str):
    file = open(file)
  for arg in args: file.write('%s'%arg)
  
@builtin.function()
def writeln(file, *args):
  if isinstance(file, str):
    file = open(file)
  for arg in args: file.write('%s'%arg)
  file.write('\n')
