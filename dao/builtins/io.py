import os
from dao import builtin

# intput and output

@builtin.function()
def read(file):
  return file.read()

@builtin.function()
def read(file):
  return file.readline()

@builtin.function()
def read(file):
  return file.readlines()

@builtin.function()
def prin(*args):
  for arg in args: print arg,

@builtin.function()
def write(file, *args):
  if isinstance(file, str):
    file = open(file)
  for arg in args: file.write('%s'%arg)
  
@builtin.function()
def println(*args):
  for arg in args: print arg,
  print

@builtin.function()
def writeln(file, *args):
  if isinstance(file, str):
    file = open(file)
  for arg in args: file.write('%s'%arg)
  file.write('\n')
  
@builtin.function()
def nl(file=None): 
  if file is None: print
  else: file.write('\n')
