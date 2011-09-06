import os
from oad import builtin
from oad.term import SUCCESS

# output

@builtin.function()
def write(*args):
  #for arg in args: os.write(1, "%s, "%arg) 
  for arg in args: print arg,
  return SUCCESS
  
@builtin.function()
def writeln(*args):
  #for arg in args: os.write(1, "%s, "%arg) 
  for arg in args: print arg,
  print
  return SUCCESS
  
@builtin.function()
def nl(): 
  #os.write(1, "\n") 
  print; 
  return SUCCESS
