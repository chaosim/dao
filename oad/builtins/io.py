import os
from oad import builtin

# output

@builtin.function()
def write(*args):
  #for arg in args: os.write(1, "%s, "%arg) 
  for arg in args: print arg,
  return True
  
@builtin.function()
def writeln(*args):
  #for arg in args: os.write(1, "%s, "%arg) 
  for arg in args: print arg,
  print
  return True
  
@builtin.function()
def nl(): 
  #os.write(1, "\n") 
  print; 
  return True
