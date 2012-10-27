from dao.builtins.compiled.parser import *
from dao.builtins.compiled.terminal import *
from dao.base import apply_generator_fun_list

def builtin_macro_fun():
  for x in parse_sequence(char('a'), 'a'):
    yield x
for x in builtin_macro_fun():
  print x
