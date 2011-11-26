from dao.builtins.compiled.parser import *
from dao.builtins.compiled.terminal import *
from dao.base import apply_generator_fun_list

def builtin_predicate_fun():
  for values in apply_generator_fun_list([1, 1]):
    for x in eq_p(*values):
      yield x
for x in builtin_predicate_fun():
  print x
