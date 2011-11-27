from dao.env import GlobalEnvironment, ModuleEnvironment
from dao.solve import BaseCommand
from dao.command import Command
from dao.special import SpecialForm

global_env = GlobalEnvironment({})

from dao.base import is_subclass
from dao.term import var

def get_var(builtin, name):
  try: symbol = builtin.symbol
  except:
    try: symbol = builtin.name
    except: symbol = name
  return var(symbol)
  
def collocet_builtins(globls, global_env, module): 
  for name, obj in globls.items():
    if isinstance(obj, Command) or is_subclass(obj, SpecialForm):
      v = get_var(obj, name)
      module[v] = obj
      if obj.is_global: global_env[v] = obj
      
t = ModuleEnvironment({}, global_env, 't')
grammar = ModuleEnvironment({}, global_env, 'gammar')
sexpression = ModuleEnvironment({}, global_env, 'sexpression')
classic = ModuleEnvironment({}, global_env, 'classic')
global_env[var('t')] = t
t[var('grammar')] = grammar
grammar[var('sexpression')] = sexpression
grammar[var('classic')] = classic