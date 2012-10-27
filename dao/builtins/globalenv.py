from dao.env import GlobalEnvironment

global_env = GlobalEnvironment({})

from dao.base import is_subclass
from dao.term import var

def collocet_builtins(from_globls, to_globals): 
  for name, obj in globls.items():
    if isinstance(obj, Command):
      try: symbol = obj.symbol
      except:
        try: symbol = obj.name
        except: symbol = name
      v = var(symbol)
      global_env[v] = obj
