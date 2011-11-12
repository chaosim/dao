from dao.env import ModuleEnvironment
from dao.term import var
from dao.t.builtins.globalenv import global_env, collocet_builtins

from dao.builtins.parser import *

parser = ModuleEnvironment({}, None, 'parser')
global_env[var('parser')] = parser
collocet_builtins(globals(), global_env, parser)
