from dao.env import ModuleEnvironment
from dao.term import var
from dao.t.builtins.globalenv import global_env, collocet_builtins

from dao.special import *

special = ModuleEnvironment({}, None, 'special')
global_env[var('special')] = special
collocet_builtins(globals(), global_env, special)
