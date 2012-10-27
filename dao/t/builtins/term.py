from dao.env import ModuleEnvironment
from dao.term import var
from dao.t.builtins.globalenv import global_env, collocet_builtins

from dao.builtins.term import *

term = ModuleEnvironment({}, None, 'term')
global_env[var('term')] = term
collocet_builtins(globals(), global_env, term)
