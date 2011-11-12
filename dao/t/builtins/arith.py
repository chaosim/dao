from dao.env import ModuleEnvironment
from dao.term import var
from dao.t.builtins.globalenv import global_env, collocet_builtins

from dao.builtins.arith import *

arith = ModuleEnvironment({}, None, 'arith')
global_env[var('arith')] = arith
collocet_builtins(globals(), global_env, arith)
