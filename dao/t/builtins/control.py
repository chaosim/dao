from dao.env import ModuleEnvironment
from dao.term import var
from dao.t.builtins.globalenv import global_env, collocet_builtins

from dao.builtins.control import *

control = ModuleEnvironment({}, global_env, 'control')
global_env[var('control')] = control
collocet_builtins(globals(), global_env, control)
