from dao.env import ModuleEnvironment
from dao.term import var
from dao.t.builtins.globalenv import global_env, collocet_builtins

from dao.builtins.io import *

io = ModuleEnvironment({}, None, 'io')
global_env[var('io')] = io
collocet_builtins(globals(), global_env, io)
