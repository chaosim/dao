from dao.env import ModuleEnvironment
from dao.term import var
from dao.t.builtins.globalenv import global_env, collocet_builtins

from dao.builtins.container import *
from dao.builtins.quasiquote import *

container = ModuleEnvironment({}, None, 'container')
global_env[var('container')] = container
collocet_builtins(globals(), global_env, container)
