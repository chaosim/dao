from dao.env import ModuleEnvironment
from dao.term import var
from dao.builtin import collocet_builtins_to_module
from dao.t.builtins.globalenv import global_env

from dao.builtins.parser import *

parser = ModuleEnvironment({}, None)
global_env[var('parser')] = parser
collocet_builtins_to_module(globals(), global_env, parser)
