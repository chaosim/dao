from dao.env import ModuleEnvironment
from dao.term import var
from dao.builtin import collocet_builtins_to_module
from dao.t.builtins.globalenv import global_env

from dao.builtins.rule import *

rule = ModuleEnvironment({}, None)
global_env[var('rule')] = rule
collocet_builtins_to_module(globals(), global_env, rule)
