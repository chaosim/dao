from dao.env import ModuleEnvironment
from dao.term import var
from dao.t.builtins.globalenv import global_env, collocet_builtins

from dao.builtins.rule import *

rule = ModuleEnvironment({}, None, 'rule')
global_env[var('rule')] = rule
collocet_builtins(globals(), global_env, rule)
