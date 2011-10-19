from dao.special import quote, eval_, catch, throw, pytry
from dao.term import cons, conslist, nil

from dao.dinpy.dexpr import *
from dao.dinpy.dinpy import *
from dao.dinpy.vars import *

from dao.dinpy.arith import *
from dao.dinpy.control import *
from dao.dinpy.io import *
from dao.dinpy.string import *
from dao.dinpy.matcher import *
from dao.dinpy.term import *

from dao.solve import set_run_mode
set_run_mode(mode=interactive)

__all__ = globals().keys()+['_']
