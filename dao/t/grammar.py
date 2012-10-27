# -*- coding: utf-8 -*-

from dao.term import vars, cons2tuple
from dao.special import begin, set, to_sexpression, in_module, eval_, from_, function
from dao.builtins.control import and_p
from dao.builtins.io import prin
from dao.builtins.parser import set_text
from dao.builtins.terminal import eoi
from dao.builtins.term import is_, define, pycall

from dao.t.builtins.globalenv import grammar, sexpression, classic
from dao.t.sexpression import defines as sexpression_defines
from dao.t.classic import defines as classic_defines

start, exp, sexpression1, program, result = vars(
  'start, exp, sexpression, program, result')

def make_expression(text):
  code = begin(
    sexpression_defines, 
    classic_defines, 
    set_text(text),
    from_(classic, program)(exp), 
    eoi,
    eval_(pycall(cons2tuple, exp)))
  return to_sexpression(code)

