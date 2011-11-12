# -*- coding: utf-8 -*-

from dao import term
from dao.builtins import arith
from dao.term import Cons, nil, conslist as L, cons2tuple 
from dao.term import var, vars, DummyVar #, Command, CommandCall
from dao.special import function, eval_, from_, quote, in_module, set
from dao.solve import to_sexpression

from dao import builtin

from dao.builtins.control import and_p, or_p, if_p, not_p
from dao.builtins.matcher import null, optional
from dao.builtins import terminal
from dao.builtins.terminal import char, spaces0, spaces, wrap_spaces0, eoi, literal
from dao.builtins.terminal import dqstring, not_lead_chars, not_follow_chars
from dao.builtins.term import setvalue, pycall, is_, define
from dao.builtins.quasiquote import quasiquote, unquote, unquote_splice

from dao.t.builtins.globalenv import classic as classic_module

x, y, var1, exp, exp2, exp_list, stmt, result = vars('x, y, var1, exp, exp2, exp_list, stmt, result')
statement, program = vars('statement, program')
expression, expression_satement, dec_inc_expression = vars('expression, expression_satement, dec_inc_expression')
op, sign, dec_inc, number, string, identifier, atom = vars('op, sign, dec_inc, number, string, identifier, atom')

_ = DummyVar('_')

defines = in_module(classic_module,

define(program, function(
  ([stmt], statement(stmt)),
  )),  

define(statement, function(
  ([stmt], expression_satement(stmt)),
  )),  

define(expression_satement, function(
  ([exp], expression(exp)),
  )),  

define(expression, function(
  ([exp], atom(exp)),
  )),  

define(atom, function(
  ([exp], number(exp)),
  ([exp], string(exp)),
  ([exp], identifier(exp)),
  ([exp], dec_inc_expression(exp)),
  )),  

define(number, function(
  ([exp], spaces0(_), sign(op), spaces0(_), 
          or_p(terminal.integer(exp2), terminal.float(exp2)), #terminal.number(exp2), #
          is_(exp, op(exp2))),
  )),

define(sign, function(
  ([arith.neg], char('-')),
  ([arith.pos], optional(char('+')))
  )),

define(string, function(
  ([exp], dqstring(exp)),
  )),  

define(identifier, function(
  ([exp], terminal.uLetterdigitString(exp2), is_(exp, pycall(var, exp2))),
  )),

define(dec_inc_expression, function(
  ([(set, var1, (op, var1, 1))], identifier(var1), dec_inc(op)),
  )),

define(dec_inc, function(
  ([arith.add], wrap_spaces0(literal('++'))),
  ([arith.sub], wrap_spaces0(literal('--')))
  ))
)
