# -*- coding: utf-8 -*-

from dao import term
from dao.builtins import arith
from dao.term import Cons, nil, conslist as L, cons2tuple 
from dao.term import var, vars, dummies #, Command, CommandCall
from dao.special import function, eval_, from_, quote, in_module, set, begin
from dao.solve import to_sexpression

from dao import builtin

from dao.builtins.io import prin
from dao.builtins.control import and_p, or_p, if_p, not_p
from dao.builtins.parser import position
from dao.builtins.matcher import null, optional, make_some as some, greedy
from dao.builtins.container import concat, pytuple
from dao.builtins import terminal
from dao.builtins.terminal import char, spaces0, spaces, wrap_spaces0, eoi, literal
from dao.builtins.terminal import dqstring, not_lead_chars, not_follow_chars
from dao.builtins.term import setvalue, pycall, is_, define
from dao.builtins.quasiquote import quasiquote, unquote, unquote_splice
from dao.builtins.arith import ge_p, gt_p

from dao.t.builtins.globalenv import classic as classic_module
from dao.t.operator import operator

# variables

x, y, z, = vars('x, y, z')

var1, exp, exp1, exp2, exp_list, stmt, stmt_list = vars(
  'var1, exp, exp1, exp2, exp_list, stmt, stmt_list')
code, result = vars('code, result')
op, op_name, prior, prior1, prior2, prior3, assoc, assoc1, assoc12 = vars(
  'op, op_name, prior, prior1, prior2, prior3, assoc, assoc1, assoc12')

# function names

statement, statement_list, program = vars('statement, statement_list, program')

assign_statement, expression_statement = vars('assign_statement, expression_statement ')

expression, dec_inc_expression, binary_expression, assign_expression  = vars(
  'expression, dec_inc_expression, binary_expression, assign_expression')

sign, dec_inc, number, string, identifier, atom = vars('sign, dec_inc, number, string, identifier, atom')
binary_operator, op_func = vars('binary_operator, op_func')

# dummies

_, _stmt = dummies('_, _stmt')

# classic grammar definitions

defines = in_module(classic_module,

define(program, function(
  ([stmt_list], statement_list(stmt_list)+spaces0(_)),
  )),  

define(statement_list, function(
  ([code], some(statement(_stmt), _stmt, stmt_list, greedy), 
           set(stmt_list, pycall(tuple, stmt_list)), 
           concat((begin,), stmt_list, code)),
  )),  

define(statement, function(
  ([stmt], expression_statement(stmt)),
  ([stmt], assign_statement(stmt)),
  )),  

define(assign_statement, function(
  ([exp], #prin('assign_statement', position()),
          assign_expression(exp), spaces0(_), or_p(eoi, char(';'))),
  )),  

define(assign_expression, function(
  ([(set, var1, exp)], #prin('id1', position()), 
      identifier(var1), #prin('id2'), 
      wrap_spaces0(char('=')),  #prin('='), 
      expression(exp, _)),
  )),  

define(expression_statement, function(
  ([exp], expression(exp, _)+spaces0(_), #prin('expression_statement', position()), 
       or_p(eoi, char(';')), #prin('finish expression_statement', position())
       ),
  )),  

define(expression, function(
  ([exp, 100], atom(exp)),
  ([exp, 90], # prin('dec_inc_expression', position()), 
          dec_inc_expression(exp)),
  ([exp, prior], # prin('binary_expression', position()), 
          binary_expression(exp, prior)),
  )),  

define(binary_expression, function(
  ([(op_func, exp1, exp2), prior2], 
     expression(exp1, prior1), 
     operator(2, op_name, prior2, assoc1, op_func), gt_p(prior1, prior2), 
     expression(exp2, prior3, assoc1), ge_p(prior2, prior3),
     ),
  )),  

define(atom, function(
  ([exp], number(exp)),
  ([exp], string(exp)),
  ([exp], identifier(exp)),
  )),  

define(number, function(
  ([exp], spaces0(_), sign(op), spaces0(_), 
          terminal.number(exp2), 
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
  ([exp], terminal.uLetterdigitString(exp2), is_(exp, pycall(var, exp2))), #prin('in_id'), 
  )),

define(dec_inc_expression, function(
  ([(set, var1, (op, var1, 1))], identifier(var1), dec_inc(op)),
  )),

define(dec_inc, function(
  ([arith.add], wrap_spaces0(literal('++'))),
  ([arith.sub], wrap_spaces0(literal('--')))
  ))
)
