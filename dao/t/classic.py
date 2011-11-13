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
from dao.t.operator import operator, left, right

# variables

x, y, z, = vars('x, y, z')

var1, exp, exp1, exp2, exp_list, stmt, stmt_list = vars(
  'var1, exp, exp1, exp2, exp_list, stmt, stmt_list')
code, result = vars('code, result')
op, op_name, prior, prior1, prior2, assoc, assoc1, assoc2 = vars(
  'op, op_name, prior, prior1, prior2, assoc, assoc1, assoc2')

# function names

statement, statement_list, program = vars('statement, statement_list, program')

assign_statement, expression_statement = vars('assign_statement, expression_statement ')

expression, dec_inc_expression, binary_expression, assign_expression  = vars(
  'expression, dec_inc_expression, binary_expression, assign_expression')

assign,  = vars(
  'assign')

sign, dec_inc, number, string, identifier, atom = vars('sign, dec_inc, number, string, identifier, atom')
binary_operator, op_func = vars('binary_operator, op_func')

# dummies

_, _type, _exp, _stmt = dummies('_, _type, _exp, _stmt')

# classic grammar definitions

# statement type
(st_expression, st_assign, st_loop, st_block, st_if, st_case, st_let, st_defun, st_defmacro
 ) = range(9)

# expression type
(et_comma, et_list, et_tuple, et_assign, et_unary, et_inc_dec, et_binary, et_augment_assign,
  et_number, et_string, et_identifier, et_atom) = range(12)

defines = in_module(classic_module,

define(program, function(
  ([stmt_list], statement_list(stmt_list)+spaces0(_)),
  )),  

define(statement_list, function(
  ([code], some(and_p(spaces0(_), statement(_stmt, _type)), _stmt, stmt_list, greedy), 
           set(stmt_list, pycall(tuple, stmt_list)), 
           concat((begin,), stmt_list, code)),
  )),  

define(statement, function(
  # expression statement
  ([exp, st_expression],  expression(exp, _type)+spaces0(_), #prin('expression_statement', position()), 
       or_p(eoi, char(';')), #prin('finish expression_statement', position())
       ),
  # assign statement
  ([exp, st_assign], #prin('assign_statement', position()),
         expression(exp, et_assign),
          spaces0(_), 
          or_p(eoi, char(';'))),
  )),  

define(expression, function(
  # assign expression
  ([exp, et_assign], #prin('assign_statement', position()),
         assign(exp),
         ),
  
  ([exp, et_number], number(exp)),
  ([exp, et_string], string(exp)),
  ([exp, et_identifier],  #prin('in_id1'), 
                          identifier(exp)),
  ([exp, et_atom], atom(exp)),
  ([exp, et_binary], expression(exp, et_binary, prior, assoc)),
  ([exp, et_atom, 90, left], expression(exp, et_atom)),
  
  ([exp, et_inc_dec], # prin('dec_inc_expression', position()), 
                      dec_inc_expression(exp)),
  
  # binary expression
  ([(op_func, exp1, exp2), et_binary, prior2, assoc], 
     expression(exp1, _type, prior1, assoc1),
     spaces0(_), 
     operator(2, op_name, prior, assoc, op_func), gt_p(prior1, prior), 
     spaces0(_),
     expression(exp2, _type, prior2, assoc2), gt_p(prior, prior2),
     ),  
  )),  

define(assign, function(
  ([exp], identifier(var1), #prin('id2'), 
          wrap_spaces0(char('=')), #prin('='), 
          expression(exp, _)),
  )),

define(atom, function(
  ([exp], number(exp)),
  ([exp], string(exp)),
  ([exp], identifier(exp)),
  )),

define(number, function(
  ([exp], sign(op), #spaces0(_), 
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
  ([exp], terminal.uLetterdigitString(_exp), #prin('in_id'), 
          is_(exp, pycall(var, _exp))), 
  )),

define(dec_inc_expression, function(
  ([(set, var1, (op, var1, 1))], identifier(var1), spaces0(_), dec_inc(op)),
  )),

define(dec_inc, function(
  ([arith.add], literal('++')),
  ([arith.sub], literal('--'))
  ))
)
