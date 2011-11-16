# -*- coding: utf-8 -*-

from dao import term
from dao.builtins import arith
from dao.term import Cons, nil, conslist as L, cons2tuple 
from dao.special import function, eval_, from_, quote, in_module, set, begin
from dao.special import let
from dao.solve import to_sexpression

from dao.builtins.io import prin, println
from dao.builtins.control import and_p, or_p, if_p, not_p
from dao.builtins.parser import position, left as left_text
from dao.builtins.matcher import null, optional
from dao.builtins.matcher import make_any as any, make_some as some, make_seplist as seplist, greedy
from dao.builtins.matcher import make_seplist_times_more as seplist_times_more
from dao.builtins.container import concat, pytuple
from dao.builtins import terminal
from dao.builtins.terminal import char, spaces0, spaces, wrap_spaces0, wrap_spaces, eoi, literal
from dao.builtins.terminal import dqstring, not_lead_chars, not_follow_chars
from dao.builtins.term import setvalue, pycall, is_, define
from dao.builtins.quasiquote import quasiquote, unquote, unquote_splice
from dao.builtins.arith import ge_p, gt_p

from dao.t.builtins.globalenv import classic as classic_module

from dao.t.classic_utils import *

# dummies
from dao.term import dummies

_, _x, _type, _exp, _exp1, _exp2, _stmt = dummies('_, _x, _type, _exp, _exp1, _exp2, _stmt')

__, __type, __prior, __assoc = nullvars(4)


# =====================================================
# classic grammaar for t language

defines = in_module(classic_module,

define(program, function(
  ([exp], statement_sequence(exp)+spaces0(_)),
  )),  

define(statement_sequence, function(
  ([exp], statement_list(stmt_list), 
      set(stmt_list, pycall(tuple, stmt_list)),
      concat((begin,), stmt_list, exp)           
      ),
  )),  

define(statement_list, function(
  ([stmt_list], some(and_p(spaces0(_), statement(_stmt, __type)), _stmt, stmt_list, greedy), 
           ),
  )),  

define(statement, function(
  ([exp, stmt_type],  
       statement_body(exp, stmt_type),
       statement_end(),
       ),
  )),

define(statement_body, function(
  
  # print statement
  ([(prin, exp), st_print],
      literal('print'), spaces(_), 
      #println('print2:', left_text()), 
      expression(exp, __type),
      ),
  
  # bracket statement
  ([exp, st_bracket],
      char('{'), 
      #println('bracket:', left_text()), 
      wrap_spaces0(statement_sequence(exp)), char('}'),
      ),
    
  # let statement
  ([(let, exp1, body), st_let],
      #prin('let statement:', position()),
      literal('let'), 
      #prin('let2', position()), 
      spaces(_), 
      #prin('let3', position()), 
      let_bindings(exp1), wrap_spaces(literal('do')), statement(body, __type),
      statement_end(),
      ),
  
  # if-elif-else statement
  ([exp, st_if],
      literal('if'), spaces(_), expression(exp1, __type), println('if2:', left_text()),
      println('if statement:', position()),
      wrap_spaces(literal('then')), #println('if12:', position()), 
      statement(exp2, __type), println('if3:', left_text()),
      
      any(and_p(wrap_spaces(literal('elif')), 
                expression(_exp1, __type), wrap_spaces(literal('then')),
                statement(_exp2, __type)),
          (_exp1, _exp2), exp_list),
      #prin('if4:', position()),
      optional(and_p(wrap_spaces(literal('else')), statement_body(exp3, __type)), greedy),
      prin('if5:', left_text()),
      is_(exp, make_iff(exp1, exp2, exp_list, exp3)),
      ),
  
  # case statement
  ([exp, st_case],
      literal('case'), 
      println('case statement:', position()),
      spaces(_), expression(exp1, __type), println('case2:', left_text()),
      
      some(and_p(wrap_spaces(literal('of')), 
                seplist_times_more(expression(_x, __type), wrap_spaces0(char(',')), 1, _x, _exp1),
                wrap_spaces0(literal(':')),
                some(statement(_x, __type), _x, _exp2)),
          (_exp1, _exp2), exp_list),
      prin('case4:', position()),
      optional(and_p(wrap_spaces(literal('else')), statement_body(exp3, __type)), greedy),
      prin('case5:', left_text()),
      is_(exp, make_case(exp1, exp_list, exp3)),
      ),
  
  # loop statement
  ([exp, st_loop],
      println('loop statement start:', position(), left_text()),
      literal('loop'),
      println('loop statement:', position()),
      is_(label, get_label()),
      or_p( loop(exp, label),
            loop_times(exp, label),
          ),
      ),
  
  # expression statement
  ([exp, st_expression],  
       println('expression_statement:', position()), 
       expression(exp, __type),
       ),
  
  )),  

define(loop, function(
  # loop
  ([exp, label], 
      println('loop:', position()),
      push_label('loop', label),
      spaces0(_), char(':'), spaces0(_),
      println('loop:', left_text()),
      statement_list(exp1),
      or_p(
        if_p(and_p(wrap_spaces(literal('until')), expression(exp2, __type)),
           is_(exp, make_loop_until(label, exp1, exp2))),
        if_p(and_p(wrap_spaces(literal('while')), expression(exp2, __type)),
           is_(exp, make_loop_while(label, exp1, exp2))),
        is_(exp, make_loop(label, exp1))),
      pop_label('loop'),
      not_p(and_p(spaces(_), or_p(literal('until'), literal('while')))), 
  ),
  )),

define(loop_times, function(
  # loop times
  ([exp, label], 
      println('loop_times:', position()),
      push_label('loop times', label),
      spaces(_), expression(exp1, __type), spaces(_), literal('times'), spaces0(_), char(':'), spaces0(_),
      println('loop_times:', left_text()),
      statement_list(exp2),
      is_(exp, make_loop_times(label, exp1, exp2)),
      pop_label('loop times'),
  ),
  )),

#define(loop_until, function(
  ## loop until
  #([exp, label], 
      #println('loop:', position()),
      #push_label('loop', label),
      #spaces0(_), char(':'), spaces0(_),
      #println('loop:', left_text()),
      #statement_list(exp1),
      #wrap_spaces('until'),
      #expression(exp2, __type),
      #is_(exp, make_loop_until(label, exp1, exp2)),
      #pop_label('loop'),
  #),
  #)),

define(let_bindings, function(
  # assign expression
  ([exp], 
     #prin('let_bindings:', position()),
     seplist(binding(_exp), wrap_spaces0(char(',')), _exp, exp)
  ),
  )),

define(binding, function(
  ([(var1, exp)], varname(var1), #prin('id2'),
          #is_(var, pycall(var, name)),
          wrap_spaces0(char('=')), #prin('='), 
          expression(exp, _)),
  )),

define(statement_end, function(
  ([], spaces0(_), # prin(exp),
       or_p(eoi, char(';')), 
       println('stmt end', position(), left_text())
       )
  )),

define(expression, function(
  # assign expression
  ([exp, et_assign], 
     println('assign expression fsfdfd:', position()),
     assign(exp),
     println('assign expression2:', exp, left_text()),
  ),
  
  # binary expression
  ([exp, et_binary], 
     #prin('binary expression', position()),
     expression(exp, et_binary, __prior, __assoc)),
  
  ([(op_func, exp1, exp2), et_binary, prior2, assoc], 
     #println('b', position()),
     #println(op_func, exp1, 'at:', position()),
     expression(exp1, et_type1, prior1, assoc1),
     #println('be', position()),
     spaces0(_), 
     #println('bes', position()),
     operator(2, op_name, prior, assoc, op_func), gt_p(prior1, prior), 
     #println('beso', position()),     
     #println(op_func, exp1, 'at:', position()),
     spaces0(_),
     #println('besos', position()),
     expression(exp2, et_type2, prior2, assoc2), gt_p(prior2, prior),
     #println(op_func, exp1, exp2, 'at:', position()),
     #println('why'),
     not_p(and_p(operator(2, op_name2, prior3, assoc3, op_func2), gt_p(prior3, prior))),
     #println('end'),
     ),  

  # increment and decrement expresson
  ([exp, et_inc_dec], # prin('dec_inc_expression', position()), 
                      dec_inc_expression(exp)),
  
  # atom expression
  ([exp, et_atom], atom(exp)),
  ([exp, et_atom, 90, left], 
     atom(exp) # expression(exp, et_atom)
     ),

  ([exp, et_number], number(exp)),
  ([exp, et_string], string(exp)),
  ([exp, et_identifier],  #prin('in_id1'), 
                          varname(exp)),
  
  )),  

define(assign, function(
  ([(set, var1, exp)], varname(var1), #prin('id2'),
          #is_(var, pycall(var, name)),
          wrap_spaces0(char('=')), #prin('='), 
          expression(exp, _)),
  )),

define(atom, function(
  ([exp], number(exp)),
  ([exp], string(exp)),
  ([exp], varname(exp)),
  )),

define(number, function(
  ([exp], sign(op), #spaces0(_), 
          terminal.number(exp2), 
          is_(exp, op(exp2)),
          #println('num', position())
          ),  
  )),

define(sign, function(
  ([arith.neg], char('-')),
  ([arith.pos], optional(char('+')))
  )),

define(string, function(
  ([exp], dqstring(exp)),
  )),  

define(varname, function(
  ([exp], identifier(_exp), #prin('in_id'), 
          is_(exp, pycall(var, _exp))), 
  )),

define(dec_inc_expression, function(
  ([(set, var1, (op, var1, 1))], varname(var1), spaces0(_), dec_inc(op)),
  )),

define(dec_inc, function(
  ([arith.add], literal('++')),
  ([arith.sub], literal('--'))
  )),

)
