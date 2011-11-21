# -*- coding: utf-8 -*-

from dao.t.classic_utils import *

from dao.t.builtins.globalenv import classic as classic_module

# dummies
from dao.term import dummies

_, _x, _type, _exp, _exp1, _exp2, _stmt = dummies('_, _x, _type, _exp, _exp1, _exp2, _stmt')

__, __type, __prior, __assoc = nullvars(4)

block_comment = t_default_block_comment

# =====================================================
# classic grammar for t language

defines = in_module(classic_module,

define(program, function(
  ([exp], phrases(exp, 0), eoi), # indent: 0
  )),  

define(phrases, function(
  ([exp, indent], any(phrase(_exp, indent), _exp, exp, greedy), eoi),
  )),  

define(phrase, function(
  ([exp, indent], and_p(any(noncode_line(), None, None, greedy),
                any(statement_line(_exp, indent), _exp, exp, greedy), eoi),
  )),  

define(line_head_spaces, tabspaces0),

define(noncode_line, function(
  ([], tabspaces0, or_p(nl, eoi)),
  ([], tabspaces0, line_comment(),or_p(nl, eoi)),
  ([], tabspaces0, block_comment, tabspaces0, or_p(nl, eoi)),
  )),

define(statement_line, function(
  ([exp, indent], spaces(indent), statement_body(),statement_end()),
  )),

define(line_comment, function(
  ([], tabspaces0, literal('//'), any_chars_except('\r\n')), nl),
  )),

define(statement_sequence, function(
  ([exp], statement_list(stmt_list), 
      set(stmt_list, pycall(tuple, stmt_list)),
      concat((begin,), stmt_list, exp)           
      ),
  )),  

define(statement_block, function(
  ([stmt_list], 
      statement_block_limiter(left, block_limiter_type), #indent/unindent 
      statement_list(stmt_list, block_limiter_type),
      statement_block_limiter(right, block_limiter_type)
      ),
  )),  

define(statement_list, function(
  ([stmt_list], some(and_p(tabspaces0, statement(_stmt, __type)), _stmt, stmt_list, greedy), 
           ),
  )),  

define(statement, function(
  ([exp, stmt_type],  
       statement_body(exp, stmt_type),
       statement_end(),
       ),
  )),

define(statement_end, function(
  ([], tabspaces0, # prin(exp),
       or_p(eoi, 
            and_p(char(';'), not_p(and_p(in_loop(), follow(loop_end_condition(loop_type))))),
            newline, 
            follow(statement_block_limiter(right, lt_bracket)),
            and_p(in_loop(), follow(loop_end_condition(loop_type))
            ), 
       #println('stmt end', position(), left_text())
       )
  )),

define(statement_body, function(
  
  # print statement
  ([(prin, exp), st_print],
      literal('print'), tabspaces, 
      #println('print2:', left_text()), 
      expression(exp, __type),
      ),
  
  # indent group statement
  ([exp, st_indent_group],
      statement_block(exp1),
      is_(exp, make_begin(exp1))
      ),
    
  # line group statement
  ([exp, st_line_group],
      statement_block(exp1),
      is_(exp, make_begin(exp1))
      ),
    
  # block statement
  ([exp, st_block],
      literal('block'),optional(and_p(tabspaces, identifier(label))), 
      push_label('block', get_label(label)),
      statement_block(stmt_list),
      pop_label('block'),
      is_(exp, make_block(block, label, stmt_list),
      ),
    
  # let statement
  ([(let, exp1, body), st_let],
      #prin('let statement:', position()),
      literal('let'), 
      #prin('let2', position()), 
      tabspaces, 
      #prin('let3', position()), 
      let_bindings(exp1), wrap_tabspaces(literal('do')), statement(body, __type),
      statement_end(),
      ),
  
  # if-elif-else statement
  ([exp, st_if],
      literal('if'), tabspaces, expression(exp1, __type), #println('if2:', left_text()),
      #println('if statement:', position()),
      wrap_tabspaces(literal('then')), #println('if12:', position()), 
      statement(exp2, __type), #println('if3:', left_text()),
      
      any(and_p(wrap_tabspaces(literal('elsif')), 
                expression(_exp1, __type), wrap_tabspaces(literal('then')),
                statement(_exp2, __type)),
          (_exp1, _exp2), exp_list),
      #prin('if4:', position()),
      #error(),
      optional(and_p(wrap_tabspaces(literal('else')), statement_body(exp3, __type)), greedy),
      #prin('if5:', left_text()),
      is_(exp, make_iff(exp1, exp2, exp_list, exp3)),
      ),
  
  # case statement
  ([exp, st_case],
      literal('case'), 
      #println('case statement:', position()),
      tabspaces, expression(exp1, __type), #println('case2:', left_text()),
      
      some(and_p(wrap_tabspaces(literal('of')), 
                seplist_times_more(expression(_x, __type), wrap_tabspaces0(char(',')), 1, _x, _exp1),
                wrap_tabspaces0(literal(':')),
                some(statement(_x, __type), _x, _exp2)),
          (_exp1, _exp2), exp_list),
      #prin('case4:', position()),
      optional(and_p(wrap_tabspaces(literal('else')), statement_body(exp3, __type)), greedy),
      #prin('case5:', left_text()),
      is_(exp, make_case(exp1, exp_list, exp3)),
      ),
  
  # loop statement
  ([exp, st_loop],
      literal('loop'),
      #println('loop statement:', position()),
      is_(label, get_label()),
      or_p( loop(exp, label),
            loop_times(exp, label),
          ),
      ),
  
  # while-loop statement
  ([exp, st_loop],
      #println('while loop statement start:', position(), left_text()),
      literal('while'),
      #println('while ffd:', position()),
      tabspaces, 
      expression(exp1, __type), 
      tabspaces, literal('loop'), tabspaces0, char(':'), tabspaces0,
      is_(label, get_label()),
      push_label('while', label),
      #println('while stmts:', position()),
      statement_list(exp2),
      #println('while ffdsdsd:', position()),
      pop_label('while'),      
      is_(exp, make_while_loop(label, exp1, exp2)),
      ),
  
  # expression statement
  ([exp, st_expression],  
       #println('expression_statement:', position()), 
       expression(exp, __type),
       ),
  
  # pass statement
  ([succeed, st_pass],  
       #println('expression_statement:', position()), 
       literal('pass'),
       ),
  
  # break statement
  ([exp, st_break],  
       literal('break'), #println('break:', position()), 
       tabspaces_if_need, optional(
         or_p( identifier(exp1), 
               and_p(optional(and_p(digit(digit1), tabspaces, literal('levels'))),
                     optional(pad_tabspaces, and_p(word(w1), optional(and_p(tabspaces, word(w2)))))
                     ) ) ),
       #println('break2:', position()),
       optional(expression(exp2, __type)),
       #println('break3:', position()),
       #fail,
       is_(exp, make_break(exp1, digit1, w1, w2, exp2)) 
       ),
  
  # redo statement
  ([exp, st_redo],  
       literal('redo'), 
       #println('redo:', position()), 
       tabspaces_if_need, optional(
         or_p( identifier(exp1), 
               and_p(optional(and_p(digit(digit1), tabspaces, literal('levels'))),
                     optional(pad_tabspaces, and_p(word(w1), optional(and_p(tabspaces, word(w2)))))
                     ) ) ),
       #println('redo2:', position()),
       #optional(expression(exp2, __type)),
       #println('redo3:', position()),
       #fail,
       is_(exp, make_redo(exp1, digit1, w1, w2)) 
       ),
  
  # function statement
  ([exp, st_function],  
       literal('fun'), 
       println('fun:', position(), left_text(5)), 
       or_p(fun_macro_define(exp, st_function),
            fun_macro_remove(exp, st_function),
            ),
       ),
  
  # macro statement
  ([exp, st_macro],  
       literal('macro'), 
       #println('fun:', position()), 
       or_p(fun_macro_define(exp, st_macro),
            fun_macro_remove(exp, st_macro),
            ),
       ),
  
  )),  

define(fun_macro_define, function(
  ([exp, def_type], 
    or_p( 
      and_p(tabspaces0, follow_char('('), fun_macro_define_body(exp1),
        is_(exp, make_definition(exp1, def_type)) ),
      and_p(tabspaces, varname(var1), 
        println('def_name:', position(), left_text(5)), 
        or_p(
          and_p(tabspaces0, fun_macro_define_body(exp1),
                is_(exp, update_matched_definition(exp1, def_type)) ),
          and_p(tabspaces0, ch('='), fun_macro_define_body(exp1),
                is_(exp,  replace_whole_definition(exp1, def_type)) ),
          and_p(tabspaces0, ch('+'), fun_macro_define_body(exp1),
                is_(exp, append_definition(exp1, def_type)) ),
          and_p(tabspaces0, ch('%'), fun_macro_define_body(exp1),
                is_(exp, insert_definition(exp1, def_type)) ),
         )
        )
      )
  ),
  )),

define(fun_macro_define_body, function(
  ([exp], 
    tabspaces0, expression(exp1, et_parenthesis), tabspaces0, ch(':'), tabspaces0, statement_sequence(exp2)
  ),
  )),

define(fun_macro_remove, function(
  ([exp, def_type], 
    wrap_tabspaces0(ch('-')), 
    seplist(fun_macro_remove_item(_exp), wrap_tabspaces0(ch(',')), _exp, exp1, 1, greedy),
    is_(exp, make_remove_expression(exp1, def_type)),
  ),
  )),

define(fun_macro_remove_item, function(
  ([exp, def_type], 
    or_p(and_p(varname(exp1), is_(exp, make_remove_item1(exp1))),
         and_p(varname(exp1), 
               wrap_tabspaces0(ch('/')), 
               expression(exp2,__type),
               make_remove_item2(exp1, exp2)),
         and_p(varname(exp1), 
               some(and_p(tabspaces0, expression(_exp, et_parenthesis)), _exp, exp2),
               make_remove_item3(exp1, exp2))
        ),
  )
  )),

define(loop, function(
  # loop
  ([exp, label], 
      #println('loop:', position()),
      push_label('loop', label),
      tabspaces0, char(':'), tabspaces0,
      #println('loop:', left_text()),
      statement_list(exp1),
      #println('loop end:', exp1),
      or_p(
        #println('badenv'),
        if_p(and_p(tabspaces0, literal('until'), tabspaces, expression(exp2, __type)),
           is_(exp, make_loop_until(label, exp1, exp2))),
        #println('loop end after until:', position(), left_text()),
        if_p(and_p( #println('what here:', position(), left_text()),
                   tabspaces0, literal('while'), tabspaces,
                   #println('what here:', position(), left_text()),
                   expression(exp2, __type),
                   #println('while ok:', exp1, exp2, position(), left_text())
                   ),
           #println('while ok2:', exp1, exp2, position(), left_text()),
           is_(exp, make_loop_while(label, exp1, exp2)),
           #is_(exp, (quote, (println, 1))),
           ),
        is_(exp, make_loop(label, exp1)) 
        ),
      #println('while ok3:', exp1, exp2, position(), left_text()),
      #error('in loop'),
      #println('while ok4:', exp1, exp2, position(), left_text()),
      pop_label('loop'),
      #println('end loop:', make_loop_while(label, exp1, exp2)),
      #fail
  ),
  )),

define(loop_times, function(
  # loop times
  ([exp, label], 
      #println('loop_times:', position()),
      push_label('loop times', label),
      tabspaces, expression(exp1, __type), tabspaces, literal('times'), tabspaces0, char(':'), tabspaces0,
      #println('loop_times:', left_text()),
      statement_list(exp2),
      is_(exp, make_loop_times(label, exp1, exp2)),
      pop_label('loop times'),
  ),
  )),

define(let_bindings, function(
  # assign expression
  ([exp], 
     #prin('let_bindings:', position()),
     seplist(binding(_exp), wrap_tabspaces0(char(',')), _exp, exp)
  ),
  )),

define(binding, function(
  ([(var1, exp)], varname(var1), #prin('id2'),
          #is_(var, pycall(var, name)),
          wrap_tabspaces0(char('=')), #prin('='), 
          expression(exp, _)),
  )),

define(expression, function(
  # assign expression
  ([exp, et_assign], 
     #println('assign expression fsfdfd:', position()),
     assign(exp),
     #println('assign expression2:', exp, left_text()),
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
     tabspaces0, 
     #println('bes', position()),
     operator(2, op_name, prior, assoc, op_func), gt_p(prior1, prior), 
     #println('beso', position()),     
     #println(op_func, exp1, 'at:', position()),
     tabspaces0,
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
          wrap_tabspaces0(char('=')), #prin('='), 
          expression(exp, _)),
  )),

define(atom, function(
  ([exp], number(exp)),
  ([exp], string(exp)),
  ([exp], varname(exp)),
  )),

define(number, function(
  ([exp], sign(op), #tabspaces0, 
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
  ([(set, var1, (op, var1, 1))], varname(var1), tabspaces0, dec_inc(op)),
  )),

define(dec_inc, function(
  ([arith.add], literal('++')),
  ([arith.sub], literal('--'))
  )),

)
