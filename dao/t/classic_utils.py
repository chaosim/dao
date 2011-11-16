from dao import builtin

from dao.term import Var, var, vars, dummies, nullvars #, Command, CommandCall

from dao.special import begin, set, iff, case, if_, block, exit_block, continue_block

from dao.builtins.arith import eq, sub

@builtin.function('make_iff')
def make_iff(test, clause, clauses, els_clause):
  els_clause = els_clause if not isinstance(els_clause, Var) else None
  return (iff, ((test, clause),)+tuple(clauses), els_clause)

@builtin.function('make_case')
def make_case(test, cases, els_clause): 
  els_clause = els_clause if not isinstance(els_clause, Var) else None
  case_dict = {}
  for values, clause in cases:
    for x in values: case_dict[x] = clause
  return case(test, case_dict, els_clause)

label_surfix = '$'
new_label_id = 1
label_stack_dict = {}

@builtin.function('get_label')
def get_label(): 
  global new_label_id
  label = 'label_%s$'+str(new_label_id)
  new_label_id += 1
  return label

@builtin.function('push_label')
def push_label(control_struct_type, label):
  label_stack_dict.setdefault(control_struct_type, []).append(label)
  label_stack_dict.setdefault(None,[]).append(label)
  
@builtin.function('pop_label')
def pop_label(control_struct_type):
  label_stack_dict[control_struct_type].pop()
  label_stack_dict[None].pop()

@builtin.function('make_loop_times')
def make_loop_times(label, times, body): 
  i = Var('loop_i')
  start_condition = (if_, (eq, i,0), (exit_block, label), (set, i, (sub, i, 1)))
  body = (start_condition,)+tuple(body)+((continue_block, label),)
  return (begin, (set, i, times), (block, label)+ body)

# variables

x, y, z, = vars('x, y, z')

from dao.t.operator import operator, left, right

# function names

statement, statement_list, program = vars('statement, statement_list, program')

assign_statement, expression_statement = vars('assign_statement, expression_statement ')
statement_body, statement_end, statement_sequence, = vars(
  'statement_body, statement_end, statement_sequence')

expression, dec_inc_expression, binary_expression, assign_expression  = vars(
  'expression, dec_inc_expression, binary_expression, assign_expression')

assign, let_bindings, binding, loop_times = vars(
  'assign, let_bindings, binding, loop_times')

sign, dec_inc, number, string, identifier, atom = vars('sign, dec_inc, number, string, identifier, atom')
binary_operator = vars('binary_operator, op_func')

# classic grammar definitions

# statement type
(st_expression, st_assign, st_loop, st_block, st_if, st_case, st_let, st_defun, st_defmacro,
 st_print, st_bracket, st_sequence
 ) = range(12)

# expression type
(et_comma, et_list, et_tuple, et_assign, et_unary, et_inc_dec, et_binary, et_augment_assign,
  et_number, et_string, et_identifier, et_atom) = range(12)


name, var1, stmt_type, label = vars('name, var1, stmt_type, label')
exp, exp1, exp2, exp3, exp_list, stmt, stmt_list, body = vars(
  'exp, exp1, exp2, exp3, exp_list, stmt, stmt_list, body')
code, result = vars('code, result')
op, op_name, op_name2, op_func, op_func2, et_type1, et_type2 = vars(
  'op, op_name, op_name2, op_func, op_func2, et_type1, et_type2')
prior, prior1, prior2, prior3, assoc, assoc1, assoc2, assoc3 = vars(
  'prior, prior1, prior2, prior3, assoc, assoc1, assoc2, assoc3')

