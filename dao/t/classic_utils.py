from dao import builtin

from dao.term import Var, var, vars, dummies, nullvars #, Command, CommandCall
from dao.term import unify
pyset = set

from dao.special import begin, set, iff, case, if_, block, exit_block, continue_block

from dao.builtins.arith import eq, sub, not_
from dao.builtins.matcher import matcher

keywords = pyset(['let', 'do', 'case', 'of', 'while', 'where', 'until', 'fun', 'macro', 'block',
                  'loop', 'if', 'elif', 'else', 'then', 'print', 'break', 'redo', 'pass', 'return'])

@matcher()
def identifier(solver, cont, arg):
  text, pos = solver.parse_state
  length = len(text)
  if pos>=length: return
  p = pos
  if text[p]!='_' and not 'a'<=text[p]<='z' and not 'A'<=text[p]<='Z': 
    return
  p += 1
  while p<length and (text[p]=='_' or 'a'<=text[p]<='z' or 'A'<=text[p]<='Z'
                       '0'<=text[p]<='9'): 
    p += 1
  w = text[pos:p]
  if w in keywords: return
  for _ in unify(arg, w, solver.env):
    solver.parse_state = text, p
    yield cont,  w
    solver.parse_state = text, pos

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

@builtin.predicate('get_label')
def get_label(solver, cont, label=None): 
  if isinstance(label, str): yield cont, label
  global new_label_id
  new_label = 'label_%s$'%str(new_label_id)
  new_label_id += 1
  if isinstance(label, Var):
    for _ in unify(label, new_label, solver.env):
      yield cont, new_label
  else: 
    yield cont, new_label
  new_label_id -= 1

#get_label = builtin.predicate('get_label')(take_label)

@builtin.predicate('push_label')
def push_label(solver, cont, control_struct_type, label):
  label_stack_dict.setdefault(control_struct_type, []).append(label)
  label_stack_dict.setdefault('', []).append(label)
  yield cont, label
  label = label_stack_dict[control_struct_type].pop()
  label_stack_dict[''].pop()
      
@builtin.predicate('pop_label')
def pop_label(solver, cont, control_struct_type):
  label = label_stack_dict[control_struct_type].pop()
  label_stack_dict[''].pop()
  yield cont, label
  label_stack_dict[control_struct_type].append(label)
  label_stack_dict[''].append(label)
  
@builtin.function('make_loop')
def make_loop(label, body): 
  body = tuple(body)+((continue_block, label),)
  return (begin, (block, label)+ body)

@builtin.function('make_loop_times')
def make_loop_times(label, times, body): 
  i = Var('loop_i')
  start_condition = (if_, (eq, i, 0), (exit_block, label), (set, i, (sub, i, 1)))
  body = (start_condition,)+tuple(body)+((continue_block, label),)
  return (begin, (set, i, times), (block, label)+ body)

@builtin.function('make_loop_until')
def make_loop_until(label, body, condition): 
  #`(block  ,label:{  ,@body;  if not( ,condition) next  ,label} )
  return (block, label)+tuple(body)+((if_, (not_, condition), (continue_block, label)),)

@builtin.function('make_loop_while')
def make_loop_while(label, body, condition): 
  #`(block  ,label:{  ,@body;  if ,condition then next  ,label} )
  return (block, label)+tuple(body)+((if_, condition, (continue_block, label)),)

@builtin.function('make_while_loop')
def make_while_loop(label, condition, body): 
  start_condition = (if_, (not_, condition), (exit_block, label))
  return (block, label, start_condition)+tuple(body)+((continue_block, label),)

@builtin.function('make_break')
def make_break(label, level, word1, word2, value): 
  if isinstance(label, Var):
    control_type = ''
    if not isinstance(word1, Var):
      control_type = word1
      if not isinstance(word2, Var):
        control_type += ' '+ word2
    if isinstance(level, Var): 
      level = 1
    label = label_stack_dict[control_type][-level]
  if isinstance(value, Var): value = None
  return exit_block(label, value)

@builtin.function('make_redo')
def make_redo(label, level, word1, word2): 
  if isinstance(label, Var):
    control_type = ''
    if not isinstance(word1, Var):
      control_type = word1
      if not isinstance(word2, Var):
        control_type += ' '+ word2
    if isinstance(level, Var): 
      level = 1
    label = label_stack_dict[control_type][-level]
  return continue_block(label)

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

assign, let_bindings, binding, loop, loop_times = vars(
  'assign, let_bindings, binding, loop, loop_times')

sign, dec_inc, number, string, varname, atom = vars('sign, dec_inc, number, string, varname, atom')
binary_operator = vars('binary_operator, op_func')

# classic grammar definitions

# statement type
(st_expression, st_assign, st_loop, st_block, st_if, st_case, st_let, st_defun, st_defmacro,
 st_print, st_bracket, st_sequence, st_pass, st_break, st_redo, st_block
 ) = range(16)

# expression type
(et_comma, et_list, et_tuple, et_assign, et_unary, et_inc_dec, et_binary, et_augment_assign,
  et_number, et_string, et_identifier, et_atom) = range(12)

w1, w2, name, var1, stmt_type, label, digit1 = vars('w1, w2, name, var1, stmt_type, label, digit1')
exp, exp1, exp2, exp3, exp_list, stmt, stmt_list, body = vars(
  'exp, exp1, exp2, exp3, exp_list, stmt, stmt_list, body')
code, result = vars('code, result')
op, op_name, op_name2, op_func, op_func2, et_type1, et_type2 = vars(
  'op, op_name, op_name2, op_func, op_func2, et_type1, et_type2')
prior, prior1, prior2, prior3, assoc, assoc1, assoc2, assoc3 = vars(
  'prior, prior1, prior2, prior3, assoc, assoc1, assoc2, assoc3')

