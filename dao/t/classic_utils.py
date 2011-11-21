from dao import builtin

from dao import term
from dao.builtins import arith
from dao.term import Cons, nil, conslist as L, cons2tuple 
from dao.special import function, eval_, from_, quote, in_module, set, begin
from dao.special import let, block
from dao.solve import to_sexpression

from dao.builtins.io import prin, println
from dao.builtins.control import and_p, or_p, if_p, not_p, fail, succeed, error
from dao.builtins.parser import position, left as left_text
from dao.builtins.matcher import null, optional
from dao.builtins.matcher import make_any as any, make_some as some, make_seplist as seplist, greedy
from dao.builtins.matcher import make_seplist_times_more as seplist_times_more
from dao.builtins.container import concat, pytuple
from dao.builtins import terminal
from dao.builtins.terminal import ch, char, tabspaces0, tabspaces, whitespaces0, wrap_tabspaces0, wrap_tabspaces, eoi, literal
from dao.builtins.terminal import dqstring, not_lead_chars, not_follow_chars, digit, pad_tabspaces, word, tabspaces_if_need
from dao.builtins.terminal import follow_char
from dao.builtins.term import setvalue, pycall, is_, define
from dao.builtins.quasiquote import quasiquote, unquote, unquote_splice
from dao.builtins.arith import ge_p, gt_p

from dao.term import Var, var, vars, dummies, nullvars #, Command, CommandCall
from dao.term import unify
pyset = set

from dao.special import begin, set, iff, case, if_, block, exit_block, continue_block

from dao.builtins.arith import eq, sub, not_
from dao.builtins.matcher import matcher

keywords = pyset(['let', 'recur', 'do', 'case', 'of', 'while', 'when', 'until', 'block',
                  'loop', 'if', 'elif', 'else', 'print', 'break', 'redo', 'pass', 'return',
                  'fun', 'macro', 'on'])

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

fun_macro_define, fun_macro_remove, fun_macro_remove_item, fun_macro_define_body = vars(
  'fun_macro_define, fun_macro_remove, fun_macro_remove_item, fun_macro_define_body')

sign, dec_inc, number, string, varname, atom = vars('sign, dec_inc, number, string, varname, atom')
binary_operator = vars('binary_operator, op_func')

# classic grammar definitions

# statement type
(st_expression, st_assign, st_loop, st_block, st_if, st_case, st_let, st_function, st_macro,
 st_print, st_curly_bracket, st_sequence, st_pass, st_break, st_redo, st_block
 ) = range(16)

# expression type
(et_comma, et_list, et_tuple, et_assign, et_unary, et_inc_dec, et_binary, et_augment_assign,
  et_number, et_string, et_identifier, et_atom, et_parenthesis) = range(13)

w1, w2, name, var1, stmt_type, label, digit1 = vars('w1, w2, name, var1, stmt_type, label, digit1')
exp, exp1, exp2, exp3, exp_list, stmt, stmt_list, body = vars(
  'exp, exp1, exp2, exp3, exp_list, stmt, stmt_list, body')
code, result = vars('code, result')
op, op_name, op_name2, op_func, op_func2, et_type1, et_type2, def_type = vars(
  'op, op_name, op_name2, op_func, op_func2, et_type1, et_type2, def_type')
prior, prior1, prior2, prior3, assoc, assoc1, assoc2, assoc3 = vars(
  'prior, prior1, prior2, prior3, assoc, assoc1, assoc2, assoc3')

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

@builtin.function('make_definition')
def make_definition(exp, def_type):
  pass

@builtin.function('update_matched_definition')
def update_matched_definition(exp, def_type):
  pass

@builtin.function('replace_whole_definition')
def replace_whole_definition(exp, def_type):
  pass

@builtin.function('append_definition')
def append_definition(exp, def_type):
  pass

@builtin.function('insert_definition')
def insert_definition(exp, def_type):
  pass

@builtin.function('make_remove_expression')
def make_remove_expression(exp, def_type):
  pass

@builtin.function('make_remove_item1')
def make_remove_item1(exp):
  pass

@builtin.function('make_remove_item2')
def make_remove_item2(exp):
  pass

@builtin.function('make_remove_item3')
def make_remove_item3(exp):
  pass

@builtin.macro('block_comment')
def block_comment(solver, cont, start, end):
  '''embedable block comment'''
  text, pos = solver.parse_state
  length = len(text)
  startlen = len(start)
  endlen = len(end)
  if pos==length: return
  if not text[pos:].startswith(start):
    return
  level = 1
  p = pos+1
  while p<length:
    if text[p:].startswith(end):
      level -= 1
      p += endlen
      if level==0: break
    elif text[p:].startswith(start):
      level += 1
      p += startlen
    else:
      p += 1
  else: return
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos

t_default_block_comment = block_comment('/.', './')