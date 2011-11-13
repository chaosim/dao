from dao import builtin
from dao.fsm import FSM
from dao.builtins.matcher import matcher

from dao.builtins.arith import *

operation_dict = [{
# unary operator
'+':pos
},{
# binary operator
'+':add
}]

priority_dict = [{
# unary operator
'+': 90, '-': 90,  
'++': 95, '--': 95,
'~':85,
'not':35,
'not!':30,
'del': 23,
},{
# binary operator
'*': 60, '/': 60, '%': 60, '\\': 60, '**':60,
 '+': 55,  '-': 55,
'&': 50, 
'|': 48,
'^': 46,
'<<': 44, '>>': 44,
'=':40, '<>':40,'<':40, '<=':40,'>=':40, '>':40, 
'=!':38, '<>!':38,'<!':38, '<=!':38,'>=!':38, '>!':38, 
':=:':36, ':\:':36, ':=':36, #unify, notunify, is_
 'in': 35, 'is': 35, 'is a':35, 'is not': 35, 'not in': 35, 'and':35,  #is a: isinstance
'and':34,
'or':34,
'xor':33,
'and!':29,
'or!':28,
'xor!':27,
}]

left, right = 0, 1
assoc_dict = [{

# unary operator, default right

'++': left,
'--': left,
'!': left, 

# it is not necessary below, default right:
#'--': right_association,  

},{

# binary operator, default left:

# it is not necessary below, just as sample:
#'+': left_association, 
#'-': left_association,  

'<<': right
}]

op_fsm = [
# unary operator
FSM(priority_dict[0].keys()), 
# binary operator
FSM(priority_dict[1].keys())]

@matcher()
def operator(solver, cont, arity, symbol, prior, assoc, operation): 
  arity = getvalue(arity, solver.env, {})
  symbol = getvalue(arity, solver.env, {})
  text, pos = solver.parse_state
  if not isinstance(symbol, Var) and text[pos:].startswith(symbol): 
    for _ in unify(assoc, get_assoc(op_str), solver.env):
      for _ in unify(operation, get_operation(op_str), solver.env):
        for _ in unify(symbol, op_str, solver.env):
          solver.parser_state = text, pos+length
          yield cont, op_str
          solver.parser_state = text, pos
    return
  length = op_fsms[arity].match(text[pos])
  if length==0: return
  else:
    op_str = text[pos:pos+length]
    for _ in unify(prior, get_priority(op_str), solver.env):
      for _ in unify(assoc, get_assoc(op_str), solver.env):
        for _ in unify(operation, get_operation(op_str), solver.env):
          for _ in unify(symbol, op_str, solver.env):
            solver.parser_state = text, pos+length
            yield cont, op_str
            solver.parser_state = text, pos

def get_priority(symbol, arity): 
  try: return priority_dict[arity-1][symbol]
  except: return priority_dict[arity-1]['+']

priority = builtin.function()(get_priority)


def get_association(symbol, arity): 
  try: return assoc_dict[arity-1][symbol]
  except: return left

association = builtin.function()(get_association)

def get_operation(symbol, arity): 
  try: return assoc_dict[arity-1][symbol]
  except: return left

operation = builtin.function()(get_operation)

@builtin.function()
def set_operator(solver, cont, symbol, arity, prior, assoc, operation):
  op_fsms[arity].add(symbol)
  priority_dict[arity][symbol] = prior
  assoc_dict[arity][symbol] = prior
  func_dict[arity][symbol] = operation
  
@builtin.function()
def remove_operator(symbol, arity):
  op_fsms[arity].remove(symbol)
  try: del priority_dict[arity][symbol]
  except: pass
  try: del assoc_dict[arity][symbol]
  except: pass
  try: del operation_dict[arity][symbol]
  except: pass