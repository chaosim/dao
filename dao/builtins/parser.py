from dao import builtin
from dao.solve import mycont
from dao.term import unify
from dao.builtins.matcher import matcher

# set and manipulate parse_state for parsing 

@matcher()
def set_parse_state(solver, cont, parse_state):
  old_parse_state = solver.parse_state
  solver.parse_state = parse_state  
  yield cont, solver.parse_state
  solver.parse_state = old_parse_state
  
@matcher()
def set_sequence(solver, cont, text):
  parse_state = solver.parse_state
  solver.parse_state = text, 0  #text, 0
  yield cont, solver.parse_state
  solver.parse_state = parse_state

set_text = set_sequence

@builtin.macro()
def parse(solver, cont, predicate, parse_state):
  old_parse_state = solver.parse_state
  solver.parse_state = parse_state
  @mycont(cont)
  def parser_cont(value, solver):
    solver.parse_state = old_parse_state
    yield cont, value 
  yield solver.cont(predicate, parser_cont), parse_state
  solver.parse_state = old_parse_state

@builtin.macro()
def parse_sequence(solver, cont, pred, sequence):
  parse_state = solver.parse_state
  solver.parse_state = sequence, 0 #sequence, start position
  @mycont(cont)
  def parser_cont(value, solver):
    yield cont, value 
  yield solver.cont(pred, parser_cont), solver.parse_state
  solver.parse_state = parse_state

parse_text = parse_sequence
  
@matcher()
def get_parse_state(solver, cont):
  yield cont, solver.parse_state

@matcher()
def unify_parse_state(solver, cont, parse_state):
  for _ in unify(parse_state, solver.parse_state, solver.env):
    yield cont, solver.parse_state

@matcher()
def get_parse_sequence(solver, cont):
  yield cont, solver.parse_state[0]

get_parse_text = get_parse_sequence

@matcher()
def unify_parse_sequence(solver, cont, sequence):
  for _ in unify(sequence, solver.parse_state[0], solver.env):
    yield cont, solver.parse_state[0]
unify_parse_text = unify_parse_sequence

def eoi_(parse_state):
  return parse_state[1]>=len(parse_state[0])

def boi_(parse_state):
  return parse_state[1]==0

def next_char_(parse_state):
  assert not eoi_(parse_state), 'reached end of input.'
  return parse_state[0][parse_state[1]]

def last_char_(parse_state):
  assert not boi_(parse_state), 'reached begin of input.'
  return parse_state[0][parse_state[1]-1]

def left_(parse_state):
  return parse_state[0][parse_state[1]:]

def parsed_(parse_state):
  return parse_state[0][:parse_state[1]]

def pos_(parse_state):
  return parse_state[1]

@matcher()
def step(solver, cont, n=1): # return current element before step
  text, pos = solver.parse_state
  solver.parse_state = text, pos+n
  yield cont, text[pos]
  solver.parse_state = text, pos

@matcher()
def skip(solver, cont, n=1): # return element after skip
  text, pos = solver.parse_state
  solver.parse_state = text, pos+n
  if pos+n<len(text): yield cont, text[pos+n]
  else: yield cont, ''
  solver.parse_state = text, pos

@matcher()
def left(solver, cont):
  text, pos = solver.parse_state
  yield cont, text[pos:]

@matcher()
def next_element(solver, cont): 
  text, pos = solver.parse_state
  yield cont, text[pos]
next_char = next_element

@matcher()
def position(solver, cont): 
  yield cont, solver.parse_state[1]

@matcher()
def sub_sequence(solver, cont, start, end): 
  yield cont, solver.parse_state[0][start:end]

subtext = sub_sequence

@matcher()
def goto(solver, cont, position):
  text, pos = solver.parse_state
  solver.parse_state = text, position
  yield cont, text[position:]
  solver.parse_state = text, pos