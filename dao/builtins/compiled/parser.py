from dao import builtin
from dao.solve import mycont
from dao.term import unify
from dao.builtins.matcher import matcher

from dao.compiler import get_parse_state as get_current_parse_state, set_parse_state  as set_current_parse_state

# set and manipulate parse_state for parsing 

def set_parse_state(parse_state):
  global current_parse_state
  old_parse_state = get_parse_state()
  set_parse_state(parse_state)  
  yield solver.parse_state
  current_parse_state = old_parse_state
  
def set_sequence(solver, cont, text):
  parse_state = solver.parse_state
  sign_state2cont = solver.sign_state2cont
  sign_state2results = solver.sign_state2results
  solver.sign_state2cont = {}
  solver.sign_state2results = {}
  solver.parse_state = text, 0  
  yield cont, solver.parse_state
  solver.parse_state = parse_state
  solver.sign_state2cont = sign_state2cont
  solver.sign_state2results = sign_state2results

set_text = set_sequence

@builtin.nomemo
@builtin.macro()
def parse(solver, cont, predicate, parse_state):
  old_parse_state = solver.parse_state
  sign_state2cont = solver.sign_state2cont
  sign_state2results = solver.sign_state2results
  solver.sign_state2cont = {}
  solver.sign_state2results = {}
  solver.parse_state = parse_state
  @mycont(cont)
  def parser_cont(value, solver):
    solver.parse_state = old_parse_state
    solver.sign_state2cont = sign_state2cont
    solver.sign_state2results = sign_state2results
    yield cont, value 
  yield solver.cont(predicate, parser_cont), parse_state
  solver.parse_state = old_parse_state
  solver.sign_state2cont = sign_state2cont
  solver.sign_state2results = sign_state2results

def parse_sequence(pred, sequence):
  old_parse_state = get_current_parse_state()
  set_current_parse_state((sequence, 0)) #sequence, start position
  for x in pred[0](*pred[1:]):
    set_current_parse_state(old_parse_state)
    yield x
  yield get_current_parse_state()

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
def left(solver, cont, length=None):
  text, pos = solver.parse_state
  yield cont, text[pos:pos+length if length is not None else None]

@builtin.nomemo
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