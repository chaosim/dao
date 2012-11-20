from dao.command import special, Command, SpecialCall
import dao.interlang as il
from dao.compilebase import CompileTypeError

from dao.interlang import TRUE, FALSE, NONE

v0, fc0 = il.Var('v'), il.Var('fc')

# set and manipulate parse_state for parsing 

@special
def parse_state(compiler, cont):
  return cont(il.parse_state)

@special
def set_parse_state(compiler, cont, parse_state):
  old_parse_state = compiler.new_var(il.Var('old_parse_state'))
  return il.begin(il.Assign(old_parse_state, il.parse_state),
                  il.SetParseState(parse_state),
                  il.append_fail_cont(compiler, il.SetParseState(old_parse_state)),
                  cont(TRUE))
@special
def settext(compiler, cont, text):
  old_parse_state = compiler.new_var(il.Var('old_parse_state'))
  return il.begin(il.Assign(old_parse_state, il.parse_state),
                  il.SetParseState(il.Tuple(text, il.Integer(0))),
                  il.append_fail_cont(compiler, il.SetParseState(old_parse_state)),
                  cont(TRUE))

'''
@builtin.nomemo
@matcher()
def set_sequence(solver, text):
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
def parse(solver, predicate, parse_state):
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

@builtin.nomemo
@builtin.macro()
def parse_sequence(solver, pred, sequence):
  parse_state = solver.parse_state
  sign_state2cont = solver.sign_state2cont
  sign_state2results = solver.sign_state2results
  solver.sign_state2cont = {}
  solver.sign_state2results = {}
  solver.parse_state = sequence, 0 #sequence, start position
  cont = solver.scont
  @mycont(cont)
  def parser_cont(value, solver):
    solver.parse_state = parse_state
    solver.sign_state2cont = sign_state2cont
    solver.sign_state2results = sign_state2results
    solver.scont = cont
    return value 
  old_fcont = solver.fcont
  @mycont(old_fcont)
  def fcont(value, solver):
    solver.parse_state = parse_state
    solver.sign_state2cont = sign_state2cont
    solver.sign_state2results = sign_state2results
    solver.scont = old_fcont
  solver.fcont = fcont
  solver.scont = solver.cont(pred, parser_cont)
  return solver.parse_state

parse_text = parse_sequence
  
@matcher()
def get_parse_state(solver):
  yield cont, solver.parse_state

@matcher()
def unify_parse_state(solver, parse_state):
  for _ in unify(parse_state, solver.parse_state, solver.env):
    yield cont, solver.parse_state

@matcher()
def get_parse_sequence(solver):
  yield cont, solver.parse_state[0]

get_parse_text = get_parse_sequence

@matcher()
def unify_parse_sequence(solver, sequence):
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
def step(solver, n=1): # return current element before step
  text, pos = solver.parse_state
  solver.parse_state = text, pos+n
  yield cont, text[pos]
  solver.parse_state = text, pos

@matcher()
def skip(solver, n=1): # return element after skip
  text, pos = solver.parse_state
  solver.parse_state = text, pos+n
  if pos+n<len(text): yield cont, text[pos+n]
  else: yield cont, ''
  solver.parse_state = text, pos

@matcher()
def left(solver, length=None):
  text, pos = solver.parse_state
  yield cont, text[pos:pos+length if length is not None else None]

@builtin.nomemo
@matcher()
def next_element(solver): 
  text, pos = solver.parse_state
  yield cont, text[pos]
next_char = next_element

@matcher()
def position(solver): 
  yield cont, solver.parse_state[1]

@matcher()
def sub_sequence(solver, start, end): 
  yield cont, solver.parse_state[0][start:end]

subtext = sub_sequence

@matcher()
def goto(solver, position):
  text, pos = solver.parse_state
  solver.parse_state = text, position
  yield cont, text[position:]
  solver.parse_state = text, pos
'''