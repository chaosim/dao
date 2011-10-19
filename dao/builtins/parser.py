from dao import builtin
from dao.solve import mycont
from dao.builtins.matcher import matcher

# set and manipulate stream for parsing 

# Solver.stream: tuple(text, position)

@builtin.macro()
def parse(solver, cont, pred, text):
  stream = solver.stream
  solver.stream = text, 0 #text, start position
  @mycont(cont)
  def parser_cont(value, solver):
    yield cont, value 
  yield solver.cont(pred, parser_cont), solver.stream
  solver.stream = stream

@matcher()
def settext(solver, cont, text):
  stream = solver.stream
  solver.stream = text, 0  #text, start position
  yield cont, solver.stream
  solver.stream = stream
  
# Theses primitive can be used with Stream or compatible class with same interface.
# LineStream in line_parser.py is an sample.

@matcher()
def step(solver, cont, size=1): # return current char before step
  text, pos = solver.stream
  solver.stream = text, pos+size
  yield cont, text[pos]
  solver.stream = text, pos

@matcher()
def skip(solver, cont, size=1): # return char after skip
  text, pos = solver.stream
  solver.stream = text, pos+size
  if pos+size<len(text): yield cont, text[pos+size]
  else: yield cont, ''
  solver.stream = text, pos

@matcher()
def left(solver, cont):
  text, pos = solver.stream
  yield cont, text[pos:]

@matcher()
def nextchar(solver, cont): 
  text, pos = solver.stream
  yield cont, text[pos]

@matcher()
def position(solver, cont): 
  yield cont, solver.stream[1]

@matcher()
def subtext(solver, cont, start, end): 
  yield cont, solver.stream[0][start:end]

@matcher()
def goto(solver, cont, position):
  text, pos = solver.stream
  solver.stream = text, position
  yield cont, text[position:]
  solver.stream = text, pos