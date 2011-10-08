from oad import builtin
from oad.solve import mycont

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

@builtin.function2()
def settext(solver, cont, text):
  stream = solver.stream
  solver.stream = text, 0  #text, start position
  yield cont, solver.stream
  solver.stream = stream
  
# Theses primitive can be used with Stream or compatible class with same interface.
# LineStream in lineparser.py is an sample.

@builtin.function2()
def step(solver, cont, size=1): # return current char before step
  text, pos = solver.stream
  solver.stream = text, pos+size
  yield cont, text[pos]
  solver.stream = text, pos

@builtin.function2()
def skip(solver, cont, size=1): # return char after skip
  text, pos = solver.stream
  solver.stream = text, pos+size
  if pos+size<len(text): yield cont, text[pos+size]
  else: yield cont, ''
  solver.stream = text, pos

@builtin.function2()
def left(solver, cont):
  text, pos = solver.stream
  yield cont, text[pos:]

@builtin.function2()
def next(solver, cont): 
  text, pos = solver.stream
  yield cont, text[pos]

@builtin.function2()
def position(solver, cont): 
  yield cont, solver.stream[1]

@builtin.function2()
def subtext(solver, cont, start, end): 
  yield cont, solver.stream[0][start:end]

@builtin.function2()
def goto(solver, cont, position):
  text, pos = solver.stream
  solver.stream = text, position
  yield cont, text[position:]
  solver.stream = text, pos