##from oad.term import True, atom, Integer
from oad import builtin

# set and manipulate stream for parsing 

class Stream:
  def __init__(self, text, position=0):
    self.text, self.position = text, position
    
  def __iter__(self): return iter((self.text, self.position))
  def new(self, position): return Stream(self.text, position)
  def step(self, size=1): 
    return self.text[self.position], self.new(self.position+size)
  def next(self): return self.text[self.position]
  def last(self): return self.text[self.position-1]
  def parsed(self): return self.text[:self.position]
  def left(self): return self.text[self.position:]
  def eof(self): return self.position==len(self.text)

@builtin.macro()
def parse(solver, pred, atom):
  solver.stream = Stream(atom.name, 0) #text, start position
  pred = pred.deref(solver.env)
  pred.scont(solver)

@builtin.function2()
def settext(solver, atom): 
  solver.stream = Stream(atom.name, 0) #text, start position
  solver.value = True
  
# Theses primitive can be used with Stream or compitble class with same interface.
# LineStream in lineparser.py is an sample.

@builtin.function2()
def step(solver, size=1): # return current char before step
  text, pos = solver.stream
  if isinstance(size, Integer): size = size.val
  solver.stream = solver.stream.new(pos+size)
  return atom(text[pos])

@builtin.function2()
def skip(solver, size=1): # return char after skip
  text, pos = solver.stream
  if isinstance(size, Integer): size = size.val
  solver.stream = solver.stream.new(pos+size)
  return atom(text[pos+size])

@builtin.function2()
def left(solver):
  return atom(solver.stream.left())

@builtin.function2()
def next(solver): 
  text, pos = solver.stream
  return atom(solver.stream.next())

@builtin.function2()
def position(solver): return Integer(solver.stream.position)

@builtin.function2()
def subtext(solver, start, end): 
  return atom(solver.stream.text[start.val:end.val])

@builtin.function2()
def goto(solver, position): 
  solver.stream = solver.stream.new(position.val)
