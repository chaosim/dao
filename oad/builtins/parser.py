##from oad.term import SUCCESS, atom, Integer
from oad.error import UnifyFail
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
def parse(evaluator, pred, atom):
  evaluator.stream = Stream(atom.name, 0) #text, start position
  pred = pred.deref(evaluator.trail)
  pred.scont(evaluator)

@builtin.function2()
def settext(evaluator, atom): 
  evaluator.stream = Stream(atom.name, 0) #text, start position
  evaluator.value = SUCCESS
  
# Theses primitive can be used with Stream or compitble class with same interface.
# LineStream in lineparser.py is an sample.

@builtin.function2()
def step(evaluator, size=1): # return current char before step
  text, pos = evaluator.stream
  if isinstance(size, Integer): size = size.val
  evaluator.stream = evaluator.stream.new(pos+size)
  return atom(text[pos])

@builtin.function2()
def skip(evaluator, size=1): # return char after skip
  text, pos = evaluator.stream
  if isinstance(size, Integer): size = size.val
  evaluator.stream = evaluator.stream.new(pos+size)
  return atom(text[pos+size])

@builtin.function2()
def left(evaluator):
  return atom(evaluator.stream.left())

@builtin.function2()
def next(evaluator): 
  text, pos = evaluator.stream
  return atom(evaluator.stream.next())

@builtin.function2()
def position(evaluator): return Integer(evaluator.stream.position)

@builtin.function2()
def subtext(evaluator, start, end): 
  return atom(evaluator.stream.text[start.val:end.val])

@builtin.function2()
def goto(evaluator, position): 
  evaluator.stream = evaluator.stream.new(position.val)
