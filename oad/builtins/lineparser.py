# -*- coding: utf-8 -*-

##from oad.term import Integer, True
from oad import builtin
from oad.builtins.parser import Stream

# builtins used for parsing aware of line, 
# can be helpful for error report, parsing debugging, etc.

#characters in separator should not occur in other places, 
# except as a whole symbol to stand for a line change.
class LineStream(Stream):
  def __init__(self, text, position=0, column=0, row=0, seperator='\n'):
    Stream.__init__(self, text, position)
    self.row, self.column = column, row
    self.seperator = seperator
    self._basePosition = 0
    self._len_sep = len(self.seperator)
    
  def new(self, position):
    stream = LineStream(self.text, position, self.row, self.column, self.seperator)
    stream._basePosition = self._basePosition
    stream._len_sep = self._len_sep
    return stream
    
  def getRowColumn(self, position):
    if position==0: return 0, 0
    if position==self._basePosition: return self.row, self.column
    
    pos, row, col = self._basePosition, self.row, self.column
    sep = self.seperator
    len_sep = self._len_sep    
    if len_sep==1: # separator is single char
      if pos<position:
        while pos<position:
          if self.text[pos]==sep: row += 1; col = 0
          else: col += 1
          pos += 1
      else:
        meetnewline = False
        while pos>position:
          pos -= 1
          if self.text[pos]==sep: 
            row -= 1; meetnewline = True
            if pos!=position: pos -= 1; 
          else: col -= 1
        if meetnewline:
          p = pos; col = 0
          while p>=0:
            if self.text[p-1]==sep: break
            if p!=0: col += 1
            p -= 1; 
    else: # separator is multiple chars
      if pos<position:
        while pos<position:
          if self.text[pos]==sep[0]: 
            if pos+len_sep-1>=position: break
            row += 1; col = 0; pos += len_sep
          else: col += 1; pos += 1
      else:
        meetnewline = False
        while pos>position:
          pos -= 1
          if self.text[pos]==sep[len_sep-1]: 
            row -= 1; pos -= len_sep; meet_newline = True
          else: col -= 1
        if meet_newline:
          p = pos; col = 0
          while p>=0:
            if self.text[p]==sep[len_sep-1]: break 
            if p!=0: col += 1; 
            p -= 1; 
    self._basePosition = pos
    self.row, self.column = row, col
    return row, col
  
if __name__=='__main__':
  from nose.tools import eq_
  stream = LineStream('aaaaa#@$aaaa#@$aaaa#@$aaaa', seperator='#@$')
  eq_(stream.getRowColumen(0), (0, 0))
  eq_(stream.getRowColumen(4), (0, 4))
  eq_(stream.getRowColumen(10), (1, 2))
  eq_(stream.getRowColumen(12), (1, 4))
  eq_(stream.getRowColumen(13), (1, 4))
  eq_(stream.getRowColumen(14), (1, 4))
  eq_(stream.getRowColumen(15), (2, 0))
  eq_(stream.getRowColumen(19), (2, 4))
  eq_(stream.getRowColumen(20), (2, 4))
  eq_(stream.getRowColumen(21), (2, 4))
  eq_(stream.getRowColumen(22), (3, 0))
  eq_(stream.getRowColumen(14), (1, 4))
  eq_(stream.getRowColumen(4), (0, 4))
  eq_(stream.getRowColumen(22), (3, 0))
  eq_(stream.getRowColumen(1), (0, 1))
  stream = LineStream('21465756758#6758566998797#56756577778#876867676', seperator='#')
  eq_(stream.getRowColumen(0), (0, 0))
  eq_(stream.getRowColumen(4), (0, 4))
  eq_(stream.getRowColumen(10), (0, 10))
  eq_(stream.getRowColumen(11), (0, 11))
  eq_(stream.getRowColumen(12), (1, 0))
  eq_(stream.getRowColumen(14), (1, 2))
  eq_(stream.getRowColumen(26), (2, 0))
  eq_(stream.getRowColumen(29), (2, 3))
  eq_(stream.getRowColumen(12), (1, 0))
  eq_(stream.getRowColumen(14), (1, 2))
  eq_(stream.getRowColumen(12), (1, 0))
  eq_(stream.getRowColumen(11), (0, 11))
  eq_(stream.getRowColumen(5), (0, 5))

@builtin.macro('line-parse')
def parse(evaluator, pred, atom):
  evaluator.stream = LineStream(atom.name) 
  pred = pred.deref(evaluator.env)
  pred.scont(evaluator)

@builtin.function2('line-settext')
def settext(evaluator, atom): 
  evaluator.stream = LineStream(atom.name, 0)
  evaluator.value = True

@builtin.function2()
def row(evaluator, position=None):
  if position is None: position = evaluator.stream.position
  else: position = position.val
  return Integer(evaluator.stream.getRowColumn(position)[0])

@builtin.function2()
def column(evaluator, position=None):
  if position is None: position = evaluator.stream.position
  else: position = position.val
  return Integer(evaluator.stream.getRowColumn(position)[1])
