# -*- coding: utf-8 -*-

'''terminal used for parsing
 solver.stream should have an interface similar to Stream in parser.py.
 LineStream in lineparser.py is comatible with Stream.'''

from oad.term import deref, unify, Var
from oad import builtin

@builtin.macro()
def char(solver, cont, argument): 
  argument = deref(argument, solver.env)
  text, pos = solver.stream
  if pos==len(text): return
  for _ in unify(argument, text[pos], solver.env):
    solver.stream = text, pos+1
    yield cont,  text[pos]

@builtin.macro()
def eof(solver, cont):
  if solver.stream[1]>=len(solver.stream[0]): yield cont,  True
  
eof = eof()

@builtin.macro()
def followChars(solver, cont, chars):
  chars = deref(chars, solver.env)
  if isinstance(chars, str):
    if solver.stream[0][solver.stream[1]] not in chars.name: return
    yield cont,  True
  elif isinstance(chars, Var): 
    for _ in unify(chars, char, solver.env): yield cont,  True
  else: throw_type_error('Var or Atom', chars)

@builtin.macro()
def notFollowChars(solver, cont, chars):
  chars = deref(chars, solver.env)
  assert isinstance(chars, str)
  if solver.stream[0][solver.stream[1]] in chars.name: return
  yield cont,  True

@builtin.macro()
def followByChars(solver, cont, chars):
  chars = chars.deref(solver.env)
  assert isinstance(chars, Atom)
  if not solver.stream.eof() and solver.stream.next() not in chars.name: 
    return
  yield cont,  True

@builtin.macro()
def notFollowByChars(solver, cont, chars):
  chars = chars.deref(solver.env)
  assert isinstance(chars, Atom)
  if not solver.stream.eof() and solver.stream.next() in chars.name: 
    return
  solver.value = True  

def followString(solver, cont, strArgument):
  strArgument = strArgument.deref(solver.env)
  assert isinstance(strArgument, Atom)
  if not solver.stream.parsed().endwith(strArgument.name): raise UnifyFail
  solver.value = True

@builtin.macro()
def notFollowString(solver, cont, string):
  string = string.deref(solver.env)
  if solver.stream.parsed().endwith(string.name): return
  solver.value = True  

def followByString(solver, cont, strArgument):
  strArgument = strArgument.deref(solver.env)
  assert isinstance(strArgument, Atom)
  if not solver.stream.left().startswith(strArgument.name): raise UnifyFail
  solver.value = True

@builtin.macro()
def notFollowByString(solver, cont, string):
  string = string.deref(solver.env)
  if solver.stream.left().startswith(string.name): return
  solver.value = True  

@builtin.macro()
def nullword(solver, cont): 
  yield cont,  True
null = nullword = nullword()

def charOnTest(test, name=''):
  def func(solver, cont, arg0):
    #assert isinstance(arg0, Var) and arg0.free(solver.env)
    text, pos = solver.stream
    c = text[pos]
    if not test(c): return
    for _ in unify(arg0, c, solver.env):
      solver.stream = text, pos+1
      yield cont,  True
  if name=='': name = test.__name
  return builtin.macro(name)(func)

def charBetween(lower, upper): return charOnTest(lambda char: lower<=char<=upper, "charbetween <%s-%s>"%(lower, upper))  
def charIn(string, reprString=''): return charOnTest(lambda char: char in string, reprString or 'charin '+string)      
digit = charBetween('0', '9')
one_9 = charBetween('1', '9')
lowcase = charBetween('a', 'z')
uppercase = charBetween('A', 'Z')
letter = charOnTest(lambda char: ('a'<=char<='z') or ('A'<=char<='Z'), 'letter')
uletter = charOnTest(lambda char: char=='_' or ('a'<=char<='z') or ('A'<=char<='Z'), 
      '_letter')
_letterdigitTest = (lambda char: char=='_' or 
                    ('0'<=char<='9') or ('a'<=char<='z') or ('A'<=char<='Z'))
uletterdigit = charOnTest(_letterdigitTest, '_letterdigitChar')
spaceString = ' \t\r\n'
space = charIn(spaceString, reprString='spacesChar')

def stringOnTest(test, name='', onceMore=True):
  def func(solver, cont,  arg):
    #assert isinstance(arg, Var) and arg.free(solver.env)
    text, pos = solver.stream
    string = ''
    while pos<len(text):         
      char = text[pos]
      if not test(char): break
      string += char
      pos += 1
    if onceMore and string=='': return
    for _ in unify(arg, string, solver.env):
      solver.stream = text, pos
      yield cont,  True
  if name=='': name = test.__name
  return builtin.macro(name)(func)
def stringBetween(lower, upper, onceMore=True):
  if not onceMore: name = "s<%s-%s>"%(lower, upper)
  else: name = "s<%s-%s>+"%(lower, upper)
  return stringOnTest(lambda char: lower<=char<=upper, name, onceMore)  
def stringIn(string, onceMore=True, reprString=''): 
  return stringOnTest(lambda char: char in string, reprString or 's'+string, onceMore)  
digits = stringBetween('0', '9')
digits0 = stringBetween('0', '9', onceMore=False)
lowcaseString = stringBetween('a', 'z')
uppercaseString = stringBetween('A', 'Z')
uLetterdigitString = stringOnTest(_letterdigitTest, '_letterdigitString')
uLetterdigitString0 = stringOnTest(_letterdigitTest, '_letterdigitTestString0', False)
spaces0 = stringIn(spaceString, onceMore=False, reprString='spaces0')
spaces = stringIn(spaceString, reprString='spaces')

def quotestring(quote, name):
  def func(solver, cont,  arg0):
    #assert isinstance(arg0, Var) and arg0.free(solver.env)
    text, pos = solver.stream
    if pos>=len(text): return
    if text[pos]!=quote: return
    pos += 1
    p = pos
    while p<len(text): 
      char = text[p]
      p += 1
      if char=='\\': p += 1
      elif char==quote: 
        string = text[pos:p-1]
        break
    else: return
    for _ in unify(arg0, string, solver.env): 
      solver.stream = text, p
      yield cont,  True
  return builtin.macro(name)(func)
dqstring = quotestring('"', 'doublequotestring')
sqstring = quotestring("'", 'singlequotestring')

@builtin.macro()
def number(solver, cont,  arg0): 
  text, pos = solver.stream
  length = len(text)
  if pos>=length: return
  if not '0'<=text[pos]<='9': return
  p = pos
  while p<length: 
    char = text[p]
    if not '0'<=char<='9': break
    p += 1
  val = eval(text[pos:p])
  for _ in unify(arg0, val, solver.env):
    solver.stream = text, p
    yield cont,  True

@builtin.macro()
def symbol(solver, cont,  arg0):
  from oad.solve import _specialforms
  SYMBOL_FORBID_CHARS = '\'", \r\n\t[]{}()`'
  text, pos = solver.stream
  if pos>=len(text): return
  char = text[pos]
  if char in SYMBOL_FORBID_CHARS or '0'<=char<='9': return
  p = pos
  while p<len(text): 
    if text[p] in SYMBOL_FORBID_CHARS: break 
    else: p += 1
  sym = text[pos:p]
  if sym in _specialforms: sym = Symbol(sym)
  else: sym = var(sym)
  for _ in unify(arg0, sym, solver.env):
    solver.stream = text, pos
    yield cont,  True
  
@builtin.macro()
def literal(solver, cont,  arg0):
  arg0 = deref(arg0, solver.env)
  assert isinstance(arg0, str)
  text, pos = solver.stream
  for char in arg0:
    if pos>=len(text): return
    if char!=text[pos]: return
    pos += 1
  solver.stream = text, pos
  yield cont,  True
