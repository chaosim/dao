# -*- coding: utf-8 -*-

'''terminal used for parsing
 evaluator.stream should have an interface similar to Stream in parser.py.
 LineStream in lineparser.py is comatible with Stream.'''

##from oad.term import atom, Atom, String, Symbol, Var, Integer, SUCCESS #, var 
from oad import builtin

@builtin.macro()
def char(evaluator, argument): 
  argument = argument.deref(evaluator.trail)
  text, pos = evaluator.stream
  if pos==len(text): raise UnifyFail()
  argument.unify(atom(text[pos]), evaluator.trail)
  evaluator.stream = evaluator.stream.new(pos+1)
  evaluator.value = SUCCESS

@builtin.macro()
def eof(evaluator):
  if not evaluator.stream.eof(): raise UnifyFail()
  evaluator.value = SUCCESS

@builtin.macro()
def followChars(evaluator, chars):
  chars = chars.deref(evaluator.trail)
  if isinstance(chars, Atom):
    if evaluator.stream.last() not in chars.name: raise UnifyFail()
  elif isinstance(chars, Var): chars.unify(char, evaluator.trail)
  else: throw_type_error('Var or Atom', chars)
  evaluator.value = SUCCESS  

@builtin.macro()
def notFollowChars(evaluator, chars):
  chars = chars.deref(evaluator.trail)
  assert isinstance(chars, Atom)
  if isinstance(chars, Atom):
    if evaluator.stream.last() in chars.name: raise UnifyFail()
  evaluator.value = SUCCESS

@builtin.macro()
def followByChars(evaluator, chars):
  chars = chars.deref(evaluator.trail)
  assert isinstance(chars, Atom)
  if not evaluator.stream.eof() and evaluator.stream.next() not in chars.name: 
    raise UnifyFail()
  evaluator.value = SUCCESS  

@builtin.macro()
def notFollowByChars(evaluator, chars):
  chars = chars.deref(evaluator.trail)
  assert isinstance(chars, Atom)
  if not evaluator.stream.eof() and evaluator.stream.next() in chars.name: 
    raise UnifyFail()
  evaluator.value = SUCCESS  

def followString(evaluator, strArgument):
  strArgument = strArgument.deref(evaluator.trail)
  assert isinstance(strArgument, Atom)
  if not evaluator.stream.parsed().endwith(strArgument.name): raise UnifyFail
  evaluator.value = SUCCESS

@builtin.macro()
def notFollowString(evaluator, string):
  string = string.deref(evaluator.trail)
  if evaluator.stream.parsed().endwith(string.name): raise UnifyFail()
  evaluator.value = SUCCESS  

def followByString(evaluator, strArgument):
  strArgument = strArgument.deref(evaluator.trail)
  assert isinstance(strArgument, Atom)
  if not evaluator.stream.left().startswith(strArgument.name): raise UnifyFail
  evaluator.value = SUCCESS

@builtin.macro()
def notFollowByString(evaluator, string):
  string = string.deref(evaluator.trail)
  if evaluator.stream.left().startswith(string.name): raise UnifyFail()
  evaluator.value = SUCCESS  

@builtin.macro()
def epsilon(evaluator): evaluator.value = SUCCESS 

def charOnTest(test, name=''):
  def func(evaluator, arg0):
    #assert isinstance(arg0, Var) and arg0.free(evaluator.trail)
    text, pos = evaluator.stream
    c = text[pos]
    if not test(c): raise UnifyFail()
    arg0.unify(atom(c), evaluator.trail)
    evaluator.stream = evaluator.stream.new(pos+1)
    evaluator.value = SUCCESS
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
  def func(evaluator, arg):
    #assert isinstance(arg, Var) and arg.free(evaluator.trail)
    text, pos = evaluator.stream
    string = ''
    while pos<len(text):         
      char = text[pos]
      if not test(char): break
      string += char
      pos += 1
    if onceMore and string=='': raise UnifyFail()
    arg.unify(atom(string), evaluator.trail)
    evaluator.stream = evaluator.stream.new(pos)
    evaluator.value = SUCCESS
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
  def func(evaluator, arg0):
    #assert isinstance(arg0, Var) and arg0.free(evaluator.trail)
    text, pos = evaluator.stream
    if pos>=len(text): raise UnifyFail()
    if text[pos]!=quote: raise UnifyFail()
    pos += 1
    p = pos
    while p<len(text): 
      char = text[p]
      p += 1
      if char=='\\': p += 1
      elif char==quote: 
        string = text[pos:p-1]
        break
    else: raise UnifyFail()
    arg0.unify(String(string), evaluator.trail)
    evaluator.stream = evaluator.stream.new(p)
    evaluator.value = SUCCESS
  return builtin.macro(name)(func)
dqstring = quotestring('"', 'doublequotestring')
sqstring = quotestring("'", 'singlequotestring')

@builtin.macro()
def number(evaluator, arg0): 
  #assert isinstance(arg0, Var) and arg0.free(evaluator.trail)
  text, pos = evaluator.stream
  length = len(text)
  if pos>=length: raise UnifyFail()
  if not '0'<=text[pos]<='9': raise UnifyFail()
  p = pos
  while p<length: 
    char = text[p]
    if not '0'<=char<='9': break
    p += 1
  val = eval(text[pos:p])
  arg0.unify(Integer(val), evaluator.trail)
  evaluator.stream = evaluator.stream.new(p)
  evaluator.value = SUCCESS

@builtin.macro()
def symbol(evaluator, arg0):
  #assert isinstance(arg0, Var) and arg0.free(evaluator.trail)
  from oad.eval import _specialforms
  SYMBOL_FORBID_CHARS = '\'", \r\n\t[]{}()`'
  text, pos = evaluator.stream
  if pos>=len(text): raise UnifyFail()
  char = text[pos]
  if char in SYMBOL_FORBID_CHARS or '0'<=char<='9': raise UnifyFail()
  p = pos
  while p<len(text): 
    if text[p] in SYMBOL_FORBID_CHARS: break 
    else: p += 1
  sym = text[pos:p]
  if sym in _specialforms: sym = Symbol(sym)
  else: sym = var(sym)
  arg0.unify(sym, evaluator.trail)
  evaluator.stream = evaluator.stream.new(p)
  evaluator.value = SUCCESS
  
@builtin.macro()
def literal(evaluator, arg0):
  arg0 = arg0.deref(evaluator.trail)
  assert isinstance(arg0, Atom)
  text, pos = evaluator.stream
  for char in arg0.name:
    if pos>=len(text): raise UnifyFail()
    if char!=text[pos]: raise UnifyFail()
    pos += 1
  evaluator.stream = evaluator.stream.new(pos)
  evaluator.value = SUCCESS
