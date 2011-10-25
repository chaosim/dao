# -*- coding: utf-8 -*-

'''terminal used for parsing
 solver.parse_state should have an interface similar to parse_state in parser.py.
 Lineparse_state in line_parser.py is comatible with parse_state.'''

from dao.term import deref, unify, Var
from dao import builtin
from dao.builtins.matcher import matcher

@matcher()
def char(solver, cont, argument): 
  argument = deref(argument, solver.env)
  text, pos = solver.parse_state
  if pos==len(text): 
    return
  for _ in unify(argument, text[pos], solver.env):
    solver.parse_state = text, pos+1
    yield cont,  text[pos]
    solver.parse_state = text, pos

@matcher()
def eos(solver, cont):
  '''end of parse_state'''
  if solver.parse_state[1]>=len(solver.parse_state[0]): 
    yield cont,  True
eos = eos()

@matcher()
def lead_chars(solver, cont, chars):
  chars = deref(chars, solver.env)
  if isinstance(chars, str):
    if solver.parse_state[0][solver.parse_state[1]] not in chars.name: return
    yield cont,  True
  elif isinstance(chars, Var): 
    for _ in unify(chars, char, solver.env): yield cont,  True
  else: throw_type_error('Var or String', chars)

@matcher()
def not_lead_chars(solver, cont, chars):
  chars = deref(chars, solver.env)
  assert isinstance(chars, str)
  if solver.parse_state[0][solver.parse_state[1]] in chars.name: return
  yield cont,  True

@matcher()
def follow_chars(solver, cont, chars):
  chars = chars.deref(solver.env)
  assert isinstance(chars, String)
  if not solver.parse_state.eos() and solver.parse_state.next() not in chars.name: 
    return
  yield cont,  True

@matcher()
def not_follow_chars(solver, cont, chars):
  chars = chars.deref(solver.env)
  assert isinstance(chars, String)
  if not solver.parse_state.eos() and solver.parse_state.next() in chars.name: 
    return
  solver.value = True  

def lead_string(solver, cont, strArgument):
  strArgument = strArgument.deref(solver.env)
  assert isinstance(strArgument, String)
  if not solver.parse_state.parsed().endwith(strArgument.name): raise UnifyFail
  solver.value = True

@matcher()
def not_lead_string(solver, cont, string):
  string = string.deref(solver.env)
  if solver.parse_state.parsed().endwith(string.name): return
  solver.value = True  

def follow_string(solver, cont, strArgument):
  strArgument = strArgument.deref(solver.env)
  assert isinstance(strArgument, String)
  if not solver.parse_state.left().startswith(strArgument.name): raise UnifyFail
  solver.value = True

@matcher()
def not_follow_string(solver, cont, string):
  string = string.deref(solver.env)
  if solver.parse_state.left().startswith(string.name): return
  solver.value = True  

def char_on_test(test, name=''):
  def func(solver, cont, arg0):
    #assert isinstance(arg0, Var) and arg0.free(solver.env)
    text, pos = solver.parse_state
    c = text[pos]
    if not test(c): return
    for _ in unify(arg0, c, solver.env):
      solver.parse_state = text, pos+1
      yield cont,  True
      solver.parse_state = text, pos
  if name=='': name = test.__name
  return matcher(name)(func)

def char_between(lower, upper): return char_on_test(lambda char: lower<=char<=upper, "charbetween <%s-%s>"%(lower, upper))  
def char_in(string, repr_string=''): return char_on_test(lambda char: char in string, repr_string or 'char_in '+string)      
digit = char_between('0', '9')
_1_9 = char_between('1', '9')
lowcase = char_between('a', 'z')
uppercase = char_between('A', 'Z')
letter = char_on_test(lambda char: ('a'<=char<='z') or ('A'<=char<='Z'), 'letter')
uletter = char_on_test(lambda char: char=='_' or ('a'<=char<='z') or ('A'<=char<='Z'), 
      '_letter')
_letter_digit_test = (lambda char: char=='_' or 
                    ('0'<=char<='9') or ('a'<=char<='z') or ('A'<=char<='Z'))
u_letter_digit = char_on_test(_letter_digit_test, '_letterdigitChar')
space_string = ' \t\r\n'
space = char_in(space_string, repr_string='spacesChar')

def string_on_test(test, name='', onceMore=True):
  def func(solver, cont,  arg):
    #assert isinstance(arg, Var) and arg.free(solver.env)
    text, pos = solver.parse_state
    string = ''
    i = 0
    while pos+i<len(text):         
      char = text[pos+i]
      if not test(char): break
      string += char
      i += 1
    if onceMore and string=='': return
    for _ in unify(arg, string, solver.env):
      solver.parse_state = text, pos+i
      yield cont,  True
      solver.parse_state = text, pos
  if name=='': name = test.__name
  return matcher(name)(func)
def string_between(lower, upper, once_more=True):
  if not once_more: name = "s<%s-%s>"%(lower, upper)
  else: name = "s<%s-%s>+"%(lower, upper)
  return string_on_test(lambda char: lower<=char<=upper, name, once_more)  
def string_in(string, once_more=True, repr_string=''): 
  return string_on_test(lambda char: char in string, repr_string or 's'+string, once_more)  
digits = string_between('0', '9')
digits0 = string_between('0', '9', once_more=False)
lowcaseString = string_between('a', 'z')
uppercaseString = string_between('A', 'Z')
uLetterdigitString = string_on_test(_letter_digit_test, '_letterdigitString')
uLetterdigitString0 = string_on_test(_letter_digit_test, '_letterdigitTestString0', False)
spaces0 = string_in(space_string, once_more=False, repr_string='spaces0')
spaces = string_in(space_string, repr_string='spaces')

def quote_string(quote, name):
  def func(solver, cont,  arg0):
    #assert isinstance(arg0, Var) and arg0.free(solver.env)
    text, pos = solver.parse_state
    if pos>=len(text): return
    if text[pos]!=quote: return
    p = pos+1
    while p<len(text): 
      char = text[p]
      p += 1
      if char=='\\': p += 1
      elif char==quote: 
        string = text[pos+1:p-1]
        break
    else: return
    for _ in unify(arg0, string, solver.env): 
      solver.parse_state = text, p
      yield cont,  True
      solver.parse_state = text, pos
  return matcher(name)(func)
dqstring = quote_string('"', 'doublequotestring')
sqstring = quote_string("'", 'singlequotestring')

@matcher()
def number(solver, cont,  arg0): 
  text, pos = solver.parse_state
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
    solver.parse_state = text, p
    yield cont,  True
    solver.parse_state = text, pos

@matcher()
def literal(solver, cont,  arg0):
  arg0 = deref(arg0, solver.env)
  assert isinstance(arg0, str)
  text, pos = solver.parse_state
  p = pos
  for char in arg0:
    if p>=len(text): return
    if char!=text[p]: return
    p += 1
  solver.parse_state = text, p
  yield cont,  True
  solver.parse_state = text, pos
