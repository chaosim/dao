# -*- coding: utf-8 -*-

'''terminal used for parsing
 solver.parse_state should have an interface similar to parse_state in parser.py.
 Lineparse_state in line_parser.py is comatible with parse_state.'''

from dao.compiler import get_parse_state, set_parse_state

from dao.term import deref, unify, Var
from dao import builtin
from dao.builtins.matcher import matcher
from dao.builtins.parser import next_char_, left_, parsed_, eoi_, last_char_

from dao.base import unify_after_compile

def char(argument):
  text, pos = get_parse_state()
  if pos==len(text): return
  for _ in unify_after_compile(argument, text[pos]):
    set_parse_state((text, pos+1))
    yield text[pos]
    set_parse_state((text, pos))

@matcher()
def char2(solver, cont, argument): 
  char = deref(argument, solver.env)
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]==char: 
    solver.parse_state = text, pos+1
    yield cont,  char
    solver.parse_state = text, pos

@matcher()
def char3(solver, cont): 
  text, pos = solver.parse_state
  if pos==len(text): return
  solver.parse_state = text, pos+1
  yield cont, text[pos]
  solver.parse_state = text, pos

@matcher()
def ch(solver, cont, char):
  '''one char'''
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]==char: 
    solver.parse_state = text, pos+1
    yield cont,  char
    solver.parse_state = text, pos

@matcher()
def chs0(solver, cont, char):
  '''0 or more char'''
  text, pos = solver.parse_state
  p = pos
  while text[p]==char: p += 1 
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos

@matcher()
def chs(solver, cont, char):
  '''1 or more char'''
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]!=char: return
  p = pos+1
  while text[p]==char: p += 1 
  if text[pos]==char: 
    solver.parse_state = text, p
    yield cont, text[pos:p]
    solver.parse_state = text, pos

@matcher()
def Eoi(solver, cont):
  '''end of parse_state'''
  if solver.parse_state[1]>=len(solver.parse_state[0]): 
    yield cont,  True
eoi = Eoi()

@matcher()
def lead_chars(solver, cont, chars):
  chars = deref(chars, solver.env)
  assert isinstance(chars, str)
  if last_char_(solver.parse_state) not in chars: return
  yield cont,  True

@matcher()
def not_lead_chars(solver, cont, chars):
  chars = deref(chars, solver.env)
  assert isinstance(chars, str)
  if last_char_(solver.parse_state) in chars: return
  yield cont,  True

@matcher()
def follow_chars(solver, cont, chars):
  chars = deref(chars, solver.env)
  if eoi_(solver.parse_state) or\
     next_char_(solver.parse_state) not in chars: 
    return
  yield cont,  True

@matcher()
def follow_char(solver, cont, char):
  char = deref(char, solver.env)
  if eoi_(solver.parse_state) or\
     next_char_(solver.parse_state)!=char: 
    return
  yield cont,  True

@matcher()
def not_follow_chars(solver, cont, chars):
  chars = deref(chars, solver.env)
  if not eoi_(solver.parse_state) and next_char_(solver.parse_state) in chars: 
    return
  yield cont,  True

@matcher()
def not_follow_char(solver, cont, char):
  char = deref(char, solver.env)
  if not eoi_(solver.parse_state) and next_char_(solver.parse_state)==char: 
    return
  yield cont,  True

def lead_string(solver, cont, string):
  strArgument = deref(strArgument, solver.env)
  assert isinstance(strArgument, str)
  if not parsed_(solver.parse_state).endwith(strArgument):return
  yield cont,  True

@matcher()
def not_lead_string(solver, cont, string):
  string = string.deref(solver.env)
  if parsed_(solver.parse_state).endwith(string): return
  solver.value = True  

def follow_string(solver, cont, strArgument):
  strArgument = strArgument.deref(solver.env)
  assert isinstance(strArgument, String)
  if not left_(solver.parse_state).startswith(strArgument.name): raise UnifyFail
  solver.value = True

@matcher()
def not_follow_string(solver, cont, string):
  string = string.deref(solver.env)
  if left_(solver.parse_state).startswith(string.name): return
  solver.value = True  

def char_on_predicate(test, name=''):
  def func(solver, cont, arg0):
    #assert isinstance(arg0, Var) and arg0.free(solver.env)
    text, pos = solver.parse_state
    if pos>=len(text): return
    c = text[pos]
    if not test(c): return
    for _ in unify(arg0, c, solver.env):
      solver.parse_state = text, pos+1
      yield cont,  True
      solver.parse_state = text, pos
  if name=='': name = test.__name
  return matcher(name)(func)

def char_between(lower, upper): return char_on_predicate(lambda char: lower<=char<=upper, "charbetween <%s-%s>"%(lower, upper))  
def char_in(string, repr_string=''): return char_on_predicate(lambda char: char in string, repr_string or 'char_in '+string)      
digit = char_between('0', '9')
_1_9 = char_between('1', '9')
lowcase = char_between('a', 'z')
uppercase = char_between('A', 'Z')
letter = char_on_predicate(lambda char: ('a'<=char<='z') or ('A'<=char<='Z'), 'letter')
uletter = char_on_predicate(lambda char: char=='_' or ('a'<=char<='z') or ('A'<=char<='Z'), 
      '_letter')
_letter_digit_test = (lambda char: char=='_' or 
                    ('0'<=char<='9') or ('a'<=char<='z') or ('A'<=char<='Z'))
u_letter_digit = char_on_predicate(_letter_digit_test, '_letterdigitChar')
space_string = ' \t' #\r\n
whitespace_string = ' \t\r\n'
unify_tabspace = char_in(space_string, repr_string='spacesChar')
unify_whitespace = char_in(whitespace_string, repr_string='spacesChar')

@matcher()
def any_chars_except(solver, cont, except_chars):
  '''any chars until meet except_chars'''
  text, pos = solver.parse_state
  length = len(text)
  p = pos
  while 1:
    if p == length or text[p] in except_chars: 
      solver.parse_state = text, p
      yield cont, text[p]
      solver.parse_state = text, pos
    p += 1

@matcher()
def space(solver, cont):
  '''one space'''
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]==' ': 
    solver.parse_state = text, pos+1
    yield cont, ' '
    solver.parse_state = text, pos
space = space()

@matcher()
def tab(solver, cont):
  '''one tab'''
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]=='\t': 
    solver.parse_state = text, pos+1
    yield cont, '\t'
    solver.parse_state = text, pos
tab = tab()

@matcher()
def tabspace(solver, cont):
  '''one space or tab'''
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]==' ' or text[pos]=='\t': 
    solver.parse_state = text, pos+1
    yield cont, text[pos]
    solver.parse_state = text, pos
tabspace = tabspace()

@matcher()
def whitespace(solver, cont):
  '''one space or tab'''
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos] in ' \t\r\n': 
    solver.parse_state = text, pos+1
    yield cont, text[pos]
    solver.parse_state = text, pos
whitespace = whitespace()

@matcher()
def newline(solver, cont):
  '''one newline'''
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]=='\r' or text[pos]=='\n':
    if text[pos+1]=='\r' or text[pos+1]=='\n' and text[pos+1]!=text[pos]:
      solver.parse_state = text, pos+2
      yield cont,  text[pos:pos+2]
    else:
      solver.parse_state = text, pos+1
      yield cont,  text[pos:pos+1]
    solver.parse_state = text, pos
nl = newline = newline()

def string__on_predicate(test, name='', onceMore=True):
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
  return string__on_predicate(lambda char: lower<=char<=upper, name, once_more)  
def string_in(string, once_more=True, repr_string=''): 
  return string__on_predicate(lambda char: char in string, repr_string or 's'+string, once_more)  
digits = string_between('0', '9')
digits0 = string_between('0', '9', once_more=False)
lowcaseString = string_between('a', 'z')
uppercaseString = string_between('A', 'Z')
uLetterdigitString = string__on_predicate(_letter_digit_test, '_letterdigitString')
uLetterdigitString0 = string__on_predicate(_letter_digit_test, '_letterdigitTestString0', False)
unify_tabspaces0 = string_in(space_string, once_more=False, repr_string='spaces0')
unify_tabspaces = string_in(space_string, repr_string='spaces')
unify_whitespaces0 = string_in(whitespace_string, once_more=False, repr_string='whitespaces0')
unify_whitespaces = string_in(whitespace_string, repr_string='whitespaces')

@matcher()
def spaces0(solver, cont):
  '''0 or more space'''
  text, pos = solver.parse_state
  length = len(text)
  p = pos
  while p<length and text[p]==' ': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
spaces0 = spaces0()

@matcher()
def tabs0(solver, cont):
  '''0 or more tab'''
  text, pos = solver.parse_state
  length = len(text)
  p = pos
  while p<length and text[p]=='\t': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
tabs0 = tabs0()

@matcher()
def _Tabspaces0(solver, cont):
  '''0 or more space or tab'''
  text, pos = solver.parse_state
  length = len(text)
  p = pos
  while p<length and (text[p]==' ' or text[p]=='\t'): p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
tabspaces0 = _Tabspaces0()

@matcher()
def whitespaces0(solver, cont):
  ''' 0 or more space or tab or newline'''
  text, pos = solver.parse_state
  length = len(text)
  p = pos
  while p<length and text[p] in ' \t\r\n': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
whitespaces0 = whitespaces0()

@matcher()
def newlines0(solver, cont):
  ''' 0 or more newline'''
  text, pos = solver.parse_state
  length = len(text)
  p = pos
  while p<length and text[p] in '\r\n': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
newlines0 = newlines0()

@matcher()
def spaces(solver, cont):
  '''1 or more space or tab'''
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]!=' ': return
  p = pos+1
  while text[p]==' ': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
spaces = spaces()

@matcher()
def tabs(solver, cont):
  '''1 or more space or tab'''
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]!='\t': return
  p = pos+1
  while text[p]=='\t': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
tabs = tabs()

@matcher()
def _Tabspaces(solver, cont):
  '''1 or more space or tab'''
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]!=' ' and text[pos]!='\t': return
  p = pos+1
  while text[p]==' ' or text[p]=='\t': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
tabspaces = _Tabspaces()

@matcher()
def pad_tabspaces(solver, cont):
  '''if not leading space, 1 or more space or tab'''
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]!=' ' and text[pos]!='\t': 
    if text[pos-1]!=' ' and text[pos]!='\t': return
    yield cont, text[pos]
    return
  p = pos+1
  while text[p]==' ' or text[p]=='\t': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
pad_tabspaces = pad_tabspaces()

@matcher()
def tabspaces_if_need(solver, cont):
  '''1 or more tabspace not before punctuation '",;:.{}[]()!?\r\n '''
  text, pos = solver.parse_state
  if pos==len(text): 
    yield cont, ''
    return
  if text[pos] not in ' \t': 
    if pos+1==len(text) or text[pos+1] in '\'",;:.{}[]()!?\r\n':
      yield cont, text[pos]
    return
  p = pos+1
  while text[p] in '\r\n': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
tabspaces_if_need = tabspaces_if_need()

@matcher()
def tabspaces_unless(solver, cont, chars):
  '''1 or more tabspace if not before chars, else 0 or more tabspace '''
  chars = deref(chars, solver.env)
  text, pos = solver.parse_state
  if pos==len(text): 
    yield cont, ''
    return
  if text[pos] not in ' \t': 
    if pos+1==len(text) or text[pos+1] in chars:
      yield cont, text[pos]
    return
  p = pos+1
  while text[p] in '\r\n': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
tabspaces_unless = tabspaces_unless()

@matcher()
def whitespaces(solver, cont):
  ''' 1 or more space or tab or newline'''
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos] not in ' \t\r\n': return
  p = pos+1
  while text[p] in ' \t\r\n': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
whitespaces = whitespaces()

@matcher()
def newlines(solver, cont):
  ''' 1 or more  newline'''
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos] not in '\r\n': return
  p = pos+1
  while text[p] in '\r\n': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
newlines = newlines()

from dao.term import DummyVar
from dao.builtins.control import and_p

def wrap_tabspaces0(item):
  _ = DummyVar('_')
  return and_p(tabspaces0, item, tabspaces0)

def wrap_tabspaces(item):
  _ = DummyVar('_')
  return and_p(tabspaces, item, tabspaces)


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
def integer(solver, cont,  arg0): 
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
def float(solver, cont,  arg0): 
  text, pos = solver.parse_state
  length = len(text)  
  if pos>=length: return
  if not '0'<=text[pos]<='9' and text[pos]!='.': return
  p = pos
  while p<length and '0'<=text[p]<='9': p += 1
  if p<length and text[p]=='.': p += 1
  while p<length and '0'<=text[p]<='9': p += 1
  if p<length-1 and text[p] in 'eE': p+=2
  while p<length and '0'<=text[p]<='9': p += 1
  if text[pos:p]=='.': return
  val = eval(text[pos:p])
  for _ in unify(arg0, val, solver.env):
    solver.parse_state = text, p
    yield cont,  True
    solver.parse_state = text, pos

number = float

@matcher()
def literal(solver, cont,  arg0):
  '''any given instance string'''
  arg0 = deref(arg0, solver.env)
  text, pos = solver.parse_state
  if text[pos:].startswith(arg0):
    solver.parse_state = text, pos+len(arg0)
    yield cont,  True
    solver.parse_state = text, pos
    
@matcher()
def identifier(solver, cont, arg):
  '''underline or letter lead, follow underline, letter or digit''' 
  text, pos = solver.parse_state
  length = len(text)
  if pos>=length: return
  if text[pos]!='_' and not 'a'<=text[pos]<='z' and not 'A'<=text[pos]<='Z': 
    return
  p = pos+1
  while p<length and (text[p]=='_' or 'a'<=text[p]<='z' or 'A'<=text[p]<='Z'
                       '0'<=text[p]<='9'): 
    p += 1
  for _ in unify(arg, text[pos:p], solver.env):
    solver.parse_state = text, p
    yield cont,  text[pos:p]
    solver.parse_state = text, pos

@matcher()
def word(solver, cont, arg):
  '''word of letters''' 
  text, pos = solver.parse_state
  length = len(text)
  if pos>=length: return
  if not 'a'<=text[pos]<='z' and not 'A'<=text[pos]<='Z': 
    return
  p = pos+1
  while p<length and ('a'<=text[p]<='z' or 'A'<=text[p]<='Z'): p += 1
  for _ in unify(arg, text[pos:p], solver.env):
    solver.parse_state = text, p
    yield cont,  text[pos:p]
    solver.parse_state = text, pos