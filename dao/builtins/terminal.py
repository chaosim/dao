# -*- coding: utf-8 -*-

'''terminal used for parsing
 solver.parse_state should have an interface similar to parse_state in parser.py.
 Lineparse_state in line_parser.py is comatible with parse_state.'''

from dao.compilebase import CompileTypeError
from dao.command import special, Command, SpecialCall, Var, LogicVar, String
import dao.interlang as il

@special
def char(compiler, cont, argument):
  text = compiler.new_var(il.ConstLocalVar('text'))
  pos = compiler.new_var(il.ConstLocalVar('pos'))
  if isinstance(argument, String):
    argument = argument.interlang()
    return il.Begin((
      il.AssignFromList(text, pos, il.parse_state),
      il.If(il.Ge(pos, il.Len(text)), 
        il.failcont(il.NONE),
        il.If(il.Eq(argument, il.GetItem(text, pos)),
              il.begin(il.append_failcont(compiler, 
                              il.SetParseState(il.Tuple(text, pos))),
                       il.SetParseState(il.Tuple(text, il.add(pos, il.Integer(1)))),
                       cont(il.GetItem(text, pos))),
              il.failcont(il.NONE)))))
  
  elif isinstance(argument, Var):
    v = compiler.new_var(il.ConstLocalVar('v'))
    argument = argument.interlang()
    argument1 = compiler.new_var(il.ConstLocalVar('argument'))
    return il.Begin((
      il.AssignFromList(text, pos, il.parse_state),
      il.If(il.Ge(pos,il.Len(text)), 
        il.failcont(il.FALSE),
        il.Begin((
          il.Assign(argument1, il.Deref(argument)),
          #il.Prin(text), il.Prin(pos), il.PrintLn(argument1),
          il.If(il.Isinstance(argument1, il.Symbol('str')),
            il.If(il.Eq(argument1, il.GetItem(text, pos)),
                  il.begin(il.append_failcont(compiler, 
                                  il.SetParseState(il.Tuple(text, pos))),
                           il.SetParseState(il.Tuple(text, il.add(pos, il.Integer(1)))),
                           cont(il.GetItem(text, pos))),
                  il.failcont(il.NONE)),
            il.If(il.IsLogicVar(argument1),
                  il.begin(il.SetParseState(il.Tuple(text, il.add(pos, il.Integer(1)))),
                           il.SetBinding(argument1, il.GetItem(text, pos)),
                           il.append_failcont(compiler, 
                              il.SetParseState(il.Tuple(text, pos)),
                              il.DelBinding(argument1)),
                           cont(il.GetItem(text, pos))),
                  il.RaiseTypeError(argument1))))))))
      
  # elif isinstance(argument, il.LogicVar) #how about this? It should be include above.
  else: raise CompileTypeError(argument)
  
@special
def char_on_predicate(compiler, cont, test):
  '''return current char and step if @test succeed, where 
  @test: a python function with one argument, which tests on one char and return True or False
         @test must be registered with register_function'''
  test = test.interlang()
  text = compiler.new_var(il.ConstLocalVar('text'))
  pos = compiler.new_var(il.ConstLocalVar('pos'))
  if not isinstance(test, il.PyFunction):
    raise DaoCompileTypeError(test)
  return il.Begin((
    il.AssignFromList(text, pos, il.parse_state),
    il.If(il.Ge(pos,il.Len(text)), 
      il.failcont(il.FALSE),
      il.If(il.Call(test, il.GetItem(text, pos)),
        il.begin(
          il.SetParseState(il.Tuple(text, il.add(pos, il.Integer(1)))),
          il.append_failcont(compiler, 
                             il.SetParseState(il.Tuple(text, pos))),
          cont(il.GetItem(text, pos))),
        il.failcont(il.FALSE)))))

from dao.compilebase import register_function

def char_between(lower, upper, func_name): 
  '''return current char and step if char is between lower and upper, where 
   @test: a python function with one argument, which tests on one char and return True or False
          @test must be registered with register_function'''
  function = register_function(func_name, 
                      lambda char: lower<=char<=upper)
  return char_on_predicate(function)

def char_in(string, func_name): 
  '''return current char and step if char is in string, where 
   @test: a python function with one argument, which tests on one char and return True or False
          @test must be registered with register_function'''
  function = register_function(func_name, 
                      lambda char: char in string)
  return char_on_predicate(function)

is_digit = register_function('is_digit', lambda char: '0'<=char<='9')
digit = char_on_predicate(is_digit)

is_one_to_nine = register_function('is_one_to_nine', lambda char: '1'<=char<='9')
one_to_nine = char_on_predicate(is_one_to_nine)

is_lowcase = register_function('is_lowcase', lambda char: 'a'<=char<='z')
lowcase = char_on_predicate(is_lowcase)

is_uppercase = register_function('is_uppercase', lambda char: 'A'<=char<='Z')
uppercase = char_on_predicate(is_uppercase)

is_letter = register_function('is_letter', 
                                lambda char: ('a'<=char<='z') or ('A'<=char<='Z'))
letter = char_on_predicate(is_letter)

is_underline_letter = register_function('is_underline_letter', 
                                lambda char: char=='_' or ('a'<=char<='z') or ('A'<=char<='Z'))
underline_letter = char_on_predicate(is_underline_letter)

is_letter_digit_test = register_function('is_letter_digit_test',lambda char: char=='_' or 
                    ('0'<=char<='9') or ('a'<=char<='z') or ('A'<=char<='Z'))
underline_letter_digit = char_on_predicate(is_letter_digit_test)

space_string = ' \t'
is_tabspace  = register_function('is_tabspace', lambda char: char in space_string)
tabspace = char_on_predicate(is_tabspace)

whitespace_string = ' \t\r\n'
is_whitespace  = register_function('is_whitespace', lambda char: char in whitespace_string)
whitespace = char_on_predicate(is_whitespace)

@special
def string_on_predicate0(compiler, cont, test):
  '''return current char and step if @test succeed, where 
   @test: a python function with one argument, which tests on one char and return True or False
          @test must be registered with register_function'''
  test = test.interlang()
  text = compiler.new_var(il.ConstLocalVar('text'))
  pos = compiler.new_var(il.ConstLocalVar('pos'))
  length = compiler.new_var(il.ConstLocalVar('length'))
  p = compiler.new_var(il.LocalVar('p'))
  if not isinstance(test, il.PyFunction):
    raise DaoCompileTypeError(test)
  return il.Begin((
    il.AssignFromList(text, pos, il.parse_state),
    il.Assign(length, il.Len(text)),
    il.If(il.Ge(pos,il.Len(text)), 
      cont(il.String('')),
      il.begin(
        il.Assign(p, pos),
        il.While(il.And(il.Lt(p, length), il.Call(test, il.GetItem(text, p))),
               il.AddAssign(p, il.Integer(1))),
        il.SetParseState(il.Tuple(text, p)),
        il.append_failcont(compiler, 
                           il.SetParseState(il.Tuple(text, pos))),
        cont(il.GetItem(text, il.Slice2(pos, p)))))))

@special
def string_on_predicate1(compiler, cont, test):
  '''return current char and step if @test succeed, where 
   @test: a python function with one argument, which tests on one char and return True or False
          @test must be registered with register_function'''
  test = test.interlang()
  text = compiler.new_var(il.ConstLocalVar('text'))
  pos = compiler.new_var(il.ConstLocalVar('pos'))
  length = compiler.new_var(il.ConstLocalVar('length'))
  p = compiler.new_var(il.LocalVar('p'))
  if not isinstance(test, il.PyFunction):
    raise DaoCompileTypeError(test)
  return il.Begin((
    il.AssignFromList(text, pos, il.parse_state),
    il.Assign(length, il.Len(text)),
    il.If(il.Ge(pos, length), 
      il.failcont(il.FALSE),
      il.If(il.Call(test, il.GetItem(text, pos)),
        il.begin(
          il.Assign(p, il.add(pos, il.Integer(1))),
          il.While(il.And(il.Lt(p, length), il.Call(test, il.GetItem(text, p))),
                 il.AddAssign(p, il.Integer(1))),
          il.SetParseState(il.Tuple(text, p)),
          il.append_failcont(compiler, 
                             il.SetParseState(il.Tuple(text, pos))),
          cont(il.GetItem(text, il.Slice2(pos, p)))),
        il.failcont(il.FALSE)))))

def string_on_predicate(test, once_more=True):
  if once_more:
    return string_on_predicate1(test)
  else:
    return string_on_predicate0(test)
  
digits0 = string_on_predicate(is_digit, once_more=False)
digits1 = string_on_predicate(is_digit, once_more=True)

one_to_nines0 = string_on_predicate(is_one_to_nine, once_more=False)
one_to_nines1 = string_on_predicate(is_one_to_nine, once_more=True)

lowcases0 = string_on_predicate(is_lowcase, once_more=False)
lowcases1 = string_on_predicate(is_lowcase, once_more=True)

uppercases0 = string_on_predicate(is_uppercase, once_more=False)
uppercases1 = string_on_predicate(is_uppercase, once_more=True)

letters0 = string_on_predicate(is_letter, once_more=False)
letters1 = string_on_predicate(is_letter, once_more=True)

underline_letters0 = string_on_predicate(is_underline_letter, once_more=False)
underline_letters1 = string_on_predicate(is_underline_letter, once_more=True)

underline_letter_digits0 = string_on_predicate(is_letter_digit_test, once_more=False)
underline_letter_digits1 = string_on_predicate(is_letter_digit_test, once_more=True)

tabspaces0 = string_on_predicate(is_tabspace, once_more=False)
tabspaces1 = string_on_predicate(is_tabspace, once_more=True)

whitespaces0 = string_on_predicate(is_whitespace, once_more=False)
whitespaces1 = string_on_predicate(is_whitespace, once_more=True)

@special
def word(compiler, cont, arg):
  'word of letters'
  text = compiler.new_var(il.ConstLocalVar('text'))
  pos = compiler.new_var(il.ConstLocalVar('pos'))
  p = compiler.new_var(il.LocalVar('p'))
  length = compiler.new_var(il.ConstLocalVar('length'))
  if isinstance(arg, Var):
    arg = arg.interlang()
    x = compiler.new_var(il.ConstLocalVar('x'))
    return il.Begin((
      il.AssignFromList(text, pos, il.parse_state),
      il.Assign(length, il.Len(text)),
      il.If(il.Ge(pos, length),
        il.failcont(il.FALSE),
          il.If(il.And(il.Not(il.Cle(il.String('a'), il.GetItem(text, pos), il.String('z'))), 
                       il.Not(il.Cle(il.String('A'), il.GetItem(text, pos), il.String('Z')))), 
            il.failcont(il.FALSE),
            il.Begin((
              il.Assign(p, il.add(pos, il.Integer(1))),
              il.while_(il.And(il.Lt(p, length), 
                              il.Or(il.Cle(il.String('a'), il.GetItem(text, p), il.String('z')), 
                                    il.Cle(il.String('A'),il.GetItem(text, p),il.String('Z')))), 
                       il.AddAssign(p, il.Integer(1))),
              il.Assign(x, il.Deref(arg)),
              il.If(il.IsLogicVar(x),
                    il.begin(il.SetParseState(il.Tuple(text, p)),
                                   il.SetBinding(x, il.GetItem(text, il.Slice2(pos, p))),
                                   il.append_failcont(compiler, 
                                      il.SetParseState(il.Tuple(text, pos)),
                                      il.DelBinding(x)),
                                   cont(il.GetItem(text, pos))),
                    il.If(il.Isinstance(x, il.String('str')),
                          il.If(il.Eq(x, il.GetItem(text, il.Slice2(pos, p))),
                                il.begin(il.append_failcont(compiler, 
                                  il.SetParseState(il.Tuple(text, pos))),
                                  il.SetParseState(il.Tuple(text, p)),
                                  cont(il.GetItem(text, pos))),
                                il.failcont(il.NONE)),
                          il.RaiseTypeError(x)))))))))
  elif isinstance(arg, String):
    arg = arg.interlang()
    return il.Begin((
      il.AssignFromList(text, pos, il.parse_state),
      il.Assign(length, il.Len(text)),
      il.If(il.Ge(pos, length), 
        il.failcont(il.FALSE),
        il.If(il.And(il.Not(il.Cle(il.String('a'), il.GetItem(text, pos), il.String('z'))), 
                     il.Not(il.Cle(il.String('A'), il.GetItem(text, pos), il.String('Z')))), 
          il.failcont(il.FALSE),
        il.Begin((
          il.Assign(p, il.add(pos, il.Integer(1))),
          il.while_(il.And(il.Lt(p, length), 
                          il.Or(il.Cle(il.String('a'), il.GetItem(text, p), il.String('z')), 
                                il.Cle(il.String('A'),il.GetItem(text, p), il.String('Z')))), 
                  il.AddAssign(p, il.Integer(1))),
          il.If(il.Eq(arg, il.GetItem(text, il.Slice2(pos, p))),
                il.begin(il.append_failcont(compiler, 
                  il.SetParseState(il.Tuple(text, pos))),
                  il.SetParseState(il.Tuple(text, p)),
                  cont(arg)),
                il.failcont(il.NONE))))))))
  else:
    raise CompileTypeError
  
@special
def identifier(compiler, cont, arg):
  '''underline or letter lead, follow underline, letter or digit'''
  text = compiler.new_var(il.ConstLocalVar('text'))
  pos = compiler.new_var(il.ConstLocalVar('pos'))
  p = compiler.new_var(il.LocalVar('p'))
  length = compiler.new_var(il.ConstLocalVar('length'))
  if isinstance(arg, Var):
    arg = arg.interlang()
    x = compiler.new_var(il.ConstLocalVar('x'))
    return il.Begin((
      il.AssignFromList(text, pos, il.parse_state),
      il.Assign(length, il.Len(text)),
      il.If(il.Ge(pos, length), 
        il.failcont(il.FALSE),
          il.If(il.and_(il.Ne(il.GetItem(text, pos), il.String('_')),
                         il.Not(il.Cle(il.String('a'), il.GetItem(text, pos), il.String('z'))), 
                         il.Not(il.Cle(il.String('A'), il.GetItem(text, pos), il.String('Z')))), 
            il.failcont(il.FALSE),
            il.Begin((
              il.Assign(p, il.add(pos, il.Integer(1))),
              il.while_(il.And(il.Lt(p, length), 
                              il.or_(il.Eq(il.GetItem(text, pos), il.String('_')),
                                    il.Cle(il.String('a'), il.GetItem(text, p), il.String('z')), 
                                    il.Cle(il.String('A'),il.GetItem(text, p),il.String('Z')),
                                    il.Cle(il.String('0'),il.GetItem(text, p),il.String('9')))), 
                       il.AddAssign(p, il.Integer(1))),
              il.Assign(x, il.Deref(arg)),
              il.If(il.IsLogicVar(x),
                    il.begin(il.SetParseState(il.Tuple(text, p)),
                                   il.SetBinding(x, il.GetItem(text, il.Slice2(pos, p))),
                                   il.append_failcont(compiler, 
                                      il.SetParseState(il.Tuple(text, pos)),
                                      il.DelBinding(x)),
                                   cont(il.GetItem(text, pos))),
                    il.If(il.Isinstance(x, il.String('str')),
                          il.If(il.Eq(x, il.GetItem(text, il.Slice2(pos, p))),
                                il.begin(il.append_failcont(compiler, 
                                  il.SetParseState(il.Tuple(text, pos))),
                                  il.SetParseState(il.Tuple(text, p)),
                                  cont(il.GetItem(text, pos))),
                                il.failcont(il.NONE)),
                          il.RaiseTypeError(x)))))))))
  elif isinstance(arg, String):
    arg = arg.interlang()
    return il.Begin((
      il.AssignFromList(text, pos, il.parse_state),
      il.Assign(length, il.Len(text)),
      il.If(il.Ge(pos, length), 
        il.failcont(il.FALSE),
        il.If(il.and_(il.Ne(il.GetItem(text, pos), il.String('_')),
                         il.Not(il.Cle(il.String('a'), il.GetItem(text, pos), il.String('z'))), 
                         il.Not(il.Cle(il.String('A'), il.GetItem(text, pos), il.String('Z')))), 
            il.failcont(il.FALSE),
            il.Begin((
              il.Assign(p, il.add(pos, il.Integer(1))),
              il.while_(il.And(il.Lt(p, length), 
                              il.or_(il.Eq(il.GetItem(text, pos), il.String('_')),
                                    il.Cle(il.String('a'), il.GetItem(text, p), il.String('z')), 
                                    il.Cle(il.String('A'),il.GetItem(text, p),il.String('Z')),
                                    il.Cle(il.String('0'),il.GetItem(text, p),il.String('9')))), 
                       il.AddAssign(p, il.Integer(1))),
              il.If(il.Eq(arg, il.GetItem(text, il.Slice2(pos, p))),
                    il.begin(il.append_failcont(compiler, 
                      il.SetParseState(il.Tuple(text, pos))),
                      il.SetParseState(il.Tuple(text, p)),
                      cont(arg)),
                    il.failcont(il.NONE))))))))
  else:
    raise CompileTypeError(arg)
  
@special
def integer(compiler, cont, arg):
  '''integer'''
  text = compiler.new_var(il.ConstLocalVar('text'))
  pos = compiler.new_var(il.ConstLocalVar('pos'))
  p = compiler.new_var(il.LocalVar('p'))
  length = compiler.new_var(il.ConstLocalVar('length'))
  if isinstance(arg, Var):
    arg = arg.interlang()
    x = compiler.new_var(il.ConstLocalVar('x'))
    return il.Begin((
      il.AssignFromList(text, pos, il.parse_state),
      il.Assign(length, il.Len(text)),
      il.If(il.Ge(pos, length), 
        il.failcont(il.FALSE),
          il.If(il.Not(il.Cle(il.String('0'), il.GetItem(text, pos), il.String('9'))),
            il.failcont(il.FALSE),
            il.Begin((
              il.Assign(p, il.add(pos, il.Integer(1))),
              il.while_(il.And(il.Lt(p, length), 
                              il.Cle(il.String('0'),il.GetItem(text, p),il.String('9'))), 
                       il.AddAssign(p, il.Integer(1))),
              il.Assign(x, il.Deref(arg)),
              il.If(il.IsLogicVar(x),
                    il.begin(il.SetParseState(il.Tuple(text, p)),
                                   il.SetBinding(x, il.GetItem(text, il.Slice2(pos, p))),
                                   il.append_failcont(compiler, 
                                      il.SetParseState(il.Tuple(text, pos)),
                                      il.DelBinding(x)),
                                   cont(il.GetItem(text, pos))),
                    il.If(il.Isinstance(x, il.String('str')),
                          il.If(il.Eq(x, il.GetItem(text, il.Slice2(pos, p))),
                                il.begin(il.append_failcont(compiler, 
                                  il.SetParseState(il.Tuple(text, pos))),
                                  il.SetParseState(il.Tuple(text, p)),
                                  cont(il.GetItem(text, pos))),
                                il.failcont(il.NONE)),
                          il.RaiseTypeError(x)))))))))
  elif isinstance(arg, il.String):
    return il.Begin((
      il.AssignFromList(text, pos, il.parse_state),
      il.Assign(length, il.Len(text)),
      il.If(il.Ge(pos, length), 
        il.failcont(il.FALSE),
          il.If(il.Not(il.Cle(il.String('0'), il.GetItem(text, pos), il.String('9'))),
            il.failcont(il.FALSE),
            il.Begin((
              il.Assign(p, il.add(pos, il.Integer(1))),
              il.while_(il.And(il.Lt(p, length), 
                              il.Cle(il.String('0'),il.GetItem(text, p),il.String('9'))), 
                       il.AddAssign(p, il.Integer(1))),
              il.If(il.Eq(arg, il.GetItem(text, il.Slice2(pos, p))),
                    il.begin(il.append_failcont(compiler, 
                      il.SetParseState(il.Tuple(text, pos))),
                      il.SetParseState(il.Tuple(text, p)),
                      cont(arg)),
                    il.failcont(il.NONE))))))))
  else:
    raise CompileTypeError
  
@special
def literal(compiler, cont, arg):
  '''any given instance string'''
  text = compiler.new_var(il.ConstLocalVar('text'))
  pos = compiler.new_var(il.ConstLocalVar('pos'))
  p = compiler.new_var(il.LocalVar('p'))
  i = compiler.new_var(il.LocalVar('i'))
  x = compiler.new_var(il.ConstLocalVar('x'))
  length = compiler.new_var(il.ConstLocalVar('length'))  
  length2 = compiler.new_var(il.ConstLocalVar('length2'))  
  if isinstance(arg, Var):
    return il.Begin((
      il.Assign(x, il.Deref(arg)),
      il.AssignFromList(text, pos, il.parse_state),
      il.Assign(length, il.Len(text)),
      il.Assign(length, il.Len(x)),
      il.Assign(i, il.Integer(0)),
      il.Assign(p, pos),
      il.while_(il.and_(il.Lt(i, length2), il.Lt(p, length), il.Eq(il.GetItem(text, p), il.GetItem(x, i))), 
        il.AddAssign(p, il.Integer(1))),
      il.If(il.Lt(i, length2), 
        il.failcont(il.NONE),
        il.begin(il.append_failcont(compiler, 
          il.SetParseState(il.Tuple(text, pos))),
          il.SetParseState(il.Tuple(text, p)),
          cont(arg)))))
  elif isinstance(arg, String):
    arg = arg.interlang()
    return il.Begin((
      il.AssignFromList(text, pos, il.parse_state),
      il.Assign(length, il.Len(text)),
      il.Assign(length2, il.Len(arg)),
      il.Assign(i, il.Integer(0)),
      il.Assign(p, pos),
      il.while_(il.and_(il.Lt(i, length2), il.Lt(p, length), il.Eq(il.GetItem(text, p), il.GetItem(arg, i))), 
        il.AddAssign(p, il.Integer(1)),
        il.AddAssign(i, il.Integer(1)),
        ),
      il.If(il.Lt(i, length2), 
        il.failcont(il.NONE),
        il.begin(il.append_failcont(compiler, 
          il.SetParseState(il.Tuple(text, pos))),
          il.SetParseState(il.Tuple(text, p)),
          cont(arg)))))
  else:
    raise CompileTypeError

  
'''
@matcher()
def lead_chars(solver, chars):
  chars = deref(chars, solver.env)
  assert isinstance(chars, str)
  if last_char_(solver.parse_state) not in chars: return
  yield cont,  True

@matcher()
def not_lead_chars(solver, chars):
  chars = deref(chars, solver.env)
  assert isinstance(chars, str)
  if last_char_(solver.parse_state) in chars: return
  yield cont,  True

@matcher()
def follow_chars(solver, chars):
  chars = deref(chars, solver.env)
  if eoi_(solver.parse_state) or\
     next_char_(solver.parse_state) not in chars: 
    return
  yield cont,  True

@matcher()
def follow_char(solver, char):
  char = deref(char, solver.env)
  if eoi_(solver.parse_state) or\
     next_char_(solver.parse_state)!=char: 
    return
  yield cont,  True

@matcher()
def not_follow_chars(solver, chars):
  chars = deref(chars, solver.env)
  if not eoi_(solver.parse_state) and next_char_(solver.parse_state) in chars: 
    return
  yield cont,  True

@matcher()
def not_follow_char(solver, char):
  char = deref(char, solver.env)
  if not eoi_(solver.parse_state) and next_char_(solver.parse_state)==char: 
    return
  yield cont,  True

def lead_string(solver, string):
  strArgument = deref(strArgument, solver.env)
  assert isinstance(strArgument, str)
  if not parsed_(solver.parse_state).endwith(strArgument):return
  yield cont,  True

@matcher()
def not_lead_string(solver, string):
  string = string.deref(solver.env)
  if parsed_(solver.parse_state).endwith(string): return
  solver.value = True  

def follow_string(solver, strArgument):
  strArgument = strArgument.deref(solver.env)
  assert isinstance(strArgument, String)
  if not left_(solver.parse_state).startswith(strArgument.name): raise UnifyFail
  solver.value = True

@matcher()
def not_follow_string(solver, string):
  string = string.deref(solver.env)
  if left_(solver.parse_state).startswith(string.name): return
  solver.value = True  

@matcher()
def any_chars_except(solver, except_chars):
  'any chars until meet except_chars'
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
def space(solver):
  'one space'
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]==' ': 
    solver.parse_state = text, pos+1
    yield cont, ' '
    solver.parse_state = text, pos
space = space()

@matcher()
def tab(solver):
  'one tab'
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]=='\t': 
    solver.parse_state = text, pos+1
    yield cont, '\t'
    solver.parse_state = text, pos
tab = tab()

@matcher()
def tabspace(solver):
  'one space or tab'
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos]==' ' or text[pos]=='\t': 
    solver.parse_state = text, pos+1
    yield cont, text[pos]
    solver.parse_state = text, pos
tabspace = tabspace()

@matcher()
def whitespace(solver):
  'one space or tab'
  text, pos = solver.parse_state
  if pos==len(text): return
  if text[pos] in ' \t\r\n': 
    solver.parse_state = text, pos+1
    yield cont, text[pos]
    solver.parse_state = text, pos
whitespace = whitespace()

@matcher()
def newline(solver):
  'one newline'
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

@matcher()
def spaces0(solver)
  '0 or more space'
  text, pos = solver.parse_state
  length = len(text)
  p = pos
  while p<length and text[p]==' ': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
spaces0 = spaces0()

@matcher()
def tabs0(solver):
  '0 or more tab'
  text, pos = solver.parse_state
  length = len(text)
  p = pos
  while p<length and text[p]=='\t': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
tabs0 = tabs0()

@matcher()
def _Tabspaces0(solver):
  '0 or more space or tab'
  text, pos = solver.parse_state
  length = len(text)
  p = pos
  while p<length and (text[p]==' ' or text[p]=='\t'): p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
tabspaces0 = _Tabspaces0()

@matcher()
def whitespaces0(solver):
  text, pos = solver.parse_state
  length = len(text)
  p = pos
  while p<length and text[p] in ' \t\r\n': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
whitespaces0 = whitespaces0()

@matcher()
def newlines0(solver):
  text, pos = solver.parse_state
  length = len(text)
  p = pos
  while p<length and text[p] in '\r\n': p += 1
  solver.parse_state = text, p
  yield cont, text[pos:p]
  solver.parse_state = text, pos
newlines0 = newlines0()

@matcher()
def spaces(solver):
  '1 or more space or tab'
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
def tabs(solver):
  '1 or more space or tab'
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
def _Tabspaces(solver):
  '1 or more space or tab'
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
def pad_tabspaces(solver):
  'if not leading space, 1 or more space or tab'
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
def tabspaces_if_need(solver):
  '1 or more tabspace not before punctuation '",;:.{}[]()!?\r\n '
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
def tabspaces_unless(solver, chars):
  '1 or more tabspace if not before chars, else 0 or more tabspace '
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
def whitespaces(solver):
  '1 or more space or tab or newline'
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
def newlines(solver):
  ' 1 or more  newline'
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
  def func(solver,  arg0):
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
def float(solver,  arg0): 
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

'''