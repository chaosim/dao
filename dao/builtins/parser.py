from dao.compilebase import CompileTypeError
from dao.command import special, Command, SpecialCall
import dao.interlang as il
from dao.interlang import TRUE, FALSE, NONE

v0, fc0 = il.LocalVar('v'), il.LocalVar('fc')

# set and manipulate parse_state for parsing 

@special
def parse_state(compiler, cont):
  return cont(il.parse_state)

@special
def set_parse_state(compiler, cont, parse_state):
  old_parse_state = compiler.new_var(il.LocalVar('old_parse_state'))
  return il.begin(il.Assign(old_parse_state, il.parse_state),
                  il.SetParseState(parse_state),
                  il.append_failcont(compiler, il.SetParseState(old_parse_state)),
                  cont(TRUE))

@special
def set_sequence(compiler, cont, sequence):
  old_parse_state = compiler.new_var(il.LocalVar('old_parse_state'))
  sequence = sequence.interlang()
  return il.begin(il.Assign(old_parse_state, il.parse_state),
                  il.SetParseState(il.Tuple(sequence, il.Integer(0))),
                  il.append_failcont(compiler, il.SetParseState(old_parse_state)),
                  cont(TRUE))

set_text = set_sequence

@special
def parse(compiler, cont, predicate, parse_state):
  old_parse_state = compiler.new_var(il.LocalVar('old_parse_state'))
  v = compiler.new_var(il.LocalVar('v'))
  return il.begin(il.Assign(old_parse_state, il.parse_state),
                  il.SetParseState(parse_state),
                  il.append_failcont(compiler, il.SetParseState(old_parse_state)),
                  predicate.cps_convert(compiler, 
                      il.clamda(v, 
                                il.Assign(il.parse_state, old_parse_state),
                                cont(v))))

@special
def parse_sequence(compiler, cont, predicate, sequence):
  old_parse_state = compiler.new_var(il.LocalVar('old_parse_state'))
  v = compiler.new_var(il.LocalVar('v'))
  sequence = sequence.interlang()
  return il.begin(il.Assign(old_parse_state, il.parse_state),
                  il.SetParseState(il.Tuple(sequence, il.Integer(0))),
                  il.append_failcont(compiler, il.SetParseState(old_parse_state)),
                  predicate.cps_convert(compiler, 
                      il.clamda(v, 
                                il.SetParseState(old_parse_state),
                                cont(v))))
parse_text = parse_sequence


@special
def get_parse_state(compiler, cont):
  return cont(il.parse_state)

@special
def get_parse_sequence(compiler, cont):
  return cont(il.GetItem(il.parse_state, 0))

get_parse_text = get_parse_sequence


@special
def Eoi(compiler, cont):
  '''end of parse_state'''
  return il.If(il.Ge(il.GetItem(il.parse_state, il.Integer(1)), 
                     il.Len(il.GetItem(il.parse_state, il.Integer(0)))),
          cont(TRUE),
          il.failcont(FALSE))  

eoi = Eoi()

@special
def Boi(compiler, cont):
  '''end of parse_state'''
  return il.If(il.Le(il.GetItem(il.parse_state, il.Integer(1)),0),
          cont(TRUE),
          il.failcont(FALSE))  

boi = Boi()

@special
def step(compiler, cont, n=il.Integer(1)): 
  # return current element before step
  text = compiler.new_var(il.LocalVar('text'))
  pos = compiler.new_var(il.LocalVar('pos'))
  return il.Begin((
    il.AssignFromList(text, pos, il.parse_state),
    il.append_failcont(compiler, il.SetParseState(il.Tuple(text, pos))),
    il.SetParseState(il.Tuple(text, il.add(pos, n))),
    cont(il.GetItem(text, pos))))

@special
def skip(compiler, cont, n=il.Integer(1)): 
  # return element after skip
  text = compiler.new_var(il.LocalVar('text'))
  pos = compiler.new_var(il.LocalVar('pos'))
  return il.Begin((
    il.AssignFromList(text, pos, il.parse_state),
    il.append_failcont(compiler, il.SetParseState(il.Tuple(text, pos))),
    il.SetParseState(il.Tuple(text, il.add(pos, n))),
    cont(il.GetItem(text, il.add(pos, n)))))

@special
def left(compiler, cont, length=NONE):
  text = compiler.new_var(il.LocalVar('text'))
  pos = compiler.new_var(il.LocalVar('pos'))
  return il.Begin((
    il.AssignFromList(text, pos, il.parse_state),
    cont(il.GetItem(text, il.If(il.IsNot(length, NONE),
                                il.Slice2(pos, il.add(pos,length)),
                                il.Slice2(pos, NONE))))))

@special
def next_element(compiler, cont): 
  text = compiler.new_var(il.LocalVar('text'))
  pos = compiler.new_var(il.LocalVar('pos'))
  return il.Begin((
    il.AssignFromList(text, pos, il.parse_state),
    cont(il.GetItem(text, pos))))
next_char = next_element

@special
def position(compiler, cont): 
  return cont(il.GetItem(il.parse_state, il.Integer(1)))

@special
def subsequence(compiler, cont, start, end): 
  text = compiler.new_var(il.LocalVar('text'))
  pos = compiler.new_var(il.LocalVar('pos'))
  start = il.Integer(start.item)
  end = il.Integer(end.item)
  return il.Begin((
    il.AssignFromList(text, pos, il.parse_state),
     cont(il.GetItem(text, il.Slice2(start, end)))))

subtext = subsequence

@special
def goto(compiler, cont, position): 
  text = compiler.new_var(il.LocalVar('text'))
  pos = compiler.new_var(il.LocalVar('pos'))
  position = il.Integer(position.item)
  return il.Begin((
    il.AssignFromList(text, pos, il.parse_state),
    il.append_failcont(compiler, il.SetParseState(il.Tuple(text, pos))),
    il.SetParseState(il.Tuple(text, position)),
    cont(il.GetItem(text, position))))

@special
def unify_parse_sequence(compiler, cont, sequence):
  x = compiler.new_var(il.LocalVar('x'))
  try:  
    sequence.cps_convert_unify
  except:
    sequence = sequence.interlang()
    return il.If(il.Eq(sequence, il.GetItem(il.parse_state, il.Integer(0))),
          cont(TRUE),
          il.failcont(il.TRUE))          
  return il.Begin((
      il.Assign(x, il.Deref(sequence.interlang())),
      il.If(il.IsLogicVar(x),
            il.begin(il.SetBinding(x, il.GetItem(il.parse_state, il.Integer(0))),
                     il.append_failcont(compiler, il.DelBinding(x)),
                      cont(il.TRUE)),
            il.If(il.Eq(x, il.GetItem(il.parse_state, il.Integer(0))),
                      cont(TRUE),
                      il.failcont(il.TRUE))))) 

unify_parse_text = unify_parse_sequence

'''
@matcher()
def unify_parse_state(solver, parse_state):
  for _ in unify(parse_state, solver.parse_state, solver.env):
    yield cont, solver.parse_state

'''