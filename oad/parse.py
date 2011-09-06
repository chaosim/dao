from oad.term import conslist
from oad.builtins.parser import settext
from oad.builtins.terminal import eof
from oad.builtins.control import and_
from oad.eval import Evaluator

class Grammar:
  def __init__(self, start, rules, result):
    self.rules, self.start, self.result = rules, start, result

def parse(grammar, text):
  global envvarCache
  envvarCache = {}
  evaluator = Evaluator()
  exp = conslist('letrec', grammar.rules, (settext, text), 
                 (and_, grammar.start, [eof]), grammar.result)
  return evaluator.eval(exp)

def eval(grammar, text): #don't need any more!!!
  evaluator = Evaluator()
  exp = conslist('letrec', grammar.rules, (settext, text), 
                 (and_, grammar.start, [eof]), grammar.result)
  parsedExp = evaluator.eval(exp)
  return evaluator.eval(parsedExp) #, prelude=False
  