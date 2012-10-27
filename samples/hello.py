from dao import word
from samplevars import x

def parse(grammar_element, text):
  x = Var()
  code = grammar_element(x)+x
  return eval([code, text])

def match(grammar_element, text):
  x = Var()
  code = grammar_element(x)
  return eval([code, text])
  
print parse(word, 'hello')

print match(word, 'hello')

def hello(x):
  return word('hello')+some(space)+word(x)
        #[sequence, [word, 'hello'] [some, [space]] [word, x]]

print parse(hello, 'hello world')

print match(hello, 'hello world')