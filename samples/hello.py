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



#
def f():
  global a
  a1 = 1
  if a1:
    a2 = 2
  else:
    pass
  a3 = phi(a1, a2)
  use(a3)

def f():
  global a
  a1 = 1
  if a1:
    a2 = 2
  else:
    a3 = 3
  a4 = phi(a2, a3)
  print a4
  use(a4)

a = 3  
def f():
  global a
  a = phi(a, a)
  a = a - 1
  f()
  use(a)
  
a1 = 3
def f():
  global a
  a3 = phi(a1, a2)
  a2 = a3-1
  f()
  use(a2)

i1 = 0
j1 = 0
def f():
  i3 = phi(i1, i2) #i3 = phi(0, j3)
  j3 = phi(j1, j2) #j3 = phi(0, j3+1)
  i2 = j3
  j2 = i2+1        #j2 = j3+1
  g()
  use(i2, j2)
  
  
i1 = 0
j1 = 0
def f():
  i3 = phi(i1, i2) #i3 = phi(0, 0)
  j3 = phi(j1, j2) #j3 = phi(0, j3+1)
  i2 = 0
  j2 = j2+1        #j2 = j3+1
  g()
  i5 = phi(i3, i4(g()))
  i6 = i5+1
  use(i6, j2)