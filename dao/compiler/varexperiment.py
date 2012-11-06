def f(a, *kwargs):
  locals()['a'] = 1
  print locals()  
  
  return

print f.func_closure#, f.defaults, f.kwargs

f(2)
