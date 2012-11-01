def callcc(k, f):
  return f(k, lambda k1, result: k(result))

class Solver: 
  def __init__(self):
    self.solved = False
    self.failed = False
    self.cutcont = self.fcont = None
    
  def solve(self, scont, value):
    while scont is not None:
      value, scont = scont(value)
      if self.solved: 
        yield value
        self.solved = False
      
  def done(self, value):
    self.solved = True
    return value, self.fcont

solver = Solver()

def solve(fun):
  return solver.solve(fun, None)

def fun1(value):
  return 1, solver.done

def fun2(value):
  return 1, lambda value: (2, solver.done)
  
for x in solve(fun2):
  print x