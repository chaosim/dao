class Solver: 
  def __init__(self):
    solver.solved = False
    solver.failed = False
    self.cutcont = self.fcont = self.fail
    
  def solve(self, function):
    scont = function
    while 1:
      value, scont = scont(None)
      if self.solved: yield value
      if self.failed: return
      self.solved = False
      
  def done(self, value):
    self.solved = True
    if self.fcont is self.fail: 
      self.failed = True
    return value, self.fcont
  
def fun1(value):
  return 1, solver.done
                                                         