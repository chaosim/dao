def done(value, solver):
  solver.scont = fcont
  solved = True
  return value

def fail_done(value, solver):
  solver.failed = True
  
class Solver:
  def solve(self, scont, fcont):
    self.fcont = fcont
    self.scont = scont
    while 1:
      value = self.scont(value, self)
      if self.solved: 
        yield value
        if failed: return
        self.solved = False


