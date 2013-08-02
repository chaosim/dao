def solve(goal, cont, fc):
  solver = Solver()
  return goal(cont)(None, fc, Bindings(), None)
  
class Element: pass

class Lambda: pass

class Clamda: 
  def run(self, v, solver):
    pass
  
class Done(Clamda):
  def __call__(self, v):
    return ValueCont(v)
  
  def run(self, v, solver):
    return v
  
class End(Clamda):
  def __call__(self, v):
    return self
  
  def run(self, v, solver):
    print 'end'

end = End()

class ValueCont(Clamda):
  def __init__(self, value):
    self.value = value
    
  def __call__(self, v):
    return self
  
  def run(self, v, solver):
    return self.value

class Bindings(dict):
  def __add__(self, other):
    result = Bindings(self)
    result.update(other)
    return result
  
  def add(self, key, value):
    self[key] = value

class Var:
  def __init__(self, name):
    self.name = name
  
  def cps(self, cont):
    def var_fun2(v, fc, bindings, parse_state):
      return cont(self.deref(bindings), fc, bindings, parse_state)
    return var_fun2
  
  def deref(self, bindings):
    try:
      v = bindings[self]
    except:
      return self
    while not isinstance(v, Var):
      self = v
      try: 
        v = bindings[self]
      except:
        return v
    return v
  
  def getvalue(self, memo, bindings):
    try: return memo[self]
    except:
      binding = bindings[self]
      if binding is self: 
        memo[self] = binding
        return binding
      else:
        result = getvalue(binding, memo)
        return result
      
  def __eq__(x, y):
    return isinstance(y, Var) and x.name==y.name
  
  def __hash__(self): return hash(self.name)
  
  def __repr__(self): return self.name
    
class DummyVar(Var):
  def __init__(self, name='_'): 
    Var.__init__(self, name)
      
  def deref(self, bindings): 
    return self

  def getvalue(self, memo, bindings):
    try: return memo[self]
    except:
      binding = bindings[self]
      if binding is self: 
        memo[self] = binding
        return binding
      else:
        result = getvalue(binding, memo)
        return result

def deref(item, bindings):
  try:
    item_deref = item.deref
  except:
    return item
  return item_deref(bindings)
  
class Integer(Element):
  def __init__(self, value):
    self.value = value
    
  def cps(self, cont):
    return cont(self.value)

class PrinCont(Clamda):
  def __init__(self, item, cont):
    self.item, self.cont = item, cont
    
  def run(self, v, solver):
    print self.item,
    self.cont.run(v, solver)
    
class Prin(Element):
  def __init__(self, item):
    self.item = item
  
  def cps(self, cont):
    return PrinCont(self.item, cont)
  
class Succeed(Element):
  def cps(self, cont):
    return cont(True)

succeed = Succeed()

class Fail(Element):
  def cps(self, cont):
    return failcont

fail = Fail()

class FailCont(Clamda):
  def run(self, v, solver):
    return fc.run(False)

failcont = FailCont()

class And(Element):
  def __init__(self, item1, item2):
    self.item1, self.item2 = item1, item2
    
  def cps(self, cont):
    return self.item1.cps(self.item2.cps(cont))

class OrCont(Clamda):
  def __init__(self, item, cont):
    self.item = item
    
class Or(Element):
  def __init__(self, item1, item2):
    self.item1, self.item2 = item1, item2
    
  def cps(self, cont):
    solver.failcont = self.item2.cps(cont)
    return self.item1.cps(cont)

x = And(Integer(1), Integer(2))
x = x.cps(Done())

x.run(None, None, None)

x = And(Prin(1), Prin(2))
x = x.cps(Done())

x.run(None, None, None)

class NotCont(Clamda):
  def __init__(self, goal, cont):
    self.goal = goal
    self.cont = cont
    
  def run(self, v, solver):
    fc = solver.failcont
    solver.failcont = self.cont
    return self.goal.cps(fc).run(v, solver)
  
class Not(Element):
  def __init__(self, goal):
    self.goal = goal
    
  def cps(self, cont):
    return NotCont(self.goal, cont)

x = Not(succeed).cps(Done())
x.run(None, end, None)

class IfCont(Clamda):
  def __init__(self, then_cont, else_cont):
    self.then_cont, self.else_cont = then_cont, else_cont
    
  def __call__(self, value):
    if value:
      return self.then_cont
    else: 
      return self.else_cont
    
  def run(self, v, solver):
    if v:
      return self.then_cont.run(v, fc, bindings, parse_state)
    else:
      return self.else_cont.run(v, fc, bindings, parse_state)
          
class If(Element):
  def __init__(self, test, then, else_):
    self.test, self.then, self.else_ = test, then, else_
    
  def cps(self, cont):
    return self.test.cps(IfCont(self.then.cps(cont), 
                                        self.else_.cps(cont)))  

class SetTextCont(Clamda):
  def __init__(self, text, cont):
    self.text = text
    self.cont = cont
    
  def run(self, v, solver):
    solver.parse_state = self.text, 0
    self.cont.run(v, solver)
    
class SetText(Element):
  def __init__(self, text):
    self.text = text
    
  def cps(self, cont):
    return SetTextCont(text, cont)
