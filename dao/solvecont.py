''' experiment for solving double continuations, 
a interpreter simulation for compiler'''

class Bindings(dict): 
  def __getitem__(self, var): 
    try: return dict.__getitem__(self, var)
    except: return var
  def __setitem__(self, var, value):
    dict.__setitem__(self, var, value)
  def repr(self): return '%r'%self

class Var:
  def __init__(self, name):
    self.name = name
                
  def deref(self, bindings):
    v = bindings[self]
    if v is self: return v
    elif not isinstance(v, Var): return v
    else: return v.deref(bindings)
  
  def __eq__(x, y):
    return isinstance(y, Var) and x.name==y.name
  
  def __hash__(self): return hash(self.name)
  
  def __repr__(self): return self.name
  
class DummyVar(Var):
  def __init__(self, name='_'): 
    Var.__init__(self, name)
      
  def deref(self, bindings): return self

  def getvalue(self, memo):
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

(begin, print_, if_, quote,
 succeed, fail, and_, or_, findall, not_, unify,
 settext, char, eoi, any, lazy_any, greedy_any) = (
  'begin', 'print', 'if', 'quote',
  'succeed', 'fail', 'and', 'or', 'findall', "not", "unify",
 'settext', 'char', 'eoi', 'any', 'lazy any', 'greedy any')

def solve(exp):
  solver = Solver()
  return Solver().solve(exp, solver.done, solver.end)

def solve_all(exp):
  return Solver().solve_all(exp)

class Solver:
  def __init__(self):
    self.pos, self.text = 0, ''
    self.bindings = Bindings()
    
  def solve(self, exp, done, end):
    self.finished = False
    self.fcont = end
    return self.cps(exp, done)
    #result = result(None)
    #cont = result
    #while not finished:
      #cond = cont(v)
    #return cont
  
  def solve_all(self, exp):
    return self.solve(exp, self.done_all, self.end)
  
  def done_all(self, v):
    print 'Succeed done!'
    self.fcont(v)
      
  def end(self, v):
    self.finished = True
    print 'Failed at last!'

  def done(self, v):
    self.finished = True
    print 'Succeed done!'
    return v
  
  def cps_exps(self, exps, cont):
    if len(exps)==0: return cont(())
    elif len(exps)==1: return self.cps(exps[0], cont)
    else:
      return self.cps(exps[0], lambda v: self.cps_exps(exps[1:], cont))
  
  def cps(self, exp, cont):
    global pos, text
    if isinstance(exp, int) or isinstance(exp, str):
      print exp
      return cont(exp)
    
    elif isinstance(exp, Var):
      print exp
      return cont(exp.deref(bindings))
    
    elif isinstance(exp, list) or isinstance(exp, tuple):
      
      if exp[0]==quote: 
        print quote, exp[1]
        return cont(exp[1])
      
      if exp[0]==begin: # and is equal to begin
        return self.cps_exps(exp[1:], cont)
      
      if exp[0]==if_: # (if x y z)
        def if_cont(v):
          if(v): return self.cps(exp[2], cont)
          else: return self.cps(exp[3], cont)
        return self.cps(exp[1], if_cont)
      
      elif exp[0]==print_:
        print ','.join([str(x) for x in exp[1:]])
        return cont(None)
      
      elif exp[0]==succeed:
        return cont(True)
      
      elif exp[0]==fail:
        return self.fcont(True)
  
      elif exp[0]==not_:
        fcont = self.fcont
        self.fcont = cont
        return self.cps(exp[1], fcont)
       
      if exp[0]==and_: # and is equal to begin
        return self.cps(exp[1], self.cps(exp[2], cont))
      
      elif exp[0]==or_:
        if len(exp)==1: 
          return cont(True)
        elif len(exp)==2: 
          return self.cps(exp[1], cont)
        elif len(exp)==3:
          fc= self.fcont
          def or_fcont(v):
            self.fcont = fc
            return self.cps(exp[2], cont)
          self.fcont = or_fcont
          return self.cps(exp[1], cont)
        else: 
          fc= self.fcont
          def or_fcont(v):
            self.fcont = fc
            return self.cps((or_,)+tuple(exp[2:]), cont)
          self.fcont = or_fcont
          return self.cps(exp[1], cont)
      
      elif exp[0]==unify: #(unify, x, y)
        x, y = exp[1:]
        fc = self.fcont
        if isinstance(x, Var):
          x = deref(x, self.bindings)
          if isinstance(x, Var):
            self.bindings[x] = y
            def unify_fcont(v):
              del self.bindings[x]
              return fc(False)
            self.fcont = unify_fcont
            return cont(True)
          else:
            y = deref(y, self.bindings)
            if isinstance(y, Var):
              self.bindings[y] = x
              def unify_fcont(v):
                try: del self.bindings[y]
                except: pass
                return fc(False)
              self.fcont = unify_fcont
              return cont(True)
        elif isinstance(y, Var):
          y = deref(y, self.bindings)
          if isinstance(y, Var):
            self.bindings[y] = x
            def unify_fcont(v):
              try: del self.bindings[y]
              except: pass
              return fc(False)
            self.fcont = unify_fcont
            return cont(True)
        if x==y: return cont(True)
        else: return fc(False)
      
      elif exp[0]==settext:
        fc = self.fcont
        self.parse_state = 0, exp[1]
        def settext_fcont(v):
          self.parse_state = old_parse_state
          return fc(v)
        self.fcont = settext_fcont
        return cont(None)
        
      elif exp[0]==eoi:
        print eoi, pos,
        if pos==len(text):
          print 'succeed.'
          return cont(True)
        print text[pos], 'failed'
        return self.fcont(False)
      
      elif exp[0]==char:
        pos, text = self.parse_state
        print char, pos, 
        if pos==len(text):
          print 'failed'
          return self.fcont(False)
        else:
          print text[pos],
          c = deref(exp[1])
          fc = self.fcont
          if isinstance(c, str):
            if c==text[pos]:
              def char_fcont(v):
                global pos, text
                pos -= 1
                print 'char fcont', pos, text[pos]
                return fc(False)
              self.fcont = char_fcont
              char_fcont.pos = pos              
              print text[pos]
              pos += 1
              return cont(True)
            else:
              return fc(False)
          elif isinstance(c, Var):
            def char_fcont(v):
              global pos, text
              pos -= 1
              print 'char fcont', pos, text[pos]
              try: del bindings[c]
              except: pass
              return fc(False)
            self.fcont = char_fcont
            char_fcont.pos = pos
            bindings[c] = text[pos]
            pos += 1
            print 'succeed.'
            return cont(True)
          else: raise TypeError(c)
      
      elif exp[0]==findall: 
        print findall
        def findall_next(v):
          print 'findall next'
          fc = self.fcont
          self.fcont = findall_done
          return fc(v)
        def findall_done(v):
          print 'findall done'
          return cont(v)
        return self.cps(exp[1], findall_next)
  
      elif exp[0]==lazy_any: #lazy any
        def lazy_any_cont(v):
            self.fcont = lazy_any_cont
            self.cps(exp[1], cont)
        def new_cont(v):
          self.fcont = lazy_any_cont
          return cont(v)
        return new_cont(None)
         
      #elif exp[0]==lazy_any: # lazy any, same as above, but use lambda to simplify
        #def lazy_any_cont(v, fc):
          #return self.cps(exp[1], lambda v, fc:cont(v, lazy_any_cont))(lambda v, fc:cont(v, fc))(v, fc)
        #return cont(None, lazy_any_cont)
      
      #elif exp[0]==any: # nongreedy any
        #print any
        #def any_cont(v, fc):
          #def fcont(v, fc2):
            #print 'any fail'
            #return cont(v, fc)
          #return self.cps(exp[1], any_cont)(v, fcont)
        #return any_cont
      
      elif exp[0]==any: # nongreedy any, same as above, but use lambda to simplify
        print any
        def any_cont(v, fc):
          return self.cps(exp[1], any_cont)(lambda v, fc2: cont(v, fc))
        return any_cont
      
      elif exp[0]==greedy_any: # greedy any
        print greedy_any
        def greedy_any_cont(v, fc):
            return self.cps(exp[1], greedy_any_cont)(v, cont(v, fc))
        return greedy_any_cont

      