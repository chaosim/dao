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
    
    def unify(self, other, cont, fcont, occurs_check=False):
        self = self.deref(bindings)
        other = deref(other, bindings)
        if isinstance(self, Var):
          if occurs_check and contain_var(other, self): return
          elif isinstance(other, Var) and other is self: 
            yield True
            return
          else:
            bindings[self] = other
            yield True
            try: del bindings[self] # DummyVar oops
            except: pass
        elif isinstance(other, Var):
          if occurs_check and contain_var(self, other): return
          elif isinstance(self, Var) and other is self: 
            yield True
            return
          else:
            bindings[other] = self
            yield True
            try: del bindings[other] # DummyVar oops
            except: pass
        else:
          for _ in unify(self, other, bindings, occurs_check):
            yield True 
            
  def deref(self):
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
      
  def deref(self): return self

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

def deref(item):
  try:
    item_deref = item.deref
  except:
    return item
  return item_deref()

def unify_(x, y, cont, fcont):
  if isinstance(x, Var):
    x = deref(x)
    if isinstance(x, Var):
      bindings[x] =y
      def new_fcont(v, fc):
        del bindings[x]
        return v, fc
      return lambda v, fc: cont(True, new_fcont)
  if isinstance(y, Var):
    y = deref(y)
    if isinstance(y, Var):
      def new_fcont(v, fc):
        del bindings[y]
        return v, fc
      return lambda v, fc: cont(True, new_fcont)
  if x==y: return cont
  else: return fcont

def solve(exp):
  cont = cps(exp, done)
  cont2 = cont(end)
  x = 1
  return cont2(None)

def done(fc):
  def done_fun(v):
    print 'done.'
    return v
  return done_fun

def end(fc):
  def end_fun(v):
    print 'end.'
    return None
  return end_fun

def solve_all(exp):
  cont1  = cps(exp, done_all)
  cont2 = cont1(end)
  return cont2(None)

def done_all(fc):
  def done_fun(v):
    print 'done.'
    return fc(lambda v2: v)
  return done_fun

(begin, print_, if_, quote,
 succeed, fail, and_, or_, findall, not_, unify,
 char, eoi, any, lazy_any, greedy_any) = (
  'begin', 'print', 'if', 'quote',
  'succeed', 'fail', 'and', 'or', 'findall', "not", "unify",
 'char', 'eoi', 'any', 'lazy any', 'greedy any')

pos, text = 0, ''
bindings = Bindings()

def cps_exps(exps, cont):
  if len(exps)==0: return lambda fc: cont(fc)(())
  elif len(exps)==1: return cps(exps[0], cont)
  else:
    return cps(exps[0], cps_exps(exps[1:], cont))

def cps(exp, cont):
  global pos, text
  if isinstance(exp, int) or isinstance(exp, str):
    def atom_cont(fc):
      def fun(v):
        print exp
        return cont(fc)(exp)
      return fun
    return atom_cont
  
  elif isinstance(exp, Var): 
    return lambda fc: lambda v: cont(fc)(exp.deref(bindings))
  
  elif isinstance(exp, list) or isinstance(exp, tuple):
    
    if exp[0]==quote: 
      return lambda fc: lambda v: cont(fc)(exp[1])
    
    if exp[0]==begin: # and is equal to begin
      return cps_exps(exp[1:], cont)
    
    if exp[0]==if_: # (if x y z)
      def if_cont(fc):
        def if_fun(v):
          if(v): return cps_exps(exp[2], cont)
          else: return cps_exps(exp[3], cont)
      return cps(exp[1], if_cont)
    
    elif exp[0]==print_:
      def print_cont(fc):
        def fun(v):
          print ','.join([str(x) for x in exp[1:]])
          return cont(fc)(None)
        return fun
      return print_cont
    
    elif exp[0]==succeed:
      return cont
    
    elif exp[0]==fail: #lambda fc: fc
      #def fail_cont(fc):
        #return fc
      #return fail_cont
      return lambda fc: fc(lambda v: v)

    elif exp[0]==not_:
      def not_cont(fc):
        result = cps(exp[1], fc)
        return result(cont)
      return not_cont
      #return lambda fc: cps(exp[1], fc)(cont)
     
    if exp[0]==and_: # and is equal to begin
      return cps(exp[1], cps(exp[2], cont))
    
    elif exp[0]==or_:
      if len(exp)==1: 
        return lambda fc: cont(fc)(())
      elif len(exp)==2: 
        return cps(exp[1], cont)
      elif len(exp)==3:  
        return cps(exp[1], cont(cps(exp[2], cont)))
      else: 
        return cps(exp[1], cont(cps((or_,)+tuple(exp[2:]), cont)))
    
    elif exp[0]==unify:
      return lambda fc: unify_(exp[1], exp[2], cont, fc)
    
    elif exp[0]==eoi:
      def eoi_cont(fc):
        print eoi, pos,
        if pos==len(text):
          print
          return cont(fc)
        print text[pos]
        return fc
      return eoi_cont
    
    elif exp[0]==char:
      def char_cont(fc):
        global pos, text
        print char, pos, 
        if pos==len(text):
          print
          return fc
        else:
          print text[pos]
          c = deref(exp[1])
          if isinstance(c, str):
            if c==text[pos]:
              def char_fcont(fc2):
                global pos, text
                pos -= 1
                print 'char fail', pos, text[pos]
                return fc
              char_fcont.pos = pos
              print text[pos]
              pos += 1
              return cont(char_fcont)
            else:
              return fc
          elif isinstance(c, Var):
            def char_fcont(fc2):
              global pos, text
              pos -= 1
              print 'char fail', pos, text[pos]
              try: del bindings[c]
              except: pass
              return fc
            char_fcont.pos = pos
            bindings[c] = text[pos]
            pos += 1
            return cont(char_fcont) 
          else: raise TypeError(c)
      return char_cont
    
    #elif exp[0]==findall: 
      #print findall
      #def findall_cont(fc):
        #def findall_next(fc2):
          #def doit(v):
            #print 'findall next'
            #return fc2(findall_done)
          #return doit
        #def findall_done(v):
          #print 'findall done'
          #return cont(fc)(True)
        #result = cps(exp[1], findall_next)
        #return result(None)
      #return findall_cont

    #elif exp[0]==findall: 
      #print findall
      #def findall_next(fc):
        #def findall_next_value(fc2):
          #print 'findall next'
          #return fc(findall_done)
        #return findall_next_value
      #def findall_done(fc2):
        #print 'findall done'
        #return cont(None)(True)
      #return cps(exp[1], findall_next)

    elif exp[0]==findall: 
      print findall
      def findall_cont(fc):
        def findall_next(fc2):
          print 'findall next'
          fc
          return fc2
        def findall_done(fc2):
          print 'findall done'
          return cont(fc)(True)
        result = cps(exp[1], findall_next)
        return result(findall_done)
      return findall_cont

    #elif exp[0]==greedy_any: # greedy any, correct
      #print greedy_any
      #def greedy_any_cont(fc):
        #def new_cont(v):
          #print 'new_cont'
          #return cont(fc)(v)
        #def recursive_cont(fc):
          #def fun(v):
            #return cps(exp[1], recursive_cont(fc))(new_cont)
          #return fun
        ##return cont(fc)(True)
        #return recursive_cont(new_cont)
      #return greedy_any_cont
      
    elif exp[0]==greedy_any: # greedy any, correct
      print greedy_any
      def greedy_any_cont(fc):
        def fun(fc2):
          return cps(exp[1], greedy_any_cont(safe_cont))(safe_cont)
        return fun
      def safe_cont(fc):
        return cont(fc)(None)
      return greedy_any_cont(safe_cont)
      
    elif exp[0]==lazy_any: #lazy any, correct
      def lazy_any_cont(fc):
        result = cps(exp[1], new_cont)
        result = result(new_fcont)
        return result(fc)
      def new_fcont(fc):
        return cont(fc)
      def new_cont(fc):
        return cont(lazy_any_cont)
      return cont(lazy_any_cont)
    
    #elif exp[0]==lazy_any: # lazy any
      #def lazy_any_cont(fc):
        #return cps(exp[1], lambda fc:cont(lazy_any_cont))(lambda fc:cont(fc))(fc)
      #return cont(lazy_any_cont)
    
    elif exp[0]==any: # nongreedy lazy, wrong.
      def any_cont(v, fc):
        print any
        def fcont(v, fc2):
          print 'any fail'
          return cont(v, fc)
        return cont(v, fcont)
        return cps(exp[1], any_cont)
      return any_cont