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

def done(fc):
  def done_fun(v):
    global finished
    finished = True
    print 'done.'
    fc
    return v
  return done_fun

def done_all(fc):
  def done_fun(v):
    print 'done.'
    return fc(lambda v2: v)
  return done_fun

def end(fc):
  def end_fun(v):
    global finished
    finished = True
    print 'end.'
    return None
  return end_fun

def solve(exp, done=done, end=end):
  global finished
  finished = False  
  cont = cps(exp, done)
  cont = cont(end)#(None)
  while finished == False:
    cont = cont(done)
  return cont

def solve_all(exp):
  return solve(exp, done_all, end)

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
      def atom_fun(v):
        print exp
        return cont(fc)(exp)
      return atom_fun
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
          if(v): return cps_exps(exp[2], cont)(fc)(v)
          else: return cps_exps(exp[3], cont)(fc)(v)
      return cps(exp[1], if_cont)
    
    elif exp[0]==print_:
      def print_cont(fc):
        def print_fun(v):
          print ','.join([str(x) for x in exp[1:]])
          return cont(fc)(v)
        return print_fun
      return print_cont
    
    elif exp[0]==succeed:
      #def succeed_cont(fc):
        #def succeed_fun(v):
          #return cont(fc)(v)
        #return succeed_fun
      #return succeed_cont
      return cont
    
    elif exp[0]==fail: #lambda fc: fc
      #def fail_cont(fc):
        #def fail_fun(v):
          #return fc(fc)(v)
        #return fail_fun
      #return fail_cont
      return lambda fc: fc(fc)

    elif exp[0]==not_:
      #def not_cont(fc):
        #result = cps(exp[1], fc)
        #return result(cont)
      #return not_cont
      return lambda fc: cps(exp[1], fc)(cont)
     
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
        #def fun(v):
          print eoi, pos,
          if pos==len(text):
            print 'succeed.'
            return cont(fc)#(v)
          print text[pos], 'failed'
          return fc#(v)
        #return fun
      return eoi_cont
    
    elif exp[0]==char:
      def char_cont(fc):
        #def fun(v):
          global pos, text
          print char, pos, 
          if pos==len(text):
            print 'failed'
            return fc#(v)
          else:
            print text[pos],
            c = deref(exp[1])
            if isinstance(c, str):
              if c==text[pos]:
                def char_fcont(fc2):
                  global pos, text
                  pos -= 1
                  print 'char fcont', pos, text[pos]
                  return fc#(v)
                char_fcont.pos = pos
                print text[pos]
                pos += 1
                return cont(char_fcont)#(v)
              else:
                return fc#(v)
            elif isinstance(c, Var):
              def char_fcont(fc2):
                global pos, text
                pos -= 1
                print 'char fcont', pos, text[pos]
                try: del bindings[c]
                except: pass
                return fc#(v)
              char_fcont.pos = pos
              bindings[c] = text[pos]
              pos += 1
              print 'succeed.'
              return cont(char_fcont)#(v) 
            else: raise TypeError(c)
        #return fun
      return char_cont
    
    elif exp[0]==findall: 
      print findall
      def findall_cont(fc):
        def findall_next(fc2):
          def findall_next_fun(v):
            print 'findall next'
            fc
            return fc2(v)
          return findall_next_fun
        def findall_done(fc2):
          def findall_done_fun(v):
            print 'findall done'
            return cont(fc)(v) 
          return findall_done_fun
        result = cps(exp[1], findall_next)
        return result(findall_done)
      return findall_cont

    elif exp[0]==lazy_any: #lazy any
      def lazy_any_cont(fc):
        #def fun(v):
          result = cps(exp[1], new_cont)
          result = result(new_fcont)
          return result(fc)#(v)
        #return fun
      def new_fcont(fc):
        #def fun(v):
          return cont(fc)#(v)
        #return fun
      def new_cont(fc):
        #def fun(v):
          return cont(lazy_any_cont)
        #return fun
      return cont(lazy_any_cont)
    
    #elif exp[0]==lazy_any: # lazy any, same as above, but use lambda to simplify
      #def lazy_any_cont(fc):
        #return cps(exp[1], lambda fc:cont(lazy_any_cont))(lambda fc:cont(fc))(fc)
      #return cont(lazy_any_cont)
    
    #elif exp[0]==any: # nongreedy any
      #print any
      #def any_cont(fc):
        #def fcont(fc2):
          #print 'any fail'
          #return cont(fc)
        #return cps(exp[1], any_cont)(fcont)
      #return any_cont
    
    elif exp[0]==any: # nongreedy any, same as above, but use lambda to simplify
      print any
      def any_cont(fc):
        #def fun(v):
          return cps(exp[1], any_cont)(lambda fc2: cont(fc))#(v)
        #return fun
      return any_cont
    
    elif exp[0]==greedy_any: # greedy any
      print greedy_any
      def greedy_any_cont(fc):
        #def fun(v):
          return cps(exp[1], greedy_any_cont)(cont(fc))#(v)
        #return fun
      return greedy_any_cont
      
    