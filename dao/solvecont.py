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

def done(v, fc):
  global finished
  finished = True
  print 'Succeed done!'
  return v, fc

def done_all(v, fc):
  print 'Succeed done!'
  return fc(v, end)

def end(v, fc):
  global finished
  finished = True
  print 'Failed at last!'
  return None, None

def solve(exp, done=done, end=end):
  global finished
  finished = False 
  result = cps(exp, done)
  result = result(None, end)
  v, cont = result
  while not finished:
    v, cond = cont(v, end)
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
    def atom_cont(v, fc):
      print exp
      return cont(exp, fc)
    return atom_cont
  
  elif isinstance(exp, Var): 
    def var_cont(v, fc):      
      return cont(exp.deref(bindings), fc)
    return var_cont
  
  elif isinstance(exp, list) or isinstance(exp, tuple):
    
    if exp[0]==quote: 
      def quote_cont(v, fc): 
        return cont(exp[1], fc)
      return quote_cont
    
    if exp[0]==begin: # and is equal to begin
      return cps_exps(exp[1:], cont)
    
    if exp[0]==if_: # (if x y z)
      def if_cont(v, fc):
        if(v): return cps(exp[2], cont)(v, fc)
        else: return cps(exp[3], cont)(v, fc)
      return cps(exp[1], if_cont)
    
    elif exp[0]==print_:
      def print_cont(v, fc):
        print ','.join([str(x) for x in exp[1:]])
        return cont(v, fc)
      return print_cont
    
    elif exp[0]==succeed:
      #def succeed_cont(v, fc):
        #return cont(v, fc)
      #return succeed_cont
      return cont
    
    elif exp[0]==fail: #lambda fc: fc
      #def fail_cont(v, fc):
        #def fail_fun(v):
          #return v, fc
        #return fail_fun
      #return fail_cont
      return lambda v, fc: (v, fc)

    elif exp[0]==not_:
      #def not_cont(v, fc):
        #return cps(exp[1], fc)(None, cont)
      #return not_cont
      return lambda v, fc: cps(exp[1], fc)(None, cont)
     
    if exp[0]==and_: # and is equal to begin
      return cps(exp[1], cps(exp[2], cont))
    
    elif exp[0]==or_:
      if len(exp)==1: 
        return (), cont
      elif len(exp)==2: 
        return cps(exp[1], cont)
      elif len(exp)==3:  
        return cps(exp[1], lambda v, fc: cont(v, cps(exp[2], lambda v, fc2: cont(v, fc2))))
      else: 
        return cps(exp[1], lambda v, fc: cont(v, cps((or_,)+tuple(exp[2:]), lambda v, fc2: cont(v, fc2))))
    
    elif exp[0]==unify: #(unify, x, y)
      def unify_cont(v, fc):
        x, y = exp[1:]
        if isinstance(x, Var):
          x = deref(x)
          if isinstance(x, Var):
            bindings[x] = y
            def unify_fcont(v, fc2):
              del bindings[x]
              return v, fc
            return cont(v, unify_fcont)
          else:
            y = deref(y)
            if isinstance(y, Var):
              bindings[y] = x
              def unify_fcont(v, fc2):
                try: del bindings[y]
                except: pass
                return v, fc
              return cont(v, unify_fcont)
        elif isinstance(y, Var):
          y = deref(y)
          if isinstance(y, Var):
            bindings[y] = x
            def new_fcont(v, fc2):
              try: del bindings[y]
              except: pass
              return v, fc
            return cont(v, unify_fcont)
        if x==y: return cont(v, fc)
        else: return v, fc
      return unify_cont
    
    elif exp[0]==eoi:
      def eoi_cont(v, fc):
        print eoi, pos,
        if pos==len(text):
          print 'succeed.'
          return cont(v, fc)
        print text[pos], 'failed'
        return v, fc
      return eoi_cont
    
    elif exp[0]==char:
      def char_cont(v, fc):
        global pos, text
        print char, pos, 
        if pos==len(text):
          print 'failed'
          return v, fc
        else:
          print text[pos],
          c = deref(exp[1])
          if isinstance(c, str):
            if c==text[pos]:
              def char_fcont(v, fc2):
                global pos, text
                pos -= 1
                print 'char fcont', pos, text[pos]
                return v, fc
              char_fcont.pos = pos
              print text[pos]
              pos += 1
              return cont(v, char_fcont)
            else:
              return fc2(v, fc)
          elif isinstance(c, Var):
            def char_fcont(v, fc2):
              global pos, text
              pos -= 1
              print 'char fcont', pos, text[pos]
              try: del bindings[c]
              except: pass
              return fc2(v, fc)
            char_fcont.pos = pos
            bindings[c] = text[pos]
            pos += 1
            print 'succeed.'
            return cont(v, char_fcont)
          else: raise TypeError(c)
      return char_cont
    
    elif exp[0]==findall: 
      print findall
      def findall_cont(v, fc):
        def findall_next(v, fc2):
          print 'findall next'
          fc #for debug
          return fc2(v, fc)
        def findall_done(v, fc2):
          print 'findall done'
          return cont(v, fc)
        result = cps(exp[1], findall_next)
        return result(v, findall_done)
      return findall_cont

    elif exp[0]==lazy_any: #lazy any
      def lazy_any_cont(v, fc):
          result = cps(exp[1], new_cont)
          result = result(v, new_fcont)
          return result[1](v, fc)
      def new_fcont(v, fc):
        return cont(v, fc)
      def new_cont(v, fc):
        return cont(v, lazy_any_cont)
      return new_cont
       
    #elif exp[0]==lazy_any: # lazy any, same as above, but use lambda to simplify
      #def lazy_any_cont(v, fc):
        #return cps(exp[1], lambda v, fc:cont(v, lazy_any_cont))(lambda v, fc:cont(v, fc))(v, fc)
      #return cont(None, lazy_any_cont)
    
    #elif exp[0]==any: # nongreedy any
      #print any
      #def any_cont(v, fc):
        #def fcont(v, fc2):
          #print 'any fail'
          #return cont(v, fc)
        #return cps(exp[1], any_cont)(v, fcont)
      #return any_cont
    
    elif exp[0]==any: # nongreedy any, same as above, but use lambda to simplify
      print any
      def any_cont(v, fc):
        return cps(exp[1], any_cont)(lambda v, fc2: cont(v, fc))
      return any_cont
    
    elif exp[0]==greedy_any: # greedy any
      print greedy_any
      def greedy_any_cont(v, fc):
          return cps(exp[1], greedy_any_cont)(v, cont(v, fc))
      return greedy_any_cont