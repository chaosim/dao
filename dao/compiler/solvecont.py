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
  return solve_cont(cps(done, end, exp))

def solve_cont(cont):
  value, fcont = None, end
  while cont is not None:
    value, cont = cont(value, fcont)
    fcont = cont

def cps_exps(cont, fcont, exp):
  if len(exp)==0: return cont((), fcont)
  elif len(exp)==1: return cps(cont, fcont, exp[0])
  else:
    return cps(
              cps_exps(cont, fcont, exp[1:]), fcont, exp[0])

def done(v, fc):
  print 'done'
  return v, None

def end(v, fc):
  print 'failed'
  return v, None

(begin, print_, succeed, fail, char, eoi, any, lazy_any, greedy_any, or_, findall,
 not_, unify) = (
  'begin', 'print', 'succeed', 'fail', 'char', 'eoi', 'any', 'lazy any', 'greedy any', 'or', 'findall',
 "not", "unify")

def cps(cont, fcont, exp):
  if isinstance(exp, int) or isinstance(exp, str):
    return lambda v, fc: cont(exp, fcont)
  
  elif isinstance(exp, Var): 
    return exp.deref(bindings)
  
  elif isinstance(exp, list) or isinstance(exp, tuple):
    
    if exp[0]==begin:
      return cps_exps(cont, fcont, exp[1:])
    
    elif exp[0]==print_:
      def prin_cont(v, fc):
        print ','.join([str(x) for x in exp[1:]])
        return cont(v, fcont)
      return prin_cont
    
    elif exp[0]==succeed:
      return cont
    
    elif exp[0]==fail:
      return fcont

    elif exp[0]==not_:
      return cps(fcont, cont, exp[1])
    
    elif exp[0]==or_:
      return cps(cont, cps(cont, fcont, exp[2]), exp[1])
    
    elif exp[0]==eoi:
      def eoi_cont(v, fc):
        print eoi
        if pos==len(text):
          return cont(None, fc)
        return v, fc
      return eoi_cont
    
    elif exp[0]==unify:
      return unify_(exp[1], exp[2], cont, fcont)
    
    elif exp[0]==char:
      def char_cont(v, fc):
        global pos, text
        print char, pos, 
        if pos==len(text):
          return None, fc # tested ok in any
          #return fcont(None, fc) # tests failed in "any"
        else:
          c = deref(exp[1])
          if isinstance(c, str):
            if c==text[pos]:
              def char_fcont(v2, fc2):
                global pos, text
                pos -= 1
                print 'char fail', pos, text[pos]
                fc, fcont
                return None, fc # tested ok in any
              # return fcont(None, fc) # tests failed in "any"
              print text[pos]
              pos += 1
              return cont(None, char_fcont)
            else:
              return None, fc
          elif isinstance(c, Var):
            def char_fcont(v2, fc2):
              global pos, text
              fc, fcont
              pos -= 1
              print 'char fail', pos, text[pos]
              try: del bindings[c]
              except: pass
              return None, fc
            bindings[c] = text[pos]
            pos += 1
            return cont(None, char_fcont) # tested ok in any
          else: raise TypeError(c)
      return char_cont
    
    elif exp[0]==findall: # It works!
      def findall_cont(v, fc):
        found = [False]
        print findall
        def findall_next(v, fc):
          print 'findall next'
          #if fc is findall_next: 
          #if found[0]: 
            #return cont(v, fcont) # necessary if greedy any, unnecessary if lazy any
          #else: 
            #return fc(v, fcont)
          return fc(v, findall_done)
        def findall_done(v, fc):
          print 'findall done'
          found[0] = True
          return cont(v, fcont)
        return cps(findall_next, findall_done, exp[1])(v, findall_done)
      return findall_cont
    
    elif exp[0]==greedy_any: # greedy any, correct
      def new_cont(v, fc):
        def new_fcont(v, fc):
          return fcont(v, fc)
        return cont(v, new_fcont)
      def greedy_any_cont(v, fc):
        return cps(greedy_any_cont, fc, exp[1])(v, new_cont)
      return greedy_any_cont
      
    elif exp[0]==lazy_any: # lazy any, correct
      def lazy_any_cont(v, fc):
        def any_fcont(v, fc):
          def any_cont(v, fc):
            return cont(v, any_fcont)
          return cps(any_cont, any_fcont, exp[1])(v, fc)
        return cont(v, any_fcont)
      def fun(v, fc):
        return lazy_any_cont(v, fc)
      return fun
      #return fun
    
    elif exp[0]==any: # nongreedy lazy, wrong.
      def any_cont(v, fc):
        print any
        def any_fcont(v, fc2):
          print 'any fail'
          return cont(v, fc)
        return cps(any_cont, fcont, exp[1])(v, any_fcont)
      return any_cont
    

def demo():
  # demo
  global bindings, text, pos
  
  x = Var('x')
  _ = DummyVar('_')
  
  bindings = Bindings()
  
  solve((not_, [succeed]))
  
  solve((not_, [fail]))
  
  print '===================================='
  print (findall, (any, [char, _]))
  pos, text = 0, 'abcdef'
  solve((findall, (any, [char, _])))
  
  print '===================================='
  print (begin, (any, [char, _]), [eoi])
  pos, text = 0, 'abcdef'
  solve((begin, (any, [char, _]), [eoi]))
  
  
  print '===================================='
  print (begin, (any, [char, _]), [char, _], [char, _], [eoi])
  pos, text = 0, 'abcdef'
  solve((begin, (any, [char, _]), [char, _], [char, _], [eoi]))
  
  print '===================================='
  print (findall, (greedy_any, [char, _]))
  pos, text = 0, 'abcdef'
  solve((findall, (greedy_any, [char, _])))
  
  print '===================================='
  print (findall, (lazy_any, [char, _]))
  pos, text = 0, 'abcdef'
  solve((findall, (lazy_any, [char, _])))
  
  print '===================================='
  print (begin, (lazy_any, [char, _]), [char, _], [char, _], [eoi])
  pos, text = 0, 'abcdef'
  solve((begin, (lazy_any, [char, _]), [char, _], [char, _], [eoi]))
  
  print '===================================='
  print (begin, (findall, (or_, (print_, 1), (print_, 2))), (print_, 3))
  solve((begin, (findall, (or_, (print_, 1), (print_, 2))), (print_, 3)))
  
  print '===================================='
  print (or_, (print_, 1), (print_, 2))
  solve((or_, (print_, 1), (print_, 2)))
  
  print '===================================='
  print (unify, 1, 2)
  solve((unify, 1, 2))
  print (unify, 1, 1)
  solve((unify, 1, 1))
  print (begin, (unify, _, 1), (unify, _, 2))
  solve((begin, (unify, _, 1), (unify, _, 2)))
  print (begin, (unify, x, 1))
  solve((begin, (unify, x, 1)))
  print 'unify in dirty bindings:', (begin, (unify, x, 2))
  solve((begin, (unify, x, 2)))
  bindings = Bindings()
  print 'after cleaning bindings:', (begin, (unify, x, 2))
  solve((begin, (unify, x, 2)))
  print (begin, (unify, x, 1), (unify, x, 2))
  solve((begin, (unify, x, 1), (unify, x, 2)))

if __name__=="__main__":
  demo()