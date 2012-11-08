from solvecont import *

import solvecont

x = Var('x')
_ = DummyVar('_')

def demo():
  solve([succeed])
  
  solve([fail])
  
  solve((not_, 1))
  
  solve((not_, [succeed]))
  
  solve((not_, [fail]))
  
  solve((or_, 1, 2))
  
  solve_all((or_, 1, 2))

  solve((or_, 1, 2, 3))
  
  solve_all((or_, 1, 2, 3))
  
  solve((findall, (or_, 1)))
  
  solve((findall, (or_, 1, 2)))
  
  solve((findall, (or_, 1, 2, 3)))
  
  solve((begin, (print_, 1), (print_, 2)))
 
  solve((not_,(begin, (print_, 1), (print_, 2))))  

  print '===================================='
  print (begin, (findall, (or_, (print_, 1), (print_, 2))), (print_, 3))
  solve((begin, (findall, (or_, (print_, 1), (print_, 2))), (print_, 3)))
  
 #demo()
 
def demo_unify():  
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
  solvecont.bindings = Bindings()
  print 'after cleaning bindings:', (begin, (unify, x, 2))
  solve((begin, (unify, x, 2)))
  print (begin, (unify, x, 1), (unify, x, 2))
  solve((begin, (unify, x, 1), (unify, x, 2)))

def demo_any():
  solvecont.bindings = Bindings()
  
  print '===================================='
  print (findall, (any, [char, _]))
  solvecont.pos, solvecont.text = 0, 'abcdef'
  solve((any, [char, _]))

  print '===================================='
  print (findall, (any, [char, _]))
  solvecont.pos, solvecont.text = 0, 'abcdef'
  solve((findall, (any, [char, _])))
  
  print '===================================='
  print (begin, (any, [char, _]), [eoi])
  solvecont.pos, solvecont.text = 0, 'abcdef'
  solve((begin, (any, [char, _]), [eoi]))
  
  
  print '===================================='
  print (begin, (any, [char, _]), [char, _], [char, _], [eoi])
  solvecont.pos, solvecont.text = 0, 'abcdef'
  solve((begin, (any, [char, _]), [char, _], [char, _], [eoi]))
  
  print '===================================='
  print (findall, (greedy_any, [char, _]))
  solvecont.pos, solvecont.text = 0, 'abcdef'
  solve((findall, (greedy_any, [char, _])))
  
  print '===================================='
  print (greedy_any, [char, _])
  solvecont.pos, solvecont.text = 0, 'abcdef'
  solve((greedy_any, [char, _]))
    
def demo_lazy_any():
  print '===================================='
  print (findall, (lazy_any, [char, _]))
  solvecont.pos, solvecont.text = 0, 'abcdef'
  solve((findall, (lazy_any, [char, _])))
  
  #print '===================================='
  #print (begin, (lazy_any, [char, _]), [char, _], [char, _])
  #solvecont.pos, solvecont.text = 0, 'abcdef'
  #solve((begin, (lazy_any, [char, _]), [char, _], [char, _]))

  #print '===================================='
  #print (findall, (begin, (lazy_any, [char, _]), [char, _], [char, _]))
  #solvecont.pos, solvecont.text = 0, 'abcdef'
  #solve((findall, (begin, (lazy_any, [char, _]), [char, _], [char, _])))
  
  #print '===================================='
  #print (begin, (lazy_any, [char, _]), [eoi])
  #solvecont.pos, solvecont.text = 0, 'abcdef'
  #solve((begin, (lazy_any, [char, _]), [eoi]))


  #print '===================================='
  #print (begin, (lazy_any, [char, _]), [char, _], [char, _], [eoi])
  #solvecont.pos, solvecont.text = 0, 'abcdef'
  #solve((begin, (lazy_any, [char, _]), [char, _], [char, _], [eoi]))

demo_lazy_any()