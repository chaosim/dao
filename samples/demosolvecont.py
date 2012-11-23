from solvecont import *

import solvecont

x = Var('x')
_ = DummyVar('_')

def demo1():
  print '===================================='
  print succeed
  solve([succeed])
  
  print '===================================='
  print [fail]
  solve([fail])
  
  print '===================================='
  print (not_, 1)
  solve((not_, 1))
  
  print '===================================='
  print (not_, [succeed])
  solve((not_, [succeed]))
  
  print '===================================='
  print (not_, [fail])
  solve((not_, [fail]))
  
  print '===================================='
  print (or_, 1, 2)
  solve((or_, 1, 2))
  
  print '===================================='
  print 'solve_all:', (or_, 1, 2)
  solve_all((or_, 1, 2))

demo1()

def demo():
  print '===================================='
  print (or_, 1, 2, 3)
  solve((or_, 1, 2, 3))
  
  print '===================================='
  print (or_, (print_, 1), (print_, 2))
  solve((or_, (print_, 1), (print_, 2)))
  
  print '===================================='
  print 'solve_all:', (or_, 1, 2, 3)
  solve_all((or_, 1, 2, 3))
  
  print '===================================='
  print (findall, (or_, 1))
  solve((findall, (or_, 1)))
  
  print '===================================='
  print (findall, (or_, 1, 2))
  solve((findall, (or_, 1, 2)))
  
  print '===================================='
  print (findall, (or_, 1, 2, 3))
  solve((findall, (or_, 1, 2, 3)))
  
  print '===================================='
  print (findall, (or_, 1, 2, 3, 4))
  solve((findall, (or_, 1, 2, 3, 4)))
  
  print '===================================='
  print (begin, (print_, 1), (print_, 2))
  solve((begin, (print_, 1), (print_, 2)))
 
  print '===================================='
  print (not_,(begin, (print_, 1), (print_, 2)))
  solve((not_,(begin, (print_, 1), (print_, 2))))  

  print '===================================='
  print (begin, (findall, (or_, (print_, 1), (print_, 2))), (print_, 3))
  solve((begin, (findall, (or_, (print_, 1), (print_, 2))), (print_, 3)))
  
demo()
 
def demo_unify():  
  print '===================================='
  print (unify, 1, 2)
  solve((unify, 1, 2))
  print
  print (unify, 1, 1)
  solve((unify, 1, 1))
  print
  print (begin, (unify, _, 1), (unify, _, 2))
  solve((begin, (unify, _, 1), (unify, _, 2)))
  print
  print (begin, (unify, x, 1))
  solve((begin, (unify, x, 1)))
  print
  print (begin, (unify, x, 1), (unify, x, 2))
  solve((begin, (unify, x, 1), (unify, x, 2)))
  print

demo_unify()

def demo_if():  
  print '===================================='
  print (if_, 1, (print_, 2), (print_, 3))
  solve((if_, 1, (print_, 2), (print_, 3)))

  print '===================================='
  print (if_, 0, (print_, 2), (print_, 3))
  solve((if_, 0, (print_, 2), (print_, 3)))

demo_if()

def demo_lazy_any():
  print 'demo_lazy_any'
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (lazy_any, [char, _]), [eoi])
  solve((begin, (set_text, 'abcde'), (lazy_any, [char, _]), [eoi]))
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (findall, (lazy_any, [char, _])))
  solve((begin, (set_text, 'abcde'), (findall, (lazy_any, [char, _]))))
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (lazy_any, [char, _]), [char, _], [char, _])
  solve((begin, (set_text, 'abcde'), (lazy_any, [char, _]), [char, _], [char, _]))
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (lazy_any, [char, _]), [char, _], [char, _], [eoi])
  solve((begin, (set_text, 'abcde'), (lazy_any, [char, _]), [char, _], [char, _], [eoi]))


  print '===================================='
  print (findall, (begin, (set_text, 'abcde'), (lazy_any, [char, _]), [char, _], [char, _]))
  solve((findall, (begin, (set_text, 'abcde'), (lazy_any, [char, _]), [char, _], [char, _])))
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (lazy_any, [char, _]), [eoi])
  solve((begin, (set_text, 'abcde'), (lazy_any, [char, _]), [eoi]))


  print '===================================='
  print (begin, (set_text, 'abcde'), (lazy_any, [char, _]), [char, _], [char, _], [eoi])
  solve((begin, (set_text, 'abcde'), (lazy_any, [char, _]), [char, _], [char, _], [eoi]))
  
  print '===================================='
  print (findall, (begin, (set_text, 'abcde'), (lazy_any, [char, _]), [char, _], [char, _], [eoi]))
  solve((findall, (begin, (set_text, 'abcde'), (lazy_any, [char, _]), [char, _], [char, _], [eoi])))

demo_lazy_any()

def demo_any():
  print 'demo_any'
    
  print '===================================='
  print (begin, (set_text, 'abcde'), (any, [char, _]), [eoi])
  solve((begin, (set_text, 'abcde'), (any, [char, _]), [eoi]))
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (findall, (any, [char, _])))
  solve((begin, (set_text, 'abcde'), (findall, (any, [char, _]))))
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (any, [char, _]), [char, _], [char, _])
  solve((begin, (set_text, 'abcde'), (any, [char, _]), [char, _], [char, _]))
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (any, [char, _]), [char, _], [char, _], [eoi])
  solve((begin, (set_text, 'abcde'), (any, [char, _]), [char, _], [char, _], [eoi]))


  print '===================================='
  print (findall, (begin, (set_text, 'abcde'), (any, [char, _]), [char, _], [char, _]))
  solve((findall, (begin, (set_text, 'abcde'), (any, [char, _]), [char, _], [char, _])))
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (any, [char, _]), [eoi])
  solve((begin, (set_text, 'abcde'), (any, [char, _]), [eoi]))


  print '===================================='
  print (begin, (set_text, 'abcde'), (any, [char, _]), [char, _], [char, _], [eoi])
  solve((begin, (set_text, 'abcde'), (any, [char, _]), [char, _], [char, _], [eoi]))
  
  print '===================================='
  print (findall, (begin, (set_text, 'abcde'), (any, [char, _]), [char, _], [char, _], [eoi]))
  solve((findall, (begin, (set_text, 'abcde'), (any, [char, _]), [char, _], [char, _], [eoi])))

demo_any()

def demo_greedy_any():
  print 'demo_greedy_any'
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (greedy_any, [char, _]), [eoi])
  solve((begin, (set_text, 'abcde'), (greedy_any, [char, _]), [eoi]))
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (findall, (greedy_any, [char, _])))
  solve((begin, (set_text, 'abcde'), (findall, (greedy_any, [char, _]))))
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (greedy_any, [char, _]), [char, _], [char, _])
  solve((begin, (set_text, 'abcde'), (greedy_any, [char, _]), [char, _], [char, _]))
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (greedy_any, [char, _]), [char, _], [char, _], [eoi])
  solve((begin, (set_text, 'abcde'), (greedy_any, [char, _]), [char, _], [char, _], [eoi]))


  print '===================================='
  print (findall, (begin, (set_text, 'abcde'), (greedy_any, [char, _]), [char, _], [char, _]))
  solve((findall, (begin, (set_text, 'abcde'), (greedy_any, [char, _]), [char, _], [char, _])))
  
  print '===================================='
  print (begin, (set_text, 'abcde'), (greedy_any, [char, _]), [eoi])
  solve((begin, (set_text, 'abcde'), (greedy_any, [char, _]), [eoi]))


  print '===================================='
  print (begin, (set_text, 'abcde'), (greedy_any, [char, _]), [char, _], [char, _], [eoi])
  solve((begin, (set_text, 'abcde'), (greedy_any, [char, _]), [char, _], [char, _], [eoi]))
  
  print '===================================='
  print (findall, (begin, (set_text, 'abcde'), (greedy_any, [char, _]), [char, _], [char, _], [eoi]))
  solve((findall, (begin, (set_text, 'abcde'), (greedy_any, [char, _]), [char, _], [char, _], [eoi])))

demo_greedy_any()

