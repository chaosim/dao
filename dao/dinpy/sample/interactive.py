from dao.dinpy import *

print i<<0
print quote(i)
print quote(i/i+i*1)
print quote((i+i)*(i+1))
print 'loop times: ', loop(3)[ prin(i), ++i ]
print 'do unitl: ', do [ prin(i), --i]. until(i==0)
print 'do when: ', do [ prin(i), ++i].when(i<3)
print 'when do: ', when(i!=0).do[ prin(i), --i]
print 'loop: ', loop [ prin(i), ++i, iff(i==3) [exit] ]
print fun.f()==[prin('f():'), prin(i+i)]
print f()