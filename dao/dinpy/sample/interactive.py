from dao.dinpy import *

print letr (f2 << fun(x)[ iff(x<1).do[x].els[f2(x-1)]]) .do [f2(3)]
print i<<0
print lshift(i, 1)
print quote(i)
print quote(i/i+i*1)
print quote((i+i)*(i+1))
print let(i<<1).do[prin(i)]
print let(i<<1).do[let(i<<2).do[ prin(i) ], prin(i)]
print fun.f1(x) == [x+x]
print f1(2)
print 'loop times:', loop(3)[ prin(i), ++i ]
print 'loop unitl:', loop [ prin(i), --i]. until(i==0)
print 'loop when:', loop [ prin(i), ++i].when(i<3)
print 'when loop:', when(i!=0).loop[ prin(i), --i]
print 'loop:', loop [ prin(i), ++i, iff(i==3) .do[exit] ]
print 'each:', each(i)[0:3].loop[prin(i)]
print fun.f()==[prin('f():'), prin(i+i)]
print f()