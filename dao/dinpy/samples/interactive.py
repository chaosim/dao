from dao.dinpy import *

import random

#print fun.f()

#print f()

fun(1)[2], c<<fun(1)[True]

print letr( a << fun(x) [ b(x)&c(x) ]
                        [ d(x) ],
            b << fun(1) [True]
                    (4) [True],
            c << fun(4) [True], 
            d << fun(3) [True], 
           ).do[ 
           a(x), x ]

print letr( a << fun(x) [ b(x)&cut&c(x) ]
                        [ d(x) ],
            b << fun(1) [True]
                    (4) [True],
            c << fun(4) [True], 
            d << fun(3) [True], 
           ).do[ 
           a(x), x ]


#print letr( a << fun(x) [ b(x)&cut&c(x) ],
            #b << fun(1) [True]
                    #(2) [True]
                    #(3) [True],
            #c << fun(2) [True] 
          #).do[ 
          #a(x), x ]

#print letr( a << fun(x) [ b(x)&cut&c(x) ],
            #b << fun(1) [True]
                    #(2) [True]
                    #(3) [True],
            #c << fun(1) [True] 
           #).do[ 
           #a(x), x ]
result = solve(do[ repeat, i << pycall(random.randint, 0,10), prin(i), iff(i!=3).do [fail] ])

print result.next()

print result.next()

print do[ repeat, i << pycall(random.randint, 0,10), prin(i), iff(i!=3).do [fail] ] #

#print findall(do[ repeat, i << pycall(random.randint, 0,10), prin(i)], x, y) #

print i << 0

print label.outer%loop[
  println('outer loop'),
  label.inner%loop[
    prin('inner loop: '),
    println(i),
    ++i,
    iff(i==3).do[ exit*2 >> 'exit from inner' ]
  ]
]

print i << 0

print label.outer%loop[
  println('outer loop'),
  label.inner%loop[
    prin('inner loop: '),
    println(i),
    ++i,
    iff(i==3).do[ exit.outer >> 'exit from inner' ]
  ]
]

print letr (f2 << fun(x)[ iff(x<1).do[x].els[f2(x-1)]]) .do [f2(3)]
print i<<0
print lshift(i, 1)
print quote(i)
print quote(i/i+i*1)
print quote((i+i)*(i+1))
print let(i<<1).do[prin(i)]
print let(i<<1).do[let(i<<2).do[ prin(i) ], prin(i)]
print fun.f1(x)[x+x]
print f1(2)
print 'loop times:', loop(3)[ prin(i), ++i ]
print 'loop unitl:', loop [ prin(i), --i]. until(i==0)
print 'loop when:', loop [ prin(i), ++i].when(i<3)
print 'when loop:', when(i!=0).loop[ prin(i), --i]
print 'loop:', loop [ prin(i), ++i, iff(i==3) .do[exit] ]
print 'each:', each(i)[0:3].loop[prin(i)]
print fun.f()[prin('f():'), prin(i+i)]
print f()