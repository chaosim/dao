# doctest

'''
>>> from dao.dinpy import *

>>> quote(i)
i

>>> quote(i/i+i*1)
i/i+i*1

>>> quote((i+i)*(i+1))
(i+i)*(i+1)

>>> i<<0
0
>>> lshift(i, 1)
0

>>> let(i<<1).do[prin(i)]
1
>>> let(i<<1).do[let(i<<2).do[ prin(i) ], prin(i)]
2 1

>>> loop(3)[ prin(i), ++i ]
0 1 2
>>> loop [ prin(i), --i]. until(i==0)
3 2 1
>>> loop [ prin(i), ++i].when(i<3)
0 1 2
>>> when(i!=0).loop[ prin(i), --i]
3 2 1
>>> loop [ prin(i), ++i, iff(i==3) .do[exit] ]
0 1 2
>>> each(i)[0:3].loop[prin(i)]
0 1 2
>>> i << 0
0
>>> label.outer%loop[ println('outer loop'), label.inner%loop[ prin('inner loop: '), println(i), ++i, iff(i==3).do[ exit*2 >> 'exit from inner' ] ] ]
outer loop
inner loop:  0
inner loop:  1
inner loop:  2
'exit from inner'
>>> i << 0
0
>>> label.outer%loop[ println('outer loop'), label.inner%loop[ prin('inner loop: '), println(i), ++i, iff(i==3).do[ exit.outer >> 'exit from inner' ] ] ]
outer loop
inner loop:  0
inner loop:  1
inner loop:  2
'exit from inner'

#>>> fun.f1(x)[x+x]
#f1
#>>> f1(2)
#4

#>>> letr (f2 << fun(x)[ iff(x<1).do[x].els[f2(x-1)]]) .do [f2(3)]
#0

#>>> fun.f()[prin('f():'), prin(4)]
#f
#>>> f()
#f(): 4
'''

if __name__ == "__main__":
  import doctest
  doctest.testmod()
