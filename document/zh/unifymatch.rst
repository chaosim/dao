合一和回溯
***************

**(需要进一步充实内容)**

合一和回溯是prolog用来实现逻辑程序语言的两大主要手段。合一管理程序的数据流，回溯管理程序的控制流。二者密切配合，使得声明式风格的程序得以正常运行。

合一
-----
过程式程序语言主要依靠赋值及函数参数在变量间传递数据，函数式程序语言主要依靠函数调用时的参数绑定在变量间传递数据，而逻辑式程序语言主要依靠合一在变量间传递数据。因此，合一是逻辑程序设计系统的主要机制之一。合一的基本思想如下：

* 合一变量

  一个变量可以和任何对象合一，称作该对象被绑定到此变量。没有绑定到非变量的变量叫做自由变量，自由变量可以多次绑定其它变量，形成一条绑定链。如果绑定链最终绑定了非变量，这个变量就变成了一个非自由变量。非自由变量与其它对象合一是否成功取决于其最终绑定值与该对象的合一。

>>> from dao.dinpy import *
>>> x
x
>>> free(x)
True
>>> unify(x, y)
True
>>> x
y
>>> y
y
>>> unify(x, 1)
True
>>> x
1
>>> y
1
>>> free(x)
False
>>> free(y)
False

* 合一原子对象

  如果二者相等，则合一成功，否则合一失败。

>>> unify(1,2)
False
>>> unify(1, 1)
True

* 合一复合对象

  如果二者所有成员都能分别合一，则合一成功，否则合一失败。对于列表或元组，依次合一所有元素。对于Cons，分别合一其head和tail。

>>> unify((1,2), (1,2))
True
>>> unify((x,2), (0,2))
Traceback (most recent call last):
  File "e:\dao\dao\dinpy\term.py", line 1, in <module>
    from dao.builtins.term import *
  File "e:\dao\dao\term.py", line 325, in __repr__
    result = interactive_solver().eval(code)
  File "e:\dao\dao\solve.py", line 190, in eval
    raise NoSolutionFound(exp)
dao.solve.NoSolutionFound: exit!
>>> unify((x,2), (1,2))
True

回溯
-----

所谓回溯，是指当求解某一目标失败的时候，系统回退通过其它路径求解。系统按照从上到下，从左到右的顺序调用命令，求解目标。系统先选择第一条路径进行求解，一旦遇到某一目标求解失败，如果该目标有多种求解方案，比如内置命令or_p，repeat，any，some，定义了多个函数体的用户函数或宏命令等等，系统将回退一步，选择该目标的下一求解方案进行求解。如果当前目标所有求解方案都失败，则回退到前一目标。如果当前层次的所有目标（比如函数能够匹配的所有不同定义）的所有求解路径都已经失败，则回退到上一层，比如上一级调用函数。直到所有层次所有目标的所有求解路径都已经失败，根目标才最终失败。系统总是依照求解顺序相反的次序，即从右到左，从下到上的次序进行回溯。

当系统回退经过每一目标时将根据需要恢复状态，比如合一操作进行的定义变量，变量绑定，解析状态等等。但是系统不会对赋值回退，这是有意设计的一个特征，使得我们可以在某些情况下通过赋值来控制回溯。

dao系统当前对目标产生多个解决方案和进行回溯的机制借助了python语言的yield语句，提高了运行速度。

如果目标有多个解，系统将能够根据需要进行回溯。。但是，即使目标没有多个解，也可以用repeat和fail命令的配合来实现回溯。

>>> from dao.dinpy import *
>>> import random
>>> do [ unify(i, pycall(random.randint, 0,10)), prin(i), iff(i!=3).do [fail]] 
0
dao.solve.NoSolutionFound: exit!

上述代码只有一条求解路径，因为执行fail命令而失败了。

借助repeat命令，我们可以产生无限的求解路径。

>>> do[ repeat, i << pycall(random.randint, 0,10), prin(i), iff(i!=3).do [fail] ]
6 4 1 5 10 5 7 2 10 8 5 0 0 4 8 9 9 5 9 3

当赋值给i的随机数不是3的时候，会因为执行fail而导致该次求解失败，从而引起回溯。在按相反次序回溯的途中，依次遇到的是打印命令prin(i)， 赋值命令i << pycall(random.randint, 0,10)，它们都没有其它求解方法，因此回溯到达repeat命令。

在prolog中，repeat是在用户程序中这样定义的::

  repeat.
  repeat:- repeat.

在dao系统中，repeat是个内建命令，但是它和prolog中的repeat所起的作用是一致的：产生无数求解路径。因此当系统遇到repeat，就开始新的求解路径再次前进。直到遇到随机数3，iff语句不再执行其的do块，从而成功得到整个目标的第一个解。

dinpy默认只执行求解目标的第一个解。利用findall命令，或者用solve(exp)的方式，可以求解目标的所有解。

* 使用findalll
>>> findall(do[ repeat, i << pycall(random.randint, 0,10), prin(i)], x, y)
0 0 10 10 9 10 9 10 1 3 2 4 7 5 5 8 0 1 8 4 6 0 8 5 10 4 5 8 1 5 7 9 7 9 7 1 6 2 8 3 1 2 9 8 10 8 9 5 10 6 10 8 9 6 9 3 0 9 1 2 8 8 4 10 7 9 1 6 5 0 1 5 9 0 2 5 6 6 4 7 8 6 1 7 2 ......

由于findall命令的作用，将导致上述命令无限执行下去。

* 使用solve

>>> solutions = solve(do[ repeat, i << pycall(random.randint, 0,10), prin(i), iff(i!=3).do [fail] ])
>>> solutions.next()
8 4 7 2 2 0 0 3
>>> solutions.next()
 0 1 0 9 5 3

对于上述代码，其解集是无限的。对于解集有限的问题，我们可以用solutions.next()逐一查看所有的解。
