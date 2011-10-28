逻辑命令
*********

  本章重点介绍dao系统的逻辑命令。逻辑命令与逻辑程序设计相关。纯函数式程序语言中，根据调用层次和代码布局来看，目标求值总是按照从上到下，从左到右的次序进行，而且进行过程中，所有目标总是成功，不存在会失败的求解目标，也不存在回溯的机制。而逻辑程序则不同，目标可能有多次求解的机会，并且可以因为合一和目标求解失败而引起系统自动回溯。但是纯逻辑系统求解的目标不具有返回值，只能通过变量的合一传递数据。dao系统综合了两种系统的特征，既可以象函数一样，通过传递参数和返回值传递数据，组织函数调用层次管理控制流，也可以通过合一传递数据，通过规则匹配，依据求解成功与失败控制回溯来管理控制流。有了这些手段，我们就可以更轻松的构造更加复杂的程序。正因如此，dao系统的命令也就具有了更多的特性。根据需要，不同命令可能具有多种甚至无数求解方案，或者其求解有可能成功也可能失败，或者永远失败。这些命令，如果成功，会返回一定的值，如果失败，则根本不会返回任何结果，而是发生回溯。也有的命令表现得很象普通的函数，只求解一次并成功返回结果。这就需要我们在实际运用中更多地了解这些命令的不同特性。
  
  依据逻辑命令在求解机制上的特点，我们将它们分成两类：  

  * 函数: 这一类命令的执行类似于普通语言中的函数，它们只有一种求解方案，并且总是会成功地返回一个结果。
  
  * 逻辑谓词：这一类命令的执行可能有多种方案，也可能会失败。如果成功，它们能够返回结果。但是实际使用中，我们也可能只关心它们的成功失败，而并不关心它们的返回结果。
  
  在后面的叙述中，对于需要特别区分的命令，我们将特别注明其分类。至于那些用途明显的命令，就没有一一标注。

逻辑控制
-------

* 谓词and_p: 逻辑与谓词

  格式: and_p( *calls )

  如果calls中所有子目标都成功，则and_p(*calls)成功。

  >>> and_p(succeed, succeed)
  True
  >>> and_p(succeed, fail)
  Traceback (most recent call last):
    File "e:\dao\dao\dinpy\term.py", line 1, in <module>
      from dao.builtins.term import *
    File "e:\dao\dao\term.py", line 325, in __repr__
      result = interactive_solver().eval(code)
    File "e:\dao\dao\solve.py", line 192, in eval
      raise NoSolutionFound(exp)
  dao.solve.NoSolutionFound: exit!
  
  >>> and_p(True,True)
  True
  >> > and_p(True,False)
  False
  >>> and_p(False,True)
  True
  >>> and_(True, False)
  False
  >>> and_(True, True)
  True
  >>> and_(False, True)
  False 

  上述实例体现了逻辑谓词与函数的明显不同。True本身是个值，但是也可以看成常量函数，即不需参数，并且重视返回True。在此意义上，False和True是同一类型的物体，只是返回值不同而已。系统对它们求值时，只有一种求解方案，没有被选方案，而且总是成功地返回结果。因此，使用and_p对它们求值的时候，and_p也总是成功的。由于and_p在实现上总是返回后一目标的结果，这才使得上述三次调用具有如此结果。

  我们要将逻辑与谓词和python的and运算区别开，也要将它dao系统本身的逻辑与运算and__区别开。一般来讲，我们使用and_p谓词的时候，是希望利用子目标call1和call2的成功与失败的组合控制程序的执行来得到不同结果，因此，我们应该注意子目标应该都是适当的逻辑谓词，而不是常量或者普通函数。下面介绍的or_p谓词也具有同样的特点。

  本节的逻辑控制谓词都是基于这种目的设计的，因此在使用上我们要研究它们共同的特点，避免错误的用法。

* 谓词or_p: 或谓词

  格式: or_p( *calls )
  如果calls有一个子目标成功，则or_p(*calls)成功。

  >>> or_p(succeed, fail)
  True
  >>> or_p(fail, fail)
  Traceback (most recent call last):
    File "e:\dao\dao\dinpy\term.py", line 1, in <module>
      from dao.builtins.term import *
    File "e:\dao\dao\term.py", line 325, in __repr__
      result = interactive_solver().eval(code)
    File "e:\dao\dao\solve.py", line 192, in eval
      raise NoSolutionFound(exp)
  dao.solve.NoSolutionFound: exit!

  >>> or_p(True,True)
  True
  >> > or_p(True,False)
  True
  >>> or_p(False,True)
  False

  从or_p(True,True)，or_p(True,False)，or_p(False,True)的求值，可以知道，True, False的求值总是成功成功返回值本身，而or_p总是返回第一个成功的子目标的值，这就是上述例子的表现。

* 谓词if_p: 条件谓词

  格式: if_p(antecedent, consequent)
  
  如果antecedent成功并且consequent成功，则if_p(antecedent, consequent)成功，否则失败。
  
  if_p的上述语义是依据 Prolog的ISO的标准，也是各个prolog实现的事实标准。请参考prolog的有关文档。

  >>> if_p(succeed, succeed)
  True
  >>> if_p(succeed, fail)
  Traceback (most recent call last):
    File "e:\dao\dao\dinpy\term.py", line 1, in <module>
      from dao.builtins.term import *
    File "e:\dao\dao\term.py", line 325, in __repr__
      result = interactive_solver().eval(code)
    File "e:\dao\dao\solve.py", line 192, in eval
      raise NoSolutionFound(exp)
  dao.solve.NoSolutionFound: exit!
  >>> if_p(fail, succeed)
  Traceback (most recent call last):
    File "e:\dao\dao\dinpy\term.py", line 1, in <module>
      from dao.builtins.term import *
    File "e:\dao\dao\term.py", line 325, in __repr__
      result = interactive_solver().eval(code)
    File "e:\dao\dao\solve.py", line 192, in eval
      raise NoSolutionFound(exp)
  dao.solve.NoSolutionFound: exit!
  >>> if_p(fail, fail)
  Traceback (most recent call last):
    File "e:\dao\dao\dinpy\term.py", line 1, in <module>
      from dao.builtins.term import *
    File "e:\dao\dao\term.py", line 325, in __repr__
      result = interactive_solver().eval(code)
    File "e:\dao\dao\solve.py", line 192, in eval
      raise NoSolutionFound(exp)
  dao.solve.NoSolutionFound: exit!

  与and_p和or_p类似，使用if_p也要注意它作为逻辑谓词的特点。if_p是否继续对consequent求值，是依据antecedent求值的成功与否，而不是依据求解antecedent得到的返回值。这是它在使用上要特别予以注意的。试比较如下两次求解过程::
  
    >>> if_p(False, print(1))
    1
    >>> if_p(fail, prin(1))
    Traceback (most recent call last):
    File "e:\dao\dao\dinpy\term.py", line 1, in <module>
      from dao.builtins.term import *
    File "e:\dao\dao\term.py", line 325, in __repr__
      result = interactive_solver().eval(code)
    File "e:\dao\dao\solve.py", line 192, in eval
      raise NoSolutionFound(exp)
    dao.solve.NoSolutionFound: exit!

* 谓词not_p: 否定谓词

  格式: not_p(goal)
  
  类似于prolog，dao系统采用了失败作为否定的实现机制。如果goal失败，则not_p(goal)成功，否则not_p(goal)失败。

    >>> not_p(True)
    Traceback (most recent call last):
      File "e:\dao\dao\dinpy\term.py", line 1, in <module>
	from dao.builtins.term import *
      File "e:\dao\dao\term.py", line 325, in __repr__
	result = interactive_solver().eval(code)
      File "e:\dao\dao\solve.py", line 192, in eval
	raise NoSolutionFound(exp)
    dao.solve.NoSolutionFound: exit!
    >>> not_p(False)
    Traceback (most recent call last):
      File "e:\dao\dao\dinpy\term.py", line 1, in <module>
	from dao.builtins.term import *
      File "e:\dao\dao\term.py", line 325, in __repr__
	result = interactive_solver().eval(code)
      File "e:\dao\dao\solve.py", line 192, in eval
	raise NoSolutionFound(exp)
    dao.solve.NoSolutionFound: exit!
    >>> not_p(succeed)
    Traceback (most recent call last):
      File "e:\dao\dao\dinpy\term.py", line 1, in <module>
	from dao.builtins.term import *
      File "e:\dao\dao\term.py", line 325, in __repr__
	result = interactive_solver().eval(code)
      File "e:\dao\dao\solve.py", line 192, in eval
	raise NoSolutionFound(exp)
    dao.solve.NoSolutionFound: exit!
    >>> not_p(fail)
    True
    >>> not_(False)
    True
    >>> not_(True)
    False
    >>> not_(succeed)
    False
    >>> not_(fail)
    Traceback (most recent call last):
      File "e:\dao\dao\dinpy\term.py", line 1, in <module>
	from dao.builtins.term import *
      File "e:\dao\dao\term.py", line 325, in __repr__
	result = interactive_solver().eval(code)
      File "e:\dao\dao\solve.py", line 192, in eval
	raise NoSolutionFound(exp)
    dao.solve.NoSolutionFound: exit!
    
  注意not_函数和not_p谓词的区别。一般用法要求not_p的参数是个谓词，如果是普通函数，由于普通函数总是会成功返回结果，因此not_p将总是失败。而not_(x）函数的作用是求参数的逻辑反，相当于python的not x。

* succeed: 成功谓词

  格式: succeed

  成功一次。

* fail: 失败谓词

  格式: fail
  
  失败一次。
  
  从前面的实例中已经看到了succeed和fail的用法。

* findall: 求所有解

  格式: findall(call, template=None, result=None)
  
    >>> let( f << fun()[2][3] ) .do[ findall(is_(x, f()), x, y), prin(y) ]
    [2, 3]
  
  findall求目标call的所有可能的解，如果参数result不是None，则求解过程中将依据模板template收集数据到一个列表中，然后与result进行合一。模板可以是任何可以进行合一的对象，但是系统并不对模板求解，因此要避免使用函数，宏之类的命令作为模板参数。

  上述命令中，先求findall需要求的is_(x, f())的全部解集。is_命令的作用是先求它的第二个参数的解，然后设置为第一个参数合一。因为f()定义了两个函数体，因此可以进行两次求解，分别返回2和3，每次合一到x后都被收集到结果列表，最终合一到了结果变量y。

  模板参数和结果参数可以是自由变量，也可以是非自由变量或者是数据。如果结果参数不是自由变量，有可能引起findall执行的失败。可以利用这一特性判断某一命令的全部解集是否符合指定的数据值。

* call: 求解谓词

  格式: call(goal)

  先取得goal的值（注意，使用getvalue命令取值，不是求解），然后对取得的值求解。

    >>> is_(x, quote(prin(1)))&call(x)
    1

* once: 一次求解谓词

  格式: once(goal)

  >>> findall(once(prin('1, ')|prin('2, ')))
  1, True

* 重复求解谓词(repeat)

  格式: repeat

    >>> let( i<<0 ). do[ repeat, prin(i), ++i, iff(i<3).do[fail] ]
    0 1 2

  repeat/fail构造的循环和loop类型的循环有所不同。repeat的循环执行必须依靠fail命令引发失败，回溯到repeat之后重新进行另一轮求解，回溯过程中遇到的合一，解析状态改变都将一一恢复到之前的状态。而在loop语句是正常的前向执行，不存在回溯，也不会有回退合一以及解析状态的动作。

* cut: 截断谓词

  格式： cut

  cut是prolog引进的削减求解空间，提高执行效率的机制。dao系统的实现遵循prolog的标准。当在用户定义的规则体中遇到cut，则冻结已经求得的解，当失败引起回溯时，不再尝试当前用户定义目标（用户定义的函数或者宏）的其它可选求解路径。

    >>> letr( a << fun(x) [ b(x)&cut&c(x) ],
		b << fun(1) [True]
			(2) [True]
			(3) [True],
		c << fun(1) [True] 
	       ).do[ 
	       a(x), x ]
    1
    >>> letr( a << fun(x) [ b(x)&cut&c(x) ],
		b << fun(1) [True]
			(2) [True]
			(3) [True],
		c << fun(1) [True] 
	       ).do[ 
	       a(x), x ]
    Traceback (most recent call last):
      File "e:\dao\dao\dinpy\term.py", line 1, in <module>
	from dao.builtins.term import *
      File "e:\dao\dao\term.py", line 325, in __repr__
	result = interactive_solver().eval(code)
      File "e:\dao\dao\solve.py", line 192, in eval
	raise NoSolutionFound(exp)
    dao.solve.NoSolutionFound: exit!

  看下面两段代码，因为截断谓词的作用，因为冻结了b(x)的求解分支，不再回溯它的另一分支，导致第一段代码的唯一结果为3，后一段代码第一遍求解的结果为4。

    >>> letr( a << fun(x) [ b(x)&cut&c(x) ]
                        [ d(x) ],
            b << fun(1) [True]
                    (4) [True],
            c << fun(4) [True], 
            d << fun(3) [True], 
           ).do[ 
           a(x), x ]
    3
    >>> letr( a << fun(x) [ b(x)&c(x) ]
                        [ d(x) ],
            b << fun(1) [True]
                    (4) [True],
            c << fun(4) [True], 
            d << fun(3) [True], 
           ).do[ 
           a(x), x ]
    4

变量和项
------------

* unify: 合一谓词

  格式: unify(x, y)

* is_: 变量与求得的值合一

  格式: is_(x, exp)

* setvalue: 变量设值

  格式: setvalue(var, value)

* getvalue: 取值

  格式: getvalue(item)

* getvalue_default: 默认取值

  格式: getvalue_default(item, default=None)

* 函数free: 是自由变量？

  格式: free(var)

* 谓词free_p: 是自由变量？

  格式: free_p(var)

* 函数bound: 是绑定变量？

  格式: bound(var)
  
  如果var被绑定到其它对象，包括被绑定到变量，则返回True，否则返回失败。

* 谓词bound_p: 是绑定变量？

  格式: bound(var)
  
  如果var被绑定到其它对象，包括被绑定到变量，则成功并返回True。否则失败。

* 函数ground: 是否实值

  格式: ground(item)

* 谓词ground_p: 是否实值

  格式: ground_p(item)

* unbind: 去除变量绑定

  格式: unbind(var)

  去除变量绑定，使得var成为自由变量

* 函数isvar: 是变量？

  格式: isvar(item)

* 谓词isvar_p: 是变量？

  格式: isvar(item)

* 函数nonvar: 不是变量？

  格式: nonvar(item)
* 谓词nonvar_p: 不是变量？

  格式: nonvar_p(item)

