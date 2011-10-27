逻辑命令
*********

逻辑控制
-------

* and_p: 与谓词

  格式: and_p(call1, call2)

  如果call1与call2都成功，则and_p(call1， call2)成功。

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

* or_p: 或谓词

  格式: or_p(call1， call2)
  如果call1与call2有一个成功，则or_p(call1， call2)成功。

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

* if_p: 条件谓词

  格式: if_p(condition, conclusion)
  
  如果condition成功并且conclusion成功，则if_p(condition, conclusion)成功，否则失败。
  
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

* not_p: 否定谓词

  格式: not_p(goal)
  
  如果goal失败，则not_p(goal)成功，否则not_p(goal)失败。

* succeed: 成功谓词

  格式: succeed

* fail: 失败谓词

  格式: fail
 
* findall: 求所有解

  格式: findall(call, template, result)

* call: 求解谓词

  格式: call(goal)

* once: 一次求解谓词

  格式: once(goal)

* 重复求解谓词(repeat)

  格式: repeat

* cut: 截断谓词
  格式： cut

变量和项
------------

* unify: 合一谓词

  格式: unify(condition, conclusion)

* is_: 变量与求得的值合一

  格式: is_(condition, conclusion)

* setvalue: 变量设值

  格式: setvalue(var, value)

* getvalue: 取值

  格式: getvalue(item)

* free: 是自由变量？

  格式: free(var)

* free_p: 是自由变量？

  格式: free_p(var)

* bound: 是绑定变量？

  格式: bound(var)
  
  如果var被绑定到其它对象，包括被绑定到变量，则返回True，否则返回失败。

* bound_p: 是绑定变量？

  格式: bound(var)
  
  如果var被绑定到其它对象，包括被绑定到变量，则成功并返回True。否则失败。

* ground: 是否实值

  格式: ground(item)

* ground_p: 是否实值

  格式: ground_p(item)

* unbind: 去除变量绑定

  格式: unbind(var)

  去除变量绑定，使得var成为自由变量

* isvar: 是变量？

  格式: isvar(item)

* nonvar: 不是变量？

  格式: nonvar(item)

