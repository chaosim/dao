扩展Dao和Dinpy
***************

Dao内核介绍
---------------

* 后续传递型程序(CPS,  continuation passing style)

  利用后续实现lisp

  利用双后续实现prolog

  * 跳床技术
 
    * 跳床技术在dao中的运用和改进

* yield 语句与python产生器

  * Yield Prolog采用的技术

    * yield prolog技术在dao中的运用和改进

为dao增加内置函数
-----------------

builtin.function

@builtin.function
def function_name(arg...):
   ...

builtin.function2::

  @builtin.function
  def function_name(solver, cont, arg...):
     ...

为dao增加内置宏定义
------------------

builtin.macro

  @builtin.macro
  def macro_name(solver, cont, arg...):
     ...

builtin.matcher
  @builtin.matcher
  def macro_name(solver, cont, arg...):
     ...

为dao增加特殊式
----------------

SpecialForm

增加或修改Dinpy的语法
