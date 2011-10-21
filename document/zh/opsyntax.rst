算符文法
*********

创意
let(a,1), (b,2)), write(1)),
let({a:1, b:2}, write(1)), 希望少几个括号，更易读一点。

灵光一闪
灵感

实现原理
弯路与转折：山重水复疑无路，柳暗花明又一村。踏破铁鞋无觅处，得来全不费工夫。
元类技术，有限状态机，正规表达式。
实现经历与经验教训
20天与三小时

运算符匹配器

实例：
some tools that help you define operator grammars to preparse python expression.
see dinpy.py for a real sample.

>>> from oad.term import Var
>>> from oad.builtins.terminal import eos
>>> from oad.builtins.term import pytuple, first
>>> bindings, body = Var('bindings'), Var('body')
>>> do = word('do')
>>> let = element('let', call(bindings)+do+getitem(body)+eos+pytuple(first(bindings), body))
>>> preparse(let({'x':1}).do[1,2])
({'x': 1}, (1, 2))

dinpy的实现
