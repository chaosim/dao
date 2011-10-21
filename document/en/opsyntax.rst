idea.

let(a,1), (b,2)), write(1)),
let({a:1, b:2}, write(1)), hope use less parentheses and brackets, to make it more readable and easier to be understand.

idea
a burst of insight
insight

theory to implement syntax on python operator
spend long time on th wrong way, I reached the turning point suddenly.
Mountain darkly, vista. Beyond the pale fruitless searching, have come to totally waste of effort.
山重水复疑无路，柳暗花明又一村。踏破铁鞋无觅处，得来全不费工夫。
meta class technique，finite state machine，regular expression。
the things I learned when I implemented syntax on python operator

twenty days and three hours

operator matcher

examples：
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

implemenation of dinpy
