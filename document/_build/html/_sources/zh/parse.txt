解析器
*******

    dao具有与prolog相似的执行逻辑程序的能力。利用定子句语法，prolog可以作为解析器使用。dao也可以作为一个完整的解析器使用，但是在实现上与prolog有所不同。prolog是将定子句文法规则进行翻译，每条规则都增加两个参数：解析文本的已解析部分和剩余部分，以此转换成普通prolog规则。Dao的方法很简单：Dao系统的求解器Solver增加一个代表解析状态的成员：solver.parse_state。解析命令对求解器的parse_state进行操作，感谢yield语句的魔法，回溯时解析谓词负责回退本身所作的操作，因此，解析功能与求解器其它部分没有任何耦合，也不影响其它部分的执行效率。这种方法比定子句文法更加灵活，高效。它没有从接口上对parse_state作任何假定，大大地扩展了解析器的应用范围。Dao系统不光可以解析文本，字符串，也可以解析二进制流，列表，元组，数组等任意序列，甚至可以轻松的应用它解析树，图或者其它任意的对象。据我所知，dao系统的表达能力和解析能力超过目前已有的任何文法及其解析器。它可以简洁直观地表示上下文无关文法和 上下文相关文法无法表示的情况，比如a*n b*n c*n, 回文，也具有超过packrat 解析器的解析能力，可以解析parsing expression grammar(PEG)所能表示的一切文法。利用lisp或prolog的元环求值技术(lisp的eval 或prolog的call)，它甚至可以在解析过程中轻松的动态修改文法，这一独特之处是任何其它解析工具从所未见的，这一能力是我多年所梦想和追求的，我相信它一定可以带来神奇的应用。

设置与获取解析状态或解析对象
-----------------

不同于prolog的定子句文法，dao解析器具有大量的自省操作，它可以在任意时间设置，查询解析状态。

- 设置与获取解析状态

  - set_parse_state(parse_state)

    设置解析状态。

  - get_parse_state()

    获取解析状态。 

  - unify_parse_state(x）

    将x与当前解析状态合一。x可以是变量或者表示解析状态的值。如果合一失败，整个谓词会失败。

  - parse(matcher, parse_state)

    先将求解器的parse_state赋值为parse_state, 然后求解器调用matcher进行解析，解析成功后将恢复到原来的解析状态。
  
  解析状态可以是任意类型的对象，一般是被解析对象和位置指针的二元组。比如对于序列，可以在解析开始前设置初始解析状态为(sequence, 0)。如果是解析树，可以将初始解析状态设置为树的根节点。当然也可以根据需要作不一样的设置。至于其他解析对象就不具体介绍了。
  
  值得强调的是：上述命令中，set_parse_state和parse命令如果遇到回溯，不管是失败引起的回溯或者是寻求多个解引起的回溯，都将回溯到该命令内部，有该命令本身回退本身先前所作的动作。后面介绍其它有副作用的命令都与此类似，使用时请加以注意。

  >>> set_parse_state(('abc', 0)
  ('abc', 0)
  >>> get_parse_state()
  ('abc', 0)
  >>> unify_parse_state(y)
  ('abc', 0)
  >>> y
  ('abc', 0)
  >>> unify_parse_state(('abc',z))
  ('abc', 0)
  >>> z
  0
  >>> char(x)
  'a'
  >>> x
  'a'
  >>> get_parse_state()
  ('abc', 1)

  上述过程中，首先初始化解析状态，要求从位置0开始解析文本'abc'，然后，我们看到可以通过不同的方式取回当前的解析状态。char是一个预定义终结符，它的作用是与解析状态当前位置元素合一。如果合一失败，则匹配失败，合一成功，则解析位置增一。我们看到char(x)成功合一并将x绑定为'a'。再次用get_parse_state()，我们看到解析状态已经前进一个位置，变成了('abc', 1)

  有了这几个谓词，我们就可以利用dao语言编程任何复杂的解析器。可以通过dinpy程序或程序的方式，也可以直接编写扩展的方式，都可以很容易就实现。下面就先针对最常见的解析序列，特别是解析文本的任务来介绍系统提供的预定义匹配器。

- 设置解析序列或文本

  - set_sequence(sequence)

    将解析状态设置为(sequence, 0)。

  - set_text(text)

    set_setquence的别名。 将解析状态设置为(text, 0)。

  - get_sequence()

  - get_text()

  - unify_sequence(x)

  - unify_text()

- 对序列或文本调用解析命令

  - parse_sequence(matcher, sequence): 

    先将求解器的parse_state赋值为(sequence,0), 然后求解器调用matcher进行解析，解析成功后将恢复到原来的解析状态。

  - parse_text(matcher, text)

    parse_sequence的别名。先将求解器的parse_state赋值为(text,0), 然后求解器调用matcher进行解析，解析成功后将恢复到原来的解析状态。

组合匹配符
----------

    可以利用组合匹配符组成复杂的解析命令。组合方式包括序列，与，或，可选，重复，分隔列表等。这些符号都定义在dao.dinpy.matcher模块中，包括MatcerSequence, MatcherOr, Repeater，may, parallel, any, some, times, times_more, times_less, seplist, seplist_more, seplist_less，seplist_between, follow等。

    所有的组合匹配符都没有对于求解器的parse_state的接口做任何假定，因此它们都可以用于任何类型的解析对象，并不仅仅用于解析序列。

- MatcerSequence(a, b)
  序列匹配符。可以利用运算符+产生序列匹配符，我们通常写a+b，而不是写MatcerSequence(a, b)。先匹配a，如果a成功，则推进解析状态后再匹配b，如果b也成功，则a+b匹配才成功。否则a+b匹配失败。
  序列匹配符也是可用于任何类型的解析对象的。它只是依次执行a和b并推进解析状态，并没有对解析状态的内部接口做任何要求。

- MatcherOr(a, b)
  或匹配符。可以利用运算符|产生或匹配符，我们通常写a|b，而不是写MatcerSequence(a, b)。先匹配a，如果a成功则a|b成功，否则再从匹配a **之前** 的位置开始匹配b，如果b成功，则a|b匹配才成功。只要a， b中间任何一个匹配成功，a|b就成功，只有a，b两者都匹配失败，a|b才匹配失败。

- optional(item, mode=nongreedy):
  可选匹配符。也可写成may(item, item)。有贪婪和非贪婪两种工作模式。贪婪模式下，item匹配成功后optional(item, mode)命令不会回溯。非贪婪模式下，可以回溯，当发生回溯时，may(item, nongreedy)的作用等同于空字符nullword。

  ~item产生贪婪可选，-item产生非贪婪可选。

- parallel(a,b): 
  平行匹配符。如果a,b能够都能匹配从解析位置开始的一段等长字符，则parallel(a,b)匹配成功。

- Repeater(item, separator=None, min=None, max=None, template=None, result=None, mode=nongreedy)
  重复匹配符，它能产生包括optional, any, some, times, times_more, times_less, seplist, seplist_more, seplist_less，follow在内的各种重复组合。可以利用item[min:max]/separator%template*result的格式产生重复匹配符，并且可以通过前置一元运算符+或-指示重复匹配符的工作模式：贪婪，非贪婪和懒惰模式。

  - 非贪婪模式(nongreedy): matcher[:]/separator%template*result，默认工作模式，先尽量多匹配字符，如果后续命令失败，则发生回溯，减少重复次数，回退解析位置，然后重新执行后续命令。
  
  - 贪婪模式(greedy): ~matcher[:]/separator%template*result，尽量多匹配字符，即使后续命令失败，也不会在这个重复匹配符发生回溯，不会回退解析位置。
  
  - 懒惰模式(lazy): -matcher[:]/separator%template*result，先尽量少地匹配字符（相当于从空字(nullword)开始匹配），如果后续命令失败，可以发生回溯，则增加重复次数，推进解析位置，然后重新执行后续命令。

  如果出现了/separator部件，则匹配一个以separator分隔的item的列表，即形如item+separator+item+...+separator+item的列表。
  
  如果出现了%template*result部件，则依据模板template依次收集各重复项到一个结果列表，然后result与此结果列表合一。如果合一失败，将导致匹配失败。%template省略，则template默认为None，因此结果列表将是[None, None, ..., None]的形式。

以下介绍重复匹配符。所有重复匹配符，除了指定整数次重复times命令以外，都能以贪婪，非贪婪和懒惰三种模式工作。如果指定了template和result，则依据模板template收集一个结果列表与result合一。对此前面已经作了介绍。

any(item, template=None, result=None, mode=nongreedy)
  任意次重复匹配符。试图匹配item的任意次重复，包括0次。

some(item, template=None, result=None, mode=nongreedy)
  一次或多次重复匹配符。试图匹配item的一次或多次重复，不包括0次。

times(item, expect_times, template=None, result=None, mode=nongreedy):
  指定次数重复匹配符。如果expect_times是个整数或者被绑定到了整数，则试图匹配item固定次数的重复出现。如果expec_times是个自由变量，则其工作方式与any一致，并且expec_times将绑定到实际重复次数。

times_more(item, expect_times, template=None, result=None, mode=nongreedy)
  重复expect_times以上次数匹配符。expect_times必须是个非负整数。试图匹配item的expect_times或更多次数的重复出现

times_less(item, expect_times, template=None, result=None, mode=nongreedy)
  重复expect_times以下次数匹配符。expect_times必须是个非负整数。试图匹配item的0次到expect_times次数的重复出现

times_between(item, min, max, template=None, result=None, mode=nongreedy)
  重复min到max次匹配符。min和max必须是非负整数。试图匹配item的0次到expect_times次数的重复出现

seplist(item, expect_times, template=None, result=None, mode=nongreedy):
  指定次数分隔列表匹配符。如果expect_times是个整数或者被绑定到了整数，则试图匹配以separator分隔的item固定次数的出现。如果expec_times是个自由变量，则试图匹配以separator分隔的item任意次出现，并且expec_times将绑定到item的实际重复次数。

seplist_more(item, expect_times, template=None, result=None, mode=nongreedy)
  重复expect_times以上次数分隔列表匹配符。expect_times必须是个非负整数。试图匹配以separator分隔的item的expect_times或更多次出现

seplist_less(item, expect_times, template=None, result=None, mode=nongreedy)
  重复expect_times以下次数分隔列表匹配符。expect_times必须是个非负整数。试图匹配以separator分隔的item的0次到expect_times次出现

seplist_between(item, min, max, template=None, result=None, mode=nongreedy)
  重复min到max次分隔列表匹配符。min和max必须是非负整数。试图匹配以separator分隔的item的0次到expect_times次出现

简单原语
--------
position， nextchar, step, skip, subtext, goto,

position
    当前解析位置

next_element(x)
next_char(x)
    下一元素。将下一元素与x合一。不改变解析状态。只用于序列类型解析对象。

step(n=1)
  解析位置前进n步，但是返回序列中前进之前所在位置的的元素

skip(n=1)
  解析位置前进n步, 返回序列中前进后所在位置的元素。

sub_sequence((start, end)
| subtext(start, end)
  返回解析序列的一个字段parser_state[0][start:end]

set_position(position)
| goto(position)
  解析位置跳到positioin

终结匹配符
---------
  char, eos, lead_chars, not_lead_chars, follow_chars, not_follow_chars, char_in, digit, _1_9, lowcase, uppercase, letter, uletter, _letter_digit_test, u_letter_digit, space, digits, digits0, lowcaseString, uppercaseString, uLetterdigitString, uLetterdigitString0, spaces0, spaces, lead_string, not_lead_string, follow_string, not_follow_string, quote_string, dqstring, sqstring, number, literal
number
 
- 产生终结符的python函数
  char_on_test, char_in， char_between,  string_on_test， string_in， string_between， quote_string

  - char(x):
    如果x与当前位置元素合一成功，则char(x)成功，解析状态位置增一，否则char(x)失败。

  - eos
    如果解析器状态已经到达序列尾部(solver.parse_state[1]>=len(solver.parse_state[0])，则eos成功，否则eos失败。

    注意：eos是匹配符调用，而不是匹配符。因此不要写成eos()，而是直接写成eos。比如char('a')+eos，而不是char('a')+eos。

  - letter, 

  - nullword
    空字nullword不改变解析状态，直接成功。

    注意：nullword是匹配符调用，而不是匹配符。因此不要写成nullword()，而是直接写成nullword 。比如char('a')+nullword+char('b')，而不是char('a')+nullword()+char('b')

与其它解析工具的比较
--------------------

- 与定子句文法的比较

  更直观，更高效，更通用，更易读易写，表达能力更强。

- 与antlr的比较

  ANTLR(ANother Tool for Language Recognition)以LL(*)文法为基础，支持包括java， python，C++在内的大量目标语言。从表达能力来看，dao系统要大大强于antlr系统的LL(*)文法。从执行效率来看，Dao目前采用的是基于回溯的方法，解析效率不如antlr。

- 与lex/yacc的比较

  lex/yacc生成LALR语法解析器转移状态表

- 与pyparsing，spark，ply，lepl等的比较

  Dao目前是完全基于回溯的方法。可以有几种比较简单的提高dao的执行效率的技术：第一：利用函数参数的signature选择规则，这一技术在pypy prolog中已经实现，可以很快捷的融合进来。第二：利用first集选择规则，这也不难实现。不久的将来我个人就可以完成它们。另外Dao目前的实现是解释执行，没有翻译为字节码的过程，更没有实现编译为本地码的编译器，因此，如果能实现dao的字节码编译器或本地码编译器，将可以大大的提升dao的执行速度，其解析速度也必能随之大大提高。字节码翻译我曾经实现了部分代码，但是要完整的实现，需要更多的贡献者，因此这两项任务都不在我的计划之内，等待将来看时机而定。还有一种方案，即利用pypy项目进行翻译或者是jit加速。
