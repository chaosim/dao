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

  - null
    
    又名nullword

    空字null不改变解析状态，直接成功。

    注意：null是匹配符调用，而不是匹配符。因此不要写成null()，而是直接写成null 。比如char('a')+null+char('b')，而不是char('a')+null()+char('b')

用dao实现lisp语言
-----------------

dao包含了lisp语言的一个实现实例：samples/sexpression.py。可以在dao/tests/testsexpression.py看到测试它的代码。另外，这个模块还包含了几个验证动态文法的特征。

sexpression.py没有用dinpy，而是直接用dao的原语编程。如果使用dinpy，可读性要好一些，但是功能上没有任何不同。
   
sexpression.py先从dao.special, dao.builtins 导入各种原语，同时也包括了用来解析sexpression的原语。当然，这些解析原语和其它所有原语一样，也可以用在被解析的sexpression程序代码之中。
   
然后定义了一个用于解析lisp符号的解析原语symbol，它识别符合lisp规则的符号字符串，如果具有与之对应的内置原语，则返回该原语，否则返回变量，请看如下代码::

     @builtin.macro()
     def symbol(solver, cont, result):
       ...
       name = text[pos:p]
       sym = _builtins.get(name, var(name))
       for _ in term.unify(result, sym, solver.env):
         ...
       ...

随后是一个辅助函数：sexpression2daoexpression。它的作用是把被解析为Cons数据类型的sexpression转换成dao可以直接执行的dao表达式。其实sexpression也是可以直接执行的，只要增加直接针对sexpression求值的几行代码的函数就可以了，我以前就彻底实现过这个函数并通过了完整的测试。但是为了方便，我采用了现在的方式。目前我看不出这两种方式有何实质的不同及优劣，如果以后有了更明确的结论，我再重新考虑有关的实现策略。

然后就是主体部分，也就是sexpresson的解析规则。这一组规则可以解析sexpression形式的合法lisp代码。规则还不完整，没有语法错误处理，虽然可以解析quasiquote和unquote表达式`exp, ,exp, ,@exp，但是还没有实现相关原语，因此还无法正确执行quosiquote表达式。dao并未实现所有lisp原语，但是有了这组规则，就可以用lisp方式利用所有的dao原语编写程序了。下面列出sexpresson解析规则的代码::

    sexpression_rules = [
    
      #原子表达式，解析数字，双引号串，lisp符号
     (atom_expression, function(
        ([X], number(X)),
        ([X], dqstring(X)),
        ([X], symbol(X))
        )),
  
     # 括号表达式，()和[]。
     (bracketExpression, function(
        ([ExprList], and_p(char('('), spaces0(_), sexpressionList(ExprList), spaces0(_), char(')'))),
        ([ExprList], and_p(char('['), spaces0(_), sexpressionList(ExprList), spaces0(_), char(']'))))),
  
     # 标点表达式，'exp, `exp, ,exp, ,@exp。dao只实现了quote原语。
     (puncExpression, function(
        ([L(quote, Expr)], and_p(char("'"), sexpression(Expr))),
        ([('quasiquote', Expr)], and_p(char("`"), sexpression(Expr))),
        ([('unquote-splicing', Expr)], and_p(literal(",@"), sexpression(Expr))),
        ([('unquote', Expr)], and_p(char(","), sexpression(Expr))))),
  
     # sexpression列表，方便写lisp风格程序，不需要将整个程序文本外加一层括号。
     (sexpressionList, function(
        ([Cons(Expr, ExprList)], and_p(sexpression(Expr), spaces_on_condition(), sexpressionList(ExprList))),
        ([nil], null))),
  
     # 允许程序文本的开始与末尾有任意的空白。
    (sexpression1, function(
        ([Expr], and_p(spaces0(_), sexpressionList(Expr), spaces0(_))))),
  
     # 解析空格
     (spaces_on_condition, function(
        ([], or_p(if_p(and_p(not_lead_chars('([])'), not_follow_chars('([])'), not_p(eoi)),
                       spaces(_)),
                  spaces0(_) ) ) ) ),
  
     #sexpression表达式的起始匹配符
     (sexpression, function(

         # 由eval_parse_result规则得到的顺理成章的推广，只是用来初步实验一下动态语法的实现。
         ([Result], and_p(char('{'), sexpression(Expr2), char('}'), 
                     setvalue(Result, eval_(pycall(sexpression2daoexpression, Expr2))))),

         ([Expr], atom_expression(Expr)),
         ([Expr], bracketExpression(Expr)),
         ([Expr], puncExpression(Expr))),
       ),
  
     # 前所未有的重大创新。漂亮的创意与思路的突破。!!
     # 统一解析与求值过程的关键与核心语句，籍此技术，就可以实现程序语言在语法上的彻底动态化!!
     # 有此技术，彻底打开了同向下一代编程语言的大门!!
    (eval_parse_result, function(
         ([Result], and_p(sexpression(Expr2), eoi, 
                   is_(Result, eval_(pycall(sexpression2daoexpression, Expr2))))))),
  
    ] 

然后就可以利用语法规则完成解析和求值。在此，先定义了管理语法的类Grammar，利用它可以指定语法的执行规则和起始符号。::

    class Grammar:
      def __init__(self, start, rules, result):
	self.rules, self.start, self.result = rules, start, result

    # 这个辅助函数利用grammar构造dao语句
    def make_parse_statement(grammar, text):
      return letr(grammar.rules, 
		  set_text(text), and_p(grammar.start, eoi), grammar.result)

最后是解析函数parse和eval。传统上解析和eval是两个分离的过程，并且一般利用截然不同的工具来完成。但是，dao系统的特质使得它既可以用来解析，也可以用来求值。从下面parse_eval函数的代码可以看到，解析和求值就是求解器Solver两次调用eval方法。第一次针对语法规则和用户文本，第二次针对产生的dao表达式。::

    def parse_eval(grammar, text):
      # 解析程序文本
      solver = Solver()
      exp = make_parse_statement(grammar, text)
      exp = solver.eval(exp) 

      # 借助eval_parse_result，就不再需要第二次求值了。 
      # 对解析结果再求值
      return solver.eval(sexpression2daoexpression(exp))
    
dao系统更强大神奇的能力是，我们可以在解析规则中调用eval_，利用该原语的元环求值能力，直接在解析的最后一步求值解析结果，从而不再需要parse_eval函数中的第二次求值。这就是sexpression_rules中eval_parse_result规则所完成的任务。有了它，如下parse函数一步即可同时完成解析和求值任务。::

    def parse(grammar, text):
      solver = Solver()
      exp = make_parse_statement(grammar, text)
      return solver.eval(exp)
    
依据语法规则集sexpression_rules，我们定义如下两个语法::

    grammar1 = Grammar(sexpression(Expr), sexpression_rules, Expr)
    grammar2 = Grammar(eval_parse_result(Result), sexpression_rules, Result)

对此，等式parse(grammar2 content)==parse_eval(grammar1, content)成立。 dao.tests.testsexpression 中 Testeval 和 Test_eval_by_parse 两组测试充分证实了这一点。    
可以进一步推广此项技术，在解析过程中任意时刻对已经解析的某部分结果进行求值。sexpression定义的第一条规则简单演示了这一技术。::

     #sexpression表达式的起始匹配符
	 (sexpression, function(

	     # 由eval_parse_result规则得到的启发，进行顺理成章的推广，简单演示实现动态语法。
	     ([Result], and_p(char('{'), sexpression(Expr2), char('}'), 
			 setvalue(Result, eval_(pycall(sexpression2daoexpression, Expr2))))),

	     ([Expr], atom_expression(Expr)),
	     ([Expr], bracketExpression(Expr)),
	     ([Expr], puncExpression(Expr))),
	   )
    
我们可以从dao.tests.testsexpression中Test_eval_while_parsing这一组测试看到上述规则的效果。

dao系统的这种能力，可以帮助我们彻底动态化编程语言的语法，将定义语法的能力完全交给程序员，程序员从此可以随心所欲的控制自己编写的代码中任意部分的语法。

与其它解析工具的比较
--------------------

- 与定子句文法的比较

  更直观，更高效，更通用，更易读易写，表达能力更强。

- 与lex/yacc，ply的比较

  lex/yacc依据LALR语法生成解析器转移状态表，产生高效的解析器代码，但是对文法限制较多。

  ply是lex/yacc在python中的实现，因为python语言的特征，比lex/yacc更容易使用。在文法上覆盖上也与lex/yacc相近。

- 与pyparsing，lepl，spark等的比较

  pyparsing和lepl都采用组合方式构造解析器，并通过回溯扩大文法覆盖范围。与dao比较，由于它们的文法符号不带参数，限制了其表达能力和易用性，不太方便添加语义动作，同时，它们也无法实现dao所能达到的文法动态性。而spark更类似于lex/yacc的方法，将解析进一步分隔成lexer和parser两个过程和部件，相对增加了解析工作的难度。

  lepl 利用memo的方法实现了对左递归文法的支持。我曾经作过实现左递归的努力，但是代码过于复杂。由于dao的文法符号可以携带参数，对传统上要求左递归的文法，dao系统也可以有简单优雅的表达方式。比如，编译教科书经常提供的左递归文法实例，算术表达式，依据优先级概念可以自然地写成如下的dinpy函数定义::

      letr (E  << fun((e1, '/', e2), 1) [E(e1, 2) + char('/') + E(e2, 1)]
		     (1, 2)[char('1')]
		     (e, 1)[E(e, 2)])
	   .do[ 
	    parse(E(e)+eoi, text
	   ]
  
  由于dao的特点，目前尚未看到不支持左递归对表达能力的实质影响。毕竟，目前还没有任何一种程序语言支持真正的左递归（第一条语句进行无条件递归调用）。因此，目前我选择放弃支持左递归，以保持实现代码的简单优雅。如果能找到简单优雅的设计，我将再次考虑支持左递归。

- 与antlr的比较

  ANTLR(ANother Tool for Language Recognition)是一个广为流行的解析器和编程语言工作平台。它以LL(*)文法为基础，支持包括java， python，C++在内的大量目标语言。dao系统的表达能力大大强于antlr系统的LL(*)文法。但是，从执行效率来看，Dao目前采用的是基于回溯的方法，还不如antlr。
  
提高dao系统执行效率的方法
---------------------------

Dao目前采用全回溯的方法，执行效率受到制约。从易到难，有多种提高dao的执行效率的技术：
  
1. 利用函数参数的signature选择规则，从而减少需要进行合一匹配操作的规则头数量。这一技术在pypy prolog中已经实现，现在已经融合进来，并且有一个大的改进(pypy prolog的设计有点问题，它没有真正地缩减规则数量，而是在调用谓词时扫描每条规则的signature以避免明显会失败的合一，dao系统的方法则是在建立函数时利用singaure预先建立一个signature到规则的映射，这样在调用函数时就可以真正削减规则数量。）。

2. 利用first集选择规则，依据前探字符减少尝试失败的规则数量，这也不难实现。

3. 利用memo机制，避免重复执行解析原语，有很多这样的参考实现，比如lepl，packrat解析器。重要的是要注意memo方法可能对程序语义带来的影响，比如赋值，打印等副作用等。这项优化的决策和实现可以考虑交给用户程序。

3. 利用pypy项目进行翻译或者是pypy的jit能力提高执行速度。对此需要了解dao的实现代码与pypy的翻译器或jit的兼容性，特别是dao的实现代码对rpython的符合程度，以评估所需的工作量。

4. 最后，Dao目前的实现是直接解释执行dao原语(Command及其子类的对象实例)，没有翻译为字节码的过程，更没有实现编译为本地码的编译器，因此，如果能实现dao的字节码编译器或本地码编译器，将可以大大的提升dao的执行速度，其解析速度也必能随之大大提高。字节码翻译我曾经实现了部分代码，但是要完整的实现，我个人目前没有时间和资源完成，需要更多的贡献者。因此字节码翻译和解释执行以及本地码编译这两项任务不在计划之内，等待将来看时机而定。
