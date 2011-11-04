常用语句
*********

**(本章基本完成)**

  Dinpy提供了丰富的语句，有些语句是传统语言常见的，比如iff, when_loop, each_loop等（为避免与python关键字冲突，语句关键字有所不同）。有些语句是lisp系列语言所独有的。比如block/exit, catch/throw，protect/always等。本章将介绍如何使用这些语句。

  后面的文字中，为了说明语句的格式，引入一些约定：
  
  {{  }}
      表示分组。
  <<  >>  
      表示其中的内容是可选的，可以出现一次或者不出现。 
  ...
      表示前面的项可以重复任意次，包括0次。  
  !...
      表示前面的项重复一次以上。

  例如，语句...  表示逗号分隔的语句列表。


简单语句
--------

* 顺序执行语句(do语句)。
  
  格式: do[ 语句... ]

  >>> do[i<<0, prin(i), prin(' '), ++i]
  0 1

  do 后面跟随一组语句序列，用中括号括起来，形成顺序执行语句。依据python分隔表达式的要求，语句和语句之间以逗号隔开。

* 变量绑定语句(let语句)。
  格式: let ( {{变量<<值}}... ) . do[ 语句... ]
  
  >>> let(i << 1).do[ i ]
  1

* 递归绑定语句(letr语句)。
  格式: letr ( {{变量<<值}}... ) . do[ 语句... ]

  >>> letr(f << fun(x)[ iff(x==1) .do[ 1 ] .els[ x+f(x-1) ]  ]) .do [f(4)]
  10

分支语句
--------

* 条件分支语句(iff语句)

  格式: iff(条件).do[ 语句... ] {{ .elsif(条件).do[ 语句... ] }}...  << .els[ 语句... ] >>

  从iff开始，依次测试iff和每个elsif分支的条件，如果条件为真，则执行随后对应的do块，如果所有的条件都为假，并且存在els块，则执行els块中的语句。
  
  可以看到，dinpy选用的都是不与python的if-elif-else冲突的关键字。

  上述格式说明中，为了方便，{{}}表示分组，... 表示前面的内容可以重复任意次，包括0次。 << ... >>>表示其中的内容是可选项，可以出现，也可以不出现。后面对此不知一一说明。

  >>> iff(0).do[1] .elsif(1).do[2] .els[3]
  2
  >>> iff(0).do[1] .elsif(0).do[2] .els[3]
  3
 
* 分情况语句(case语句)。
  
  格式: case(表达式) {{.of(值...)[ 语句... ]}}... << .els[ 语句... ] >>
  
  先对case后的表达式求值，看值包含在哪一个of后面的值列表之中，选择执行对应的语句列表。如果表达式的值不包含在任何一个of后的值列表中，则执行els后面的语句列表。

  >>> x
  x
  >>> case(x).of(1)[prin(1)].of(2,3)[prin(4)].els[prin(5)]
  5
  >>> x<<3
  3
  >>> case(x).of(x)[prin(1), 1].of(2)[prin(2), 2].els[prin(3), 3]]
  3
  >>> case(x).of(1)[prin(1), 1].of(2)[prin(2), 2].els[prin(3), 3]]
  1
  >>> x<<(1,2)
  (1, 2)
  >>> case(x).of((1,2), (3,4))[prin((1,2),(3,4))].of((2,3))[prin((2,3))].els[prin('others')]
  (1, 2) (3, 4)

  我们看到，最后一个例子的case表达式是个元组。case表达式可以是整数以外类型的数据。只要表达式的值可以作为python dict的键，就可以用在case语句中。


块结构
-------

* 块语句(block语句)。格式：block.name[语句...]

  >>> block.name[ prin(1), prin(2)]
  1 2

* 退出语句(exit语句)。

  格式：exit {{ <<.label>> | {{ << /type >> << *level >> }} }}  << '>>' 返回值 >>

  退出块语句或循环语句。可以用exit.type*level的格式指定从哪种循环退出，要退出几层。type是'when'，'times', 'until'或者'each'等。如果level省略，则只退出包含该exit语句的最内层循环。也可以用exit.label的格式指定所要退出的块语句或循环语句的标号。如果type和label都省略，则退出所有类型循环的指定层或最内层语句块。exit >> value 表达式将指定返回的值。如果省略，默认返回None。

  >>> block.a[ prin(1), exit.a, prin(2) ]
  1

* 重做语句(next语句)。

  格式：next << .label >> | {{ << /type >> << *level >>}}

  重做块语句或循环语句。可以用next.type*level的格式指定从哪种循环退出，要退出几层。type是'loop'（无限循环)，'when'（when-loop或loop-when循环），'times'(loop-times循环), 'until'(loop-until循环)或者'each'(each循环)等。如果level省略，则只重做包含该next语句的最内层循环。也可以用next.label的格式指定所要重做的块语句或循环语句的标号。如果type和label都省略，则重做所有类型循环的指导层或最内层语句块。
  
  >>> do [i<<0, block.a[ ++i, iff(i<3) .do [next.a] ], i ]
  3

  上面的例子说明，利用block, exit 和next语句可以构成循环。下一节将要介绍循环语句，而这些循环语句实际上正是利用block语句以及与exit和next对应的底层命令exit_block和continue_block构造的。

循环
------

* 无限循环语句(loop语句)。

  格式：loop[ 语句... ]

  >>> i << 0  
  >>> loop [ prin(i), ++i, iff(i==3) .do[exit] ]
  0 1 2

* 定数循环语句(loop-times语句)。
  
  格式：loop(次数) [ 语句... ]
  
  >>> loop(3)[ prin(i), --i ]
  3 2 1

* 前置条件循环语句(when-loop语句)。

  格式：when(条件).loop[ 语句... ]

  >>> when(i!=0).loop[ prin(i), ++i]
  0 1 2

* 后置条件循环语句(loop-when语句)。

  格式：loop[ 语句... ].when(条件)

  >>> loop [ prin(i), ++i].when(i<3)
  0 1 2

* 直到型循环语句(loop-until语句)。

  格式: loop[ 语句... ].until(条件)

  >>> do[ i<<3, loop [ prin(i), --i ]. until(i==0)
  3 2 1

* 遍历循环语句(each语句)。

  格式：each(变量!...){{[表达式]!...}}.loop[ 语句... ]

  将变量列表依次与所有范围组合成的元组列表中的每一项绑定。循环执行语句列表。表达式可以是slice类型的值，将转换为range。

  >>> each(i)[0:3].loop[ prin(i) ]
  0 1 2

* 循环标号语句(label语句)。

  格式: label.name%loop语句

  利用label语句，可以给上述循环语句设置标号。exit语句和next语句可以引用这个标号。
  
  >>> label.outer%loop[
        println('outer loop'),
        i << 0,
        label.inner%loop[
          prin('inner loop: '),
          println(i),
          ++i,
          iff(i==3).do[ exit.outer >> 'exit from inner' ] ] ]
  outer loop
  inner loop:  0
  inner loop:  1
  inner loop:  2
  'exit from inner'
  
  这段程序利用exit.outer语句直接跳出外层循环, 并带回返回值'exit from inner'。

  >>> loop[
        println('outer loop'),
        i << 0,
        loop[
          prin('inner loop: '),
          println(i),
          ++i,
          iff(i==3).do[ exit*2 >> 'exit from inner' ] ] ]
  outer loop
  inner loop:  0
  inner loop:  1
  inner loop:  2
  'exit from inner'

  这段程序利用exit*2语句直接跳出两层循环, 并带回返回值'exit from inner'。

异常处理
--------

**(需要实例)**

* 限定执行语句(on语句)。
  格式：on( {变量<<表达式}!... ) .do [ 语句...]

* catch 与 throw

  * 捕获语句(catch语句)。
    
    格式：catch(标记).do[语句...]

  * 抛掷语句(throw语句)。

    格式: throw(标记).do[语句...]

* 保护执行语句(protect语句)。

  格式：protect[语句...].always[语句...]

* python 异常语句(pytry语句)。# 尚未实现

  格式：pytry[语句...].on(ExceptioinClass, e).do[语句...].final[语句...]

  用pytry实现each-loop语句。
