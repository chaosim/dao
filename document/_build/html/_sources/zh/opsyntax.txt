运算符文法
*********

Dao系统一直是我所追求的梦想，而Dinpy和算符文法则完全来自于偶然的灵感。也可以看作老天对我所作努力的额外奖励。回忆灵光一闪的那一刹那是很愉快的事情：当时我需要为let语句写测试，但是因为括号太多了，我想将let(a,1), (b,2)), write(1))写成这样let({a:1, b:2}, write(1))，让它变得更易读易写一点。（后来我意识到给let选用这样一个式子是不合适的，因为它导致无法确定绑定次序。但是我依然必须非常感谢这一闪念。）。突然我想到可以充分利用python的运算符重载定义我所需要的语法，于是创意的大门打开了。我一开始不太确信在pythond环境中这种方法到底能有多远，于是就建立了一个空的.py文件，在里面信手涂鸦，结果发现能做的事情超乎想象。接下来我就开始考虑如何实现了。这里我倒是走了不少弯路，也可能是必须经历的弯路。这个实现过程再一次使我体会到山重水复疑无路，柳暗花明又一村，踏破铁鞋无觅处，得来全不费工夫的感觉。

现在讲述一下实现过程。我先试了一下实现do[body].until(condition)。我手工写了几个类，发现相当繁琐。我希望能够自动产生这些类，于是很自然地想到采用元类技术。我了解过关于元类的知识，但是还没有在项目中使用过。因为Tim Peters 说过的这样的话: "Metaclasses are deeper magic than 99% of users should ever worry about. If you wonder whether you need them, you don't (the people who actually need them know with certainty that they need them, and don't need an explanation about why)." “元类是99%的用户不必操心的深奥魔法。如果你怀疑是否有必要使用它们，那么你就不需要。那些实际需要它们的人知道他们需要使用元类的确定性，不需要解释原因。”我认为现在应该是我确定必须要用元类的时候了我想这一次遇到了必须使用元类的问题。我设计的解决方案是：将python运算符表达式的正规文法转换成自动机，运算符成为自动机状态之间的弧。我希望根据这些弧将对应状态自动产生相应的类。然而，实现过程出乎意料的困难，有很多技术问题涌现出来：例如，一个运算符会从一个状态转换到多个状态，将语义动作附加到产生的类，等等。我艰苦地工作了二十天，结果确实发现存在的问题比我想象的更难克服。就在我看不到希望准备放弃的黑暗时刻，我突然想到了一个超级简单的解决方案：两步走，第一步只遍历表达式，收集运算符和操作数。第二步，进行语法解析，产生最终结果。通过直接运用dao系统既有的解析能力，这种方法也成为dao系统的一个自举bootstrap实现。采用这种方法，我只用了三小时就完整地实现了方案的框架代码和主要功能，而系统的能力或灵活性超乎预想，并且最终说明，我还是不需要用到元类（这不是真的，看看pysyntax.py的代码，你将发现元类实际出现的那个场所）。真是令人既感慨又兴奋，收获多多。

目前，Dinpy的实现就是python运算符文法的一个实例。有了dao系统，我们为python实现了运算符文法。利用运算符文法，又为dao系统设计了dinpy语言。这就是自举(bootstrap)和元环(metacircular)的力量。

运算符匹配器
-----------

先来看看pysyntax.py提供的简单例子。假如我们要定义类似let(bindings).do[body]的表达式，可以通过下面的代码实现：

>>> from oad.term import Var
>>> from oad.builtins.terminal import eos
>>> from oad.builtins.term import pytuple, first
>>> bindings, body = Var('bindings'), Var('body')
>>> do = word('do')
>>> let = element('let', call(bindings)+do+getitem(body)+eos+pytuple(first(bindings), body))
>>> preparse(let({'x':1}).do[1,2])
({'x': 1}, (1, 2))

上述代码最关键的是let = element('let', call(bindings)+do+getitem(body)+eos+pytuple(first(bindings), body))这一语句。其中call(bindings)与let(bindings)这一__call__方法匹配；do, 即word('do')相当于getattr('do')，与__getattr__('do')匹配，也就是.do字段；getitem(body)与__getitem__(body)匹配。eos成功表明运算符序列已经结束。至于pytuple(first(bindings), body))是语义动作，返回所需结果。preparse(let({'x':1}).do[1,2])将调用求解器执行语法解析，返回解析结果。

在pysyntax.py中，对所有可以重载的表达式运算符都定义了解析命令，它们是::

  binary(arg=None):匹配二元运算符
  unary()：匹配一元运算符

  lt = binary(__lt__) 匹配<, 小于
  le = binary(__le__) 匹配<=，小于等于
  eq = binary(__eq__) 匹配==，等于  
  ne = binary(__ne__) 匹配!=, <>，不等于
  gt = binary(__gt__) 匹配>，大于
  ge = binary(__ge__) 匹配>=，大于等于
  bitor = binary(__or__)  匹配|，位或
  xor = binary(__xor__) 匹配^，位异或
  bitand = binary(__and__) 匹配&，位与
  lshift = binary(__lshift__) 匹配<<，左移
  rshift = binary(__rshift__) 匹配>>，右移
  add = binary(__add__) 匹配+，加
  sub = binary(__sub__) 匹配-，减
  mul = binary(__mul__) 匹配*，乘
  div = binary(__div__) 匹配/，除
  floordiv = binary(__floordiv__) 匹配//，整除
  mod = binary(__mod__) 匹配%，取余
  pos = unary(__pos__)() 匹配+x, negative，负 
  neg = unary(__neg__)() 匹配-x, Positive，正
  invert = unary(__invert__)() 匹配~x	Bitwise not
  abs = unary(__abs__)() 匹配abs()，绝对值
  pow = binary(__pow__) 匹配**	Exponentiation，指数
  getattr = binary(__getattr__) 匹配 attribute access，属性访问
  getitem = binary(__getitem__) 匹配object[index]，索引
  iterator = unary(__iter__)，遍历
  call(args=None, kwargs=None)：匹配__call__

  def word(word): return getattr(word)
  def words(text): return [getattr(w.strip()) for w in text.split(',')] 
  def attr_item(name): return lambda arg: getattr(name),getitem(arg))
  def attr_call(name): return lambda *args: and_p(getattr(name), call(*args))

  def getitem_to_list(argument=None):与__getitem__(value)匹配，如果value不是列表，则argument与[value]合一, 否则与value合一。

从pysyntax看到，它所解析的对象实际上是(运算符，操作数)的列表。这从实例上说明了dao的解析功能和解析命令可以轻松地应用于解析非文本对象。

再看一个比较复杂一点的例子，dao.dinpy.dinpy模块中利用运算符文法定义loop语句的实例::

  when_fun = attr_call('when')
  until_fun = attr_call('until')

  # loop[write(1)], loop(3)[write(1)]
  # loop [write(1)].until(1), loop[write(1)].when(1)
  loop = element('loop',
    (  # loop(1)[write(1)]
      (call(vv.times)+getitem_to_list(vv.body)+eos
        +make_loop_times(vv.body, getvalue_default(vv.times)))
      # loop[...], loop[...].when(), loop[...].until() 
    | ( getitem_to_list(vv.body)+
        ( # loop [...] # infinite loop
          ( eos+make_loop(vv.body))
          # loop[...].when(), loop[...].until() 
        | ( # .when(1)
            ( when_fun(vv.test)+eos
                +make_loop_when(vv.body, vv.test))
            #.until(1)
           |( until_fun(vv.test)+eos
                +make_loop_until(vv.body, vv.test) ) )  
    ) ) ) )

从dinpy.py文件可以看到运算符文法(pysyntax.py)的能力以及易学易用的特点。

dinpy的整个语法实现主要是利用运算符文法，除此以外还有其它几个部件的配合：dexpr.py，使得可以直接通过+-×/等运算符直接调用dao.builtins.arith中的OperatorCall；另外还有一些简单的语法是通过直接定义类的方法实现的，比如v.i, var.i.j。 dinpy充分展示了python定义领域特定语言(domain specific language, dsl)的能力。有了dinpy的帮助，特别是它的运算符文法(pysyntax.py)，你也能够根据需要轻松定义你自己的dsl。充分发挥你的创意和想象力，大胆地创造吧。
