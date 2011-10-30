利用dao和dinpy创建新语言
-------------------------

利用dao的强大功能，可以轻松创造新的语言。而且，在这个新的语言里，你还可以给它赋予动态的语法，使得程序员可以魔法般地控制代码的写法。

动态语法
--------

程序员自主定义语法

rebol的方言

dao的能力使得创造新语法比rebol更加容易。


http://lambda-the-ultimate.org/node/3281#comment-48286

http://logix-language.sourceforge.net/tutorial/contents.html

http://c2.com/cgi/wiki?LogixLanguage

http://www.nongnu.org/l-lang/
::

    parse_macro while '(' @(Expression(Bool) condition) ')' @(Statement body)
    { 
      Statement { 
	    loop { if(!($condition))
		     break;
		   $body }}
    }

http://metalua.luaforge.net/
http://metalua.luaforge.net/src/samples/pythonic.lua.html
pysample.lua
$SUBTITLE$

::

	 This file requires an alternative lexer, therefore it must be
	 compiled that way:

	> mlc -x pythonic.lua pysample.lua

	 Or, if you want to execute it right away:

	 > mlc -x pythonic.lua -x pysample.lua

	print "Eratosthenes' Sieve, in pythonic Metalua:"

	function print_sieve (limit):
	  local sieve, i = { }, 2
	  while i<limit:
	    while sieve[i]:
	      i=i+1
	    print(i)
	    for j = i*i, limit, i:
	      sieve[j] = true
	    i=i+1

	print_sieve(100)

	the "-{...}" means that we're going to do compile-time
	stuff (here, syntax extension) 
	-{ block:
	    Register the additional keywords in mlp.lexer
	   mlp.lexer:add{ "let", "in" }

	    Extend the expression parser; code generation is
	    delegated to the function let_in_builder() below.
	   mlp.expr:add{ 
	     "let", mlp.id, "=", mlp.expr, "in", mlp.expr, 
	     builder = let_in_builder }

	    -- This creates the code returned by the macro.
	   -- Notice the holes-in-quote-in-splice.
	   local function let_in_builder (x)
	     local variable, value, expr = unpack (x)
	     return +{
	       function (-{variable}) 
		 return -{expr} 
	       end (-{value}) }
	   end
	} -- back to "normal" code

	a, b, c = 1, 1, -2 
	roots = let sqrt_delta = (b^2-4*a*c)^0.5 in 
		{ (sqrt_delta-b)/(2*a), (-sqrt_delta-b)/(2*a) } 

http://felix-lang.org/


Katahdin  
http://www.chrisseaton.com/katahdin/

Converge
