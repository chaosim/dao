附录一.  “Hello world!” 之 道名变
***********************************

http://www.roesler-ac.de/wolfram/hello.htm

汇编
-----
 Org 0
   mov dptr,#msg
   mov R0,#30h  
loop:
   clr a
   movc a,@a+dptr
   jz end
   mov @R0,a
   inc R0
   inc dptr
   sjmp  loop
end: jmp $ 
msg: db 'Hello World',0	
    
C语言
-----

#include <stdio.h>
 main()
 { printf("hello, world\n");
 }


Lisp
--------

(defun helloworld ()
  (print "Hello World!")
)


Haskell
--------

main = putStrLn "Hello World"


Prolog
------------

hello :- display('Hello World!') , nl .


python语言
-----------

print "hello, world\n"

dinpy语言
----------

dao[ prin("hello, world\n") ]

dao语言
----------

print hello world!
