This is Dao version 0.7.0
============================

what would happen when lisp meets prolog in python?  

Dinpy arises!

Dao is the new generation programming system implemented by a functional logic solver, 
unifying code with data, grammar with program, logic with functional, compiling with running.

What's new in this release?
---------------------------

See the file "news.txt" for what's new in all releases.

Download and install
----------------------

  You need python 2.6 or higher version to install and run dao. Dao is not tested with python 3.x.

  You can download and install dao from pypi with these method:

* find the place of daot from pypi, download the tarball of daot from (http://code.google.com/p/daot)，unextract it to a single folder, enter the folder in the shell and execute the command::

  python setup.py install

* install by easy_install through the internet directly, with the shell command::

  easy_install dao

* install by easy_install through the internet directly,  with the shell command::

  pip install dao

How to use Dao?
------------------

See the "get start" in the http://code.google.com/p/daot/w/list to get started with dao and dinpy.
Download the document and for more completly information about dao and dinpy. 
See the tests, and you'll get some information about the interface and user cases.

Have a look at running Dinpy in interactive mode in the python shell below::

>>> from dao.dinpy import *

>>> i<<0
0
>>> lshift(i, 1)
0

>>> quote(i)
i
>>> quote(i/i+i*1)
i/i+i*1
>>> quote((i+i)*(i+1))
(i+i)*(i+1)

>>> let(i<<1).do[prin(i)]
1
>>> let(i<<1).do[let(i<<2).do[ prin(i) ], prin(i)]
2 1

>>> loop(3)[ prin(i), ++i ]
0 1 2
>>> loop [ prin(i), --i]. until(i==0)
3 2 1
>>> loop [ prin(i), ++i].when(i<3)
0 1 2
>>> when(i!=0).loop[ prin(i), --i]
3 2 1
>>> loop [ prin(i), ++i, iff(i==3) .do[exit] ]
0 1 2
>>> each(i)[0:3].loop[prin(i)]
0 1 2
>>> i << 0
0
>>> label.outer%loop[ println('outer loop'), label.inner%loop[ prin('inner loop: '), println(i), ++i, iff(i==3).do[ exit*2 >> 'exit from inner' ] ] ]
outer loop
inner loop:  0
inner loop:  1
inner loop:  2
'exit from inner'
>>> i << 0
0
>>> label.outer%loop[ println('outer loop'), label.inner%loop[ prin('inner loop: '), println(i), ++i, iff(i==3).do[ exit.outer >> 'exit from inner' ] ] ]
outer loop
inner loop:  0
inner loop:  1
inner loop:  2
'exit from inner'

>>> fun.f1(x) == [x+x]
f1
>>> f1(2)
4

>>> letr (f2 << fun(x)[ iff(x<1).do[x].els[f2(x-1)]]) .do [f2(3)]
0

>>> fun.f()==[prin('f():'), prin(4)]
f
>>> f()
f(): 4


Documentation
-------------

The document of dao can be download from (http://code.google.com/p/daot).
See https://github.com/chaosim/dao/wiki for online document.

Testing
-------

dao use the nose test framework, the code in dao is tested with many tests.


Web sites
---------

New dao releases, documents and related stuffs are published at
http://code.google.com/p/daot. 

the project's repository is on github (https://github.com/chaosim/dao). You can get the source code that is in continuous development.

Come to visit us!

Bug reports
-----------

To report or search for bugs, please use the wiki at 
http://code.google.com/p/daot, or email to simeon.chaos@gmail.com

Platform notes
--------------

dao is developed and tested on Windows XP, Python 2.6.6.

License information
-------------------

copyright (c) 2011-2013 曹星明(Simeon Chaos, email: simeon.chaos@gmail.com)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

This software is released under GNU General Public License version 3. 
see license.txt

See the file "license.txt" for information on the history of this
software, terms & conditions for usage, and a DISCLAIMER OF ALL
WARRANTIES.

This dao distribution contains GNU General Public Licensed (GPLed) code.

All trademarks referenced herein are property of their respective holders. 


That's all, folks!

              Your Sincerely
             -- 曹星明(Simeon Chaos, simeon.chaos@gmail.com)
