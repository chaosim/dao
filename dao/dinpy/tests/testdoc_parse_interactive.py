# doctest

'''
>>> from dao.dinpy import *
>>> set_parse_state(('abc', 0))
('abc', 0)
>>> unify_parse_state(('abc', x))
('abc', 0)
>>> x
0
>>> get_parse_state()
('abc', 0)
>>> char(y)
'a'
>>> y
'a'
>>> set_text('abc')
('abc', 0)
>>> char(z)
'a'
>>> get_parse_state()
('abc', 1)
>>> z
'a'
>>> parse(char('a'), ('abc',0))
'a'
>>> from nose.tools import assert_raises
>>> from dao.solve import NoSolutionFound
>>> assert_raises(NoSolutionFound, repr, parse(char('a'), ('abc',1)))
'''

if __name__ == "__main__":
  import doctest
  doctest.testmod()