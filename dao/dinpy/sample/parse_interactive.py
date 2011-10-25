from dao.dinpy import *

print set_parse_state(('abc', 0))
print unify_parse_state(('abc', x))
print x
print get_parse_state()
print char(y)
print y
print set_text('abc')
print char(z)
print get_parse_state()
print z

print parse(char('a'), ('abc',0))
print parse(char('a'), ('abc',1))