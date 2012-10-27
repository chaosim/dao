from dao.dinpy import *

print unbind(x)
print free(x)
print unify(x,1)
print free(x)
print set_parse_state(('abc', 0))

# x:1, failed to unify with 0.
#print unify_parse_state(('abc', x))

print x
print get_parse_state()
print char(y)
print y
print set_text('abc')
print char(z)
print get_parse_state()
print z

print parse(char('a'), ('abc',0))

# should raise NoSolutionFound
#print parse(char('a'), ('abc',1))