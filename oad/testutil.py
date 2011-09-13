# -*- coding: utf-8 -*-

from oad.term import Var

k, x, y, n = Var('k'), Var('x'), Var('y'), Var('n')
a, b, f, fac, even, odd, foo = Var('a'), Var('b'), Var('f'), Var('fac'), Var('even'), Var('odd'), Var('foo')

def cleanup_vars():
  for v in [k, x, y, n, a, b, f, fac, even, odd, foo]: v.binding = None
