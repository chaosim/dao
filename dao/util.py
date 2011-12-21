# -*- coding: utf-8 -*-

from dao.term import var, DummyVar

k, x, y, z, n, i, j = var('k'), var('x'), var('y'), var('z'), var('n'), var('i'), var('j')
a, b, f, fac, even, odd, foo = var('a'), var('b'), var('f'), var('fac'), var('even'), var('odd'), var('foo')

def xxxcleanup_vars():
  for v in [k, x, y, z, n, a, b, f, fac, even, odd, foo]: v.binding = None

