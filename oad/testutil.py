# -*- coding: utf-8 -*-

from oad.term import Var, DummyVar

k, x, y, z, n, i, j = Var('k'), Var('x'), Var('y'), Var('z'), Var('n'), Var('i'), Var('j')
a, b, f, fac, even, odd, foo = Var('a'), Var('b'), Var('f'), Var('fac'), Var('even'), Var('odd'), Var('foo')

def xxxcleanup_vars():
  for v in [k, x, y, z, n, a, b, f, fac, even, odd, foo]: v.binding = None

def vars(names): return [Var(x.strip()) for x in names.split(',')]
def dummies(names): return [DummyVar(x.strip()) for x in names.split(',')]
