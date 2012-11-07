class TestTrampoline:
  def test_var(self):
    result = trampoline(Clamda(v, fc, done()(2, None))(1, None))
    expect = Clambda(v, fc, il.Return(Clamda(v, fc,  il.Return(done(), (2, None))), (1, None)))
    eq_(result, expect)

  def test_lambda(self):
    x, y, k = il.Var('x'), il.Var('y'), il.Var('k')
    result = trampoline(done()(lamda((x, y, k), k(1, None)), None))
    expect = Clamda(v, fc, il.Return(done, lamda((x, y, k), il.Return(k, (1, None)))))
    eq_(result, expect)