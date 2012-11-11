class TestTrampoline:
  def test_var(self):
    result = trampoline(Clamda(v, done()(2, None))(1, None))
    expect = 1
    eq_(result, expect)

  def test_lambda(self):
    x, y, k = il.Var('x'), il.Var('y'), il.Var('k')
    result = trampoline(done()(lamda((x, y, k), k(1, None)), None))
    expect = 1
    eq_(result, expect)