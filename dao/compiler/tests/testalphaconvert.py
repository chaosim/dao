class TestAlphaConvert:
  def test_var(self):
    x = il.Var('x')
    env = AlphaConvertEnvironment()
    assert_raises(VariableNotBound, env.alpha_convert, x)

  def test_lamda(self):
    x, x1, y, y1, k = il.Var('x'), il.Var('x1'), il.Var('y'), il.Var('y1'), il.Var('k')
    env = AlphaConvertEnvironment()
    eq_(env.alpha_convert(lamda((x,y), 1)), lamda((x,y), 1))
    
  def test_lamda2(self):
    x, x1, y, y1, k = il.Var('x'), il.Var('x1'), il.Var('y'), il.Var('y1'), il.Var('k')
    env = AlphaConvertEnvironment()
    eq_(env.alpha_convert(lamda((x,y), lamda((x,), x, y), x)), lamda((x,y), lamda((x1,), x1, y), x))


