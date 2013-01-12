from dao.compilebase import CompileTypeError
from dao.command import special, Command, SpecialCall, BuiltinFunction
import dao.interlang as il
from dao.interlang import TRUE, FALSE, NONE

add = BuiltinFunction('add', il.add)
sub = BuiltinFunction('sub', il.sub)
mul = BuiltinFunction('mul', il.mul)
div = BuiltinFunction('div', il.div)
isnot = BuiltinFunction('add', il.IsNot)
and_a = BuiltinFunction('eq', il.And)
or_a = BuiltinFunction('sub', il.Or)
lt = BuiltinFunction('mul', il.Lt)
le = BuiltinFunction('div', il.Le)
eq = BuiltinFunction('eq', il.Eq)
ne = BuiltinFunction('ne', il.Ne)
ge = BuiltinFunction('ge', il.Ge)
gt = BuiltinFunction('gt', il.Gt)

@special
def between(compiler, cont, lower, upper, mid):
  lower1 = compiler.new_var(il.ConstLocalVar('lower'))
  upper1 = compiler.new_var(il.ConstLocalVar('upper'))
  mid1 = compiler.new_var(il.ConstLocalVar('mid'))
  fc = compiler.new_var(il.ConstLocalVar('fc'))
  i = compiler.new_var(il.Var('i'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  return lower.cps_convert(compiler, il.clamda(lower1,
    upper.cps_convert(compiler, il.clamda(upper1,
    mid.cps_convert(compiler, il.clamda(mid1,
        il.If(il.IsLogicVar(mid1),
          il.begin(
            il.Assign(i, lower1),
            il.Assign(fc, il.failcont),
            il.SetFailCont(il.clamda(v, 
              il.If(il.Eq(i, upper1),
                il.Begin((
                  il.Assign(il.failcont, fc),
                  fc(il.FALSE))),
                il.Begin((
                  il.AddAssign(i, il.Integer(1)),
                  il.SetBinding(mid1, i),
                  cont(il.TRUE)))))),                
            il.SetBinding(mid1, lower1),
            cont(il.TRUE)),
          il.If(il.Cle(lower1, mid1, upper1),
            cont(il.TRUE),
            il.failcont(il.FALSE)))))
    ))))    

'''
_op_precedence = {'lt':10, 'le':10, 'eq':10, 'ne':10, 'ge':10,'gt':10,
           'add':60, 'sub':60,'mul':70, 'div':70,
           'floordiv':70, 'truediv':70,'mod':70,'pow':100,
           'and_':40, 'xor':30,'or_':20,
           'lshift':50, 'rshift':50,
           'pos':80,'neg':80,'invert':90}

def _operator_repr(oprand, operator):
  if isinstance(oprand, OperatorCall):
    if _op_precedence[oprand.operator.name]<_op_precedence[operator.name]:
      return '(%s)'%oprand
  return '%s'%oprand

class BinaryCall(OperatorCall): 
  def __repr__(self):
    if run_mode() is interactive:
      code = interactive_parser().parse(self)
      code = interactive_tagger().tag_loop_label(code)
      code = to_sexpression(code)
      result = interactive_solver().eval(code)
      return repr(result) if result is not None else ''
    x = _operator_repr(self.operand[0], self.operator)
    y = _operator_repr(self.operand[1], self.operator)
    return '%s%s%s'%(x, self.operator.symbol, y)

class UnaryCall(OperatorCall): 
  def __repr__(self):
    if run_mode() is interactive:
      code = interactive_parser().parse(self)
      code = interactive_tagger().tag_loop_label(code)
      result = interactive_solver().eval(code)
      return repr(result) if result is not None else ''
    x = _operator_repr(self.operand[0], self.operator)
    return '%s%s'%(self.operator.symbol, x)

class BuiltinBinaryCont:
  def __init__(self, operator, operands):
    self.operator, self.operands = operator, operands
  def code(self):
    return '%s(%s, %s)'%(code(self.operator), code(self.operands[0]), code(self.operands[1]))
    
class BuiltinBinary(builtin.BuiltinFunction):
  def __call__(self, x, y): return BinaryCall(self, x, y)
  #def compile_to_cont(self, cont, compiler):
    #return BuiltinBinaryCont(self, cont)
  def code(self): return self.name

class BuiltinUnary(builtin.BuiltinFunction):
  def __call__(self, x): return UnaryCall(self, x)

binary = builtin.builtin(BuiltinBinary)
unary = builtin.builtin(BuiltinUnary)

@builtin.function('not_', 'not_')
def not_(value):
  return not value

@binary('lt', '<', is_global=True)
def lt(x, y): return operator.lt(x, y)  
@binary('le', '<=')
def le(x, y): return operator.le(x, y)  
@binary('eq', '==')
def eq(x, y): return operator.eq(x, y)  
@binary('ne', '!=')
def ne(x, y): 
  return operator.ne(x, y)  
@binary('gt', '>')
def gt(x, y): return operator.gt(x, y)  
@binary('ge', '>=')
def ge(x, y): return operator.ge(x, y)  
@binary('getattr', '.')
def getattr(x, y): return operator.getattr(x, y)  
@binary('getitem', '[ ]')
def getitem(x, y): return operator.getitem(x, y)

@set_type(type.Function(type.atom))
@binary('add', '+', is_global=True)
def add(x, y): return operator.add(x, y)  

@set_type(type.Function(type.atom))
@binary('add', '+', is_global=True)
@binary('sub','-')
def sub(x, y): return operator.sub(x, y)

@binary('mul', '*')
def mul(x, y): return operator.mul(x, y)  
@binary('floordiv', '/')
def floordiv(x, y): return operator.floordiv(x,y)  
@binary('div', '/')
def div(x, y): return operator.div(x, y)  
@binary('truediv', '//')
def truediv(x, y): return operator.truediv(x, y)  
@binary('mod', '%')
def mod(x, y): return operator.mod(x, y)  
@binary('pow', '**')
def pow(x, y): return operator.pow(x, y)  
@binary('lshift', '<<')
def lshift(x, y): return operator.lshift(x, y)
@binary('rshift', '>>')
def rshift(x, y): return operator.rshift(x, y)  
@binary('and_', '&')
def and_(x, y): return operator.and_(x, y)  
@binary('xor', '^')
def xor(x, y): return operator.xor(x, y)  
@binary('or_', '|')
def or_(x, y): return operator.or_(x, y)

@builtin.function('iter')
def iter(x): return operator.iter(x) 
@unary('neg', 'neg')
def neg(x): return operator.neg(x)  
@unary('pos', '+')
def pos(x): return operator.pos(x)  
@builtin.function('abs')
def abs(x): return operator.abs(x)  
@binary('invert', '~')
def invert(x): return operator.invert(x)

@builtin.predicate('equal', '=!')
def equal(solver, left, right):
  if deref(left, solver.env)==deref(right, solver.env): 
    return True
  else:
    solver.scont = solver.fcont

def arith_predicate(binary, name, symbol):
  @builtin.predicate(name, symbol)
  def pred(solver, value0, value1):
    if binary(value0, value1): return True
    else: solver.scont = solver.fcont
  return pred

eq_p = arith_predicate(operator.eq, 'eq_p', '==!')
ne_p = arith_predicate(operator.ne, 'ne_p', '!=!')
lt_p = arith_predicate(operator.lt, 'lt_p', '<!')
le_p = arith_predicate(operator.le, 'le_p', '<=!')
gt_p = arith_predicate(operator.gt, 'gt_p', '>!')
ge_p = arith_predicate(operator.ge, 'ge_p', '>=!')

#arith_builtins = builtin.collocet_builtins(globals())
#print arith_builtins
'''