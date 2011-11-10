from dao import builtin
from dao.term import Var, ClosureVar, deref, getvalue, CommandCall
import operator
from dao.solve import run_mode, interactive, to_sexpression
from dao.solve import interactive_solver, interactive_tagger, interactive_parser

class OperatorCall(CommandCall): pass

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

class BuiltinBinary(builtin.BuiltinFunction):
  def __call__(self, x, y): return BinaryCall(self, x, y)

class BuiltinUnary(builtin.BuiltinFunction):
  def __call__(self, x): return UnaryCall(self, x)

binary = builtin.builtin(BuiltinBinary)
unary = builtin.builtin(BuiltinUnary)

@builtin.function('not_', 'not')
def not_(value):
  return not value

@binary('lt', '<')
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
@binary('add', '+')
def add(x, y): return operator.add(x, y)  
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
@unary('neg', '-')
def neg(x): return operator.neg(x)  
@unary('pos', '+')
def pos(x): return operator.pos(x)  
@builtin.function('abs')
def abs(x): return operator.abs(x)  
@binary('invert', '~')
def invert(x): return operator.invert(x)

@builtin.predicate()
def between(solver, cont, *exps):
  lower, upper, mid = exps
  lower = deref(lower, solver.env)
  if isinstance(lower, Var): error.throw_instantiation_error()
  upper = deref(upper, solver.env)
  if isinstance(upper, Var): error.throw_instantiation_error()
  mid = deref(mid, solver.env)
  if not isinstance(mid, Var):
    if lower<=mid<=upper: yield cont, True
    else: return
  for x in range(lower, upper+1):
    for y in mid.unify(x, solver.env): yield cont, True

@builtin.predicate('equal', '=!')
def equal(solver, cont, left, right):
  if deref(left, solver.env)==deref(right, solver.env): 
    yield cont, True

def arith_predicate(binary, name, symbol):
  @builtin.macro(name, symbol)
  def pred(solver, cont, var0, var1):
    if binary(getvalue(var0, solver.env), 
                getvalue(var1, solver.env)):
      yield cont, True
  return pred

eq_p = arith_predicate(operator.eq, 'eq_p', '==!')
ne_p = arith_predicate(operator.ne, 'ne_p', '!=!')
lt_p = arith_predicate(operator.lt, 'lt_p', '<!')
le_p = arith_predicate(operator.le, 'le_p', '<=!')
gt_p = arith_predicate(operator.gt, 'gt_p', '>!')
ge_p = arith_predicate(operator.ge, 'ge_p', '>=!')

#arith_builtins = builtin.collocet_builtins_to_module(globals())
#print arith_builtins
