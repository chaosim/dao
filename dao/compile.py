# -*- coding: utf-8 -*-

'''code for compilation:
alpha convert -> cps convert
-> optimize
-> tail recursive convert
-> pythonize -> generate code
'''

from dao.compilebase import Environment, Compiler
from dao.command import element
from dao.compilebase import CompileTypeError, VariableNotBound
from dao import interlang as il

prelude = '''# -*- coding: utf-8 -*-
# generated file from compiling dao expression.

from dao.builtins import *
from dao.command import LogicVar as DaoLogicVar
from dao.command import Var as DaoVar
from dao.solvebase import Solver, deref, LogicVar, ExpressionWithCode
from dao.solvebase import UnquoteSplice, MacroFunction, NoSolution
from dao.solve import eval as eval_exp
from dao.command import BuiltinFunctionCall
from dao import interlang as il

solver = Solver()

'''

def compile_to_pyfile(exp, env):
  code = compile_to_python(exp, env)
  file = open(r'f:\dao_all\dao\dao\tests\compiled.py', 'w')
  file.write(code)
  file.close()

def compile_to_python(exp, env, done=None): 
  '''assemble steps from dao expression to python code'''
  original_exp = exp
  compiler = Compiler()
  if done is None:
    done = il.Done(compiler.new_var(il.LocalVar('v')))
  compiler.exit_block_cont_map = {}
  compiler.continue_block_cont_map = {}
  compiler.protect_cont = done
  if env is None: env = Environment()
  exp = element(exp)
  exp = exp.alpha_convert(env, compiler)
  exp = exp.cps_convert(compiler, done)
  v = compiler.new_var(il.LocalVar('v'))
  solver_prelude = il.begin(
    il.Assign(il.parse_state, il.NONE),
    il.Assign(il.failcont, 
        il.clamda(v, 
              il.RaiseException(il.Call(il.Symbol("NoSolution"), v)))),
    il.Assign(il.cut_cont,il.failcont),
    il.Assign(il.cut_or_cont,il.failcont),
    il.Assign(il.catch_cont_map, il.empty_dict),
    )
  exp = il.begin(solver_prelude, exp)
  exp.local_vars = set()
  compiler.lamda_stack = [exp]
  exp.analyse(compiler)
  env = Environment()
  exp = exp.optimize(env, compiler)
  #exp = exp.tail_recursive_convert()
  function = compiler.new_var(il.LocalVar('compiled_dao_function'))
  exp = il.Function(function, (), exp)
  exp = il.begin(*exp.pythonize(env, compiler)[0])
  if isinstance(exp, il.Begin):
    exp = exp.statements[0]
  exp.body = exp.body.replace_return_with_yield()
  compiler = Compiler()
  result = exp.to_code(compiler)
  return prelude + result
'''
il.begin(il.Assign(a0, il.UnquoteSplice(ExpressionWithCode(Tuple((3, 4)), 
il.Lamda((), il.Tuple((3, 4)))))), il.Assign(result, []), 
il.If(il.Isinstance(a0, il.Klass(UnquoteSplice)), 
il.Assign(result, il.add(result, il.Call(list, il.Attr(a0, item)))), 
il.ListAppend(result, a0)), il.Call(il.Klass(BuiltinFunctionCall), il.QuoteItem(add), il.MakeTuple(result)))
'''