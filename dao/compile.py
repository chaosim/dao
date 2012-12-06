# -*- coding: utf-8 -*-

'''code for compilation:
alpha convert -> cps convert
-> optimization 
-> tail recursive convert
-> pythonize -> generate code
'''

from dao.compilebase import Environment, Compiler, CodeGenerator, OptimizationData
from dao.compilebase import CompileTypeError, VariableNotBound
from dao import interlang as il

prelude = '''# -*- coding: utf-8 -*-
# generated file after compiling dao expression.

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
  exp = il.element(exp)
  exp = exp.alpha_convert(env, compiler)
  exp = exp.cps_convert(compiler, done)
  v = compiler.new_var(il.LocalVar('v'))
  solver_prelude = il.begin(
    il.Assign(il.failcont, il.clamda(v, il.RaiseException(il.Call(il.Symbol("NoSolution"), v)))),
    il.Assign(il.cut_cont,il.failcont),
    il.Assign(il.cut_or_cont,il.failcont),
    il.Assign(il.catch_cont_map, il.empty_dict),
    #il.Assign(il.unwind_cont_stack, il.empty_list),
    #il.Assign(il.exit_block_cont_map, il.empty_dict),
    #il.Assign(il.continue_block_cont_map, il.empty_dict)
    )
  exp = il.begin(solver_prelude, exp)
  data = OptimizationData()
  exp.optimization_analisys(data)
  exp = exp.optimize(data)
  #exp = exp.tail_recursive_convert()
  function = compiler.new_var(il.LocalVar('compiled_dao_function'))
  exp = il.Function(function, (), exp)
  exp = il.begin(*exp.pythonize(env, compiler)[0])
  exp = exp.statements[0]
  exp.body = exp.body.replace_return_with_yield()
  coder = CodeGenerator()
  result = exp.to_code(coder)
  return prelude + result
'''
il.CFunction(block_foo, v1, il.Clamda(v10, il.begin(
  il.Assign(f, v10), 
  il.Clamda(v4, il.Clamda(a0, il.CFunction(block_foo1, v6, il.Clamda(function, 
     function(il.Clamda(a1, il.mul(a0, a1))))(f))(None))(2))(v10)))(
       il.Lamda((cont), il.begin(None, il.Clamda(v2, v2)(1)))
       ))(None)
'''