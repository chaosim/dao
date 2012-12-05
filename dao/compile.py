# -*- coding: utf-8 -*-

'''code for compilation:
alpha convert -> cps convert -> assign convert 
-> optimization 
-> tail recursive convert -> trampoline 
-> pythonize -> generate code
'''

from dao.compilebase import Environment, Compiler, CodeGenerator, OptimizationData
from dao.compilebase import CompileTypeError, VariableNotBound
from dao import interlang as il

prelude = '''# -*- coding: utf-8 -*-
# generated file after compiling dao expression.

# from dao.interlang import Expression
from dao.compilebase import Environment
from dao.builtins import *
from dao.command import LogicVar as DaoLogicVar
from dao.command import Var as DaoVar
from dao.solvebase import Solver, deref, LogicVar, ExpressionWithCode
from dao.solvebase import UnquoteSplice, MacroFunction
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
  if done is None:
    done = il.Done()
  compiler = Compiler()
  if env is None: env = Environment()
  exp = il.element(exp).alpha_convert(env, compiler)
  exp = exp.cps_convert(compiler, done)
  data = OptimizationData()
  exp.optimization_analisys(data)
  exp = exp.optimize(data)
  #exp = exp.tail_recursive_convert()
  function = compiler.new_var(il.LocalVar('compiled_dao_function'))
  exp = il.Function(function, (), exp)
  exp = il.begin(*exp.pythonize_exp(env, compiler)[0])
  exp = exp.statements[0]
  exp.body = exp.body.replace_return_with_yield()
  coder = CodeGenerator()
  result = exp.to_code(coder)
  return prelude + result

'''
il.Clamda(v5, il.begin(
  il.Assign(f, v5), 
  il.Clamda(v, il.Clamda(v4, il.begin(
    il.Assign(x, v4), 
    il.Clamda(v1, il.Clamda(function, 
      function(il.Done(v, v), 
        il.MacroArgs((ExpressionWithCode(add(Var('x'), Var('x')), il.Lamda((), il.Clamda(a0, il.Clamda(a1, il.Clamda(v3, v3)(il.add(a0, a1)))(x))(x))),))))(f))(v4)))(1))(v5)))(il.Lamda((cont, params), il.begin(il.Assign(arity_fun_1, il.Lamda((), il.begin(il.Assign(cut_cont, il.cut_cont), il.Assign(il.cut_cont, il.fail_cont), il.Assign(x1, il.GetItem(params, 0)), il.Clamda(v7, il.Clamda(v10, il.Clamda(a01, il.Clamda(v8, il.Clamda(v6, il.begin(il.Assign(il.cut_cont, cut_cont), cont(v6)))(None))(il.Prin(a01)))(il.EvalExpressionWithCode(v10)))(x1))(True)))), il.Assign(arity_body_map, RulesDict({1: arity_fun_1})), il.If(il.In(il.Len(params), arity_body_map), il.GetItem(arity_body_map, il.Len(params))(), il.fail_cont(None)))))
'''
'''
il.begin(
  il.Assign(arity_body_map, RulesDict({1: il.Lamda((), il.begin(il.Prin(2), None))})), 
  il.If(il.In(il.Len(il.MacroArgs((ExpressionWithCode(add(Var('x'), Var('x')), il.Lamda((), 2)),))), arity_body_map), 
        il.GetItem(arity_body_map, il.Len(il.MacroArgs((ExpressionWithCode(add(Var('x'), Var('x')), il.Lamda((), 2)),))))(), 
        il.fail_cont(None)))
'''