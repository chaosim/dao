# -*- coding: utf-8 -*-

'''code for compilation:
alpha convert -> cps convert -> assign-convert 
-> optimization 
-> tail recursive convert -> trampoline 
-> pythonize -> generate code
'''

from dao.compilebase import Environment, Compiler, CodeGenerator, OptimizationData
from dao.compilebase import CompileTypeError, VariableNotBound
from dao.compilebase import alpha_convert, cps_convert, assign_convert
from dao.compilebase import optimization_analisys, optimize, tail_recursive_convert, trampoline
from dao.compilebase import insert_return_yield, pythonize, to_code
from dao import interlang as il

prelude = '''from dao.interlang import LogicVar
from dao.solvebase import Solver

solver = Solver()

'''

def compile_to_python(exp, done=None):  
  if done is None:
    done = il.Done()
  compiler = Compiler()
  env = Environment()
  exp = alpha_convert(exp, env, compiler)
  exp = cps_convert(compiler, exp, done)
  exp = assign_convert(exp, {}, compiler)
  data = OptimizationData()
  optimization_analisys(exp, data)
  exp = optimize(exp, data)
  function = compiler.new_var(il.Var('compiled_dao_function'))
  v = compiler.new_var(il.Var('v'))
  if isinstance(exp, tuple):
    exp = il.CFunction(function, v, insert_return_yield(il.begin(*exp), il.Yield))
  else:
    exp = il.CFunction(function, v, insert_return_yield(il.begin(exp), il.Yield))
  exp = tail_recursive_convert(exp)
  exp = trampoline(exp)
  exp = pythonize(exp, env, compiler)
  coder = CodeGenerator()
  result = to_code(coder, exp)
  return prelude + result

def compile_to_pyfile(exp):
  file = open(r'f:\dao_all\dao\dao\tests\compiled.py', 'w')
  file.write(compile_to_python(exp))
  file.close()

