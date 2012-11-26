# -*- coding: utf-8 -*-

'''code for compilation:
alpha convert -> cps convert -> assign convert 
-> optimization 
-> tail recursive convert -> trampoline 
-> pythonize -> generate code
'''

from dao.compilebase import Environment, Compiler, CodeGenerator, OptimizationData
from dao.compilebase import CompileTypeError, VariableNotBound
from dao.compilebase import optimize
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

def pythonize(exp, env, compiler):
  exps, has_any_statement = exp.pythonize_exp(env, compiler)
  return il.begin(*exps)

def compile_to_python(exp, env, done=None): 
  '''assemble steps from dao expression to python code'''
  if done is None:
    done = il.Done()
  compiler = Compiler()
  if env is None: env = Environment()
  exp = il.element(exp).alpha_convert(env, compiler)
  exp = exp.cps_convert(compiler, done)
  function = compiler.new_var(il.LocalVar('compiled_dao_function'))
  exp = il.Function(function, (), exp)
  #exp = assign_convert(exp, {}, compiler)
  data = OptimizationData()
  exp.optimization_analisys(data)
  exp = optimize(exp, data)
  #exp = exp.tail_recursive_convert()
  #exp = exp.trampoline()
  exp = pythonize(exp, env, compiler)
  exp = exp.statements[0]
  #if isinstance(exp.body, il.Begin) and isinstance(exp.body.statements[0], il.GlobalDecl):
    #exp.body.statements = exp.body.statements[1:]
  exp.body = exp.body.replace_return_with_yield()
  coder = CodeGenerator()
  result = exp.to_code(coder)
  return prelude + result

def compile_to_pyfile(exp, env):
  code = compile_to_python(exp, env)
  file = open(r'f:\dao_all\dao\dao\tests\compiled.py', 'w')
  file.write(code)
  file.close()

def cps_to_python(exp, compiler, done=None):
  function = compiler.new_var(il.LocalVar('compiled_dao_function'))
  exp = il.Function(function, (), exp)
  #exp = assign_convert(exp, {}, compiler)
  data = OptimizationData()
  exp.optimization_analisys(data)
  exp = optimize(exp, data)
  #exp = exp.tail_recursive_convert()
  #exp = exp.trampoline()
  env = Environment()
  exp = pythonize(exp, env, compiler)
  exp = exp.statements[0]
  exp.body = exp.body.replace_return_with_yield()
  coder = CodeGenerator()
  result = exp.to_code(coder)
  return prelude + result 

def cps_to_pyfile(exp, compiler):
  code = cps_to_python(exp, compiler)
  file = open(r'f:\dao_all\dao\dao\tests\compiled.py', 'w')
  file.write(code)
  file.close()
  
