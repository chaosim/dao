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
from dao.interlang import LogicVar
from dao.command import LogicVar as DaoLogicVar
from dao.solvebase import Solver, deref

solver = Solver()

'''

def pythonize(exp, env, compiler):
  exps, has_any_statement = exp.pythonize_exp(env, compiler)
  return il.begin(*exps)

def compile_to_python(exp, done=None): 
  '''assemble steps from dao expression to python code'''
  if done is None:
    done = il.Done()
  compiler = Compiler()
  env = Environment()
  exp = exp.alpha_convert(env, compiler)
  exp = exp.cps_convert(compiler, done)
  function = compiler.new_var(il.Var('compiled_dao_function'))
  exp = il.Function(function, (), exp)
  #exp = assign_convert(exp, {}, compiler)
  data = OptimizationData()
  exp.optimization_analisys(data)
  exp = optimize(exp, data)
  exp.body = exp.body.insert_return_yield(il.Yield)
  exp = exp.tail_recursive_convert()
  exp = exp.trampoline()
  exp = pythonize(exp, env, compiler)
  coder = CodeGenerator()
  result = exp.to_code(coder)
  return prelude + result

def compile_to_pyfile(exp):
  file = open(r'f:\dao_all\dao\dao\tests\compiled.py', 'w')
  file.write(compile_to_python(exp))
  file.close()

'''
il.Function(compiled_dao_function, (), 
il.begin(il.Assign(old_failcont, il.failcont), 
  il.SetFailCont(il.Clamda(v1, 
    il.begin(il.SetFailCont(old_failcont), 
      il.Atom(None)))), 
  il.SetFailCont(il.Clamda(v3, il.Clamda(a0, il.Clamda(v2, il.Clamda(v, 
      il.failcont(v))(v2))(il.Prin(a0)))(il.Integer(2)))), 
  il.Clamda(a01, il.Clamda(v2, il.Clamda(v, il.failcont(v))(v2))(il.Prin(a01)))(il.Integer(1))))
'''