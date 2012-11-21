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
il.Clamda(v11, 
  il.begin(
    il.Assign(i, v11), 
    il.Clamda(v, il.CFunction(block_a, v3, 
      il.begin(
        il.Assign(old_unwind_cont_stack_length, il.unwind_cont_stack_length), 
        il.SetExitBlockContMap(il.String(a), 
          il.Clamda(v4, 
            il.begin(
              il.Unwind(old_unwind_cont_stack_length), 
              il.Clamda(v1, i)(v4)))), 
        il.SetContinueBlockContMap(il.String(a), 
          il.Clamda(v5, 
            il.begin(il.Unwind(old_unwind_cont_stack_length), 
              block_a(v5)))), 
        il.Clamda(a01, il.Clamda(a11, il.Clamda(v10, il.begin(
          il.Assign(i, v10), 
          il.Clamda(v6, il.Clamda(a0, il.Clamda(a1, il.Clamda(v9, 
            il.If(v9, il.GetExitBlockCont(il.String(a))(il.Integer(1))))((a0==a1)))(il.Integer(0)))(i))(il.Atom(None))))(a01-a11))(il.Integer(1)))(i)))(il.Atom(None)))(il.Atom(None))))(il.Integer(3)))
'''