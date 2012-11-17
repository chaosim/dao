# -*- coding: utf-8 -*-

'''code for compilation:
alpha convert -> cps convert -> assign-convert 
-> optimization 
-> tail recursive convert -> trampoline 
-> pythonize -> generate code
'''

from dao.compilebase import Environment, Compiler, CodeGenerator, OptimizationData
from dao.compilebase import CompileTypeError, VariableNotBound
from dao.compilebase import optimize
from dao.interlang import pythonize
from dao import interlang as il

prelude = '''# -*- coding: utf-8 -*-
# generated file after compiling dao expression.

from dao.interlang import LogicVar
from dao.solvebase import Solver, deref

solver = Solver()

'''

def compile_to_python(exp, done=None):  
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
il.Clamda(v2, il.begin(
  il.Assign(f, v2), 
  il.Clamda(v, il.Clamda(function, il.Clamda(a0, 
    function(il.Done(v, v), a0))(il.Integer(2)))(f))
  (il.Atom(None))))
(il.Lamda((x, cont), 
  il.Clamda(a03, il.Clamda(a11, il.Clamda(v3, 
    il.If(v3, cont(il.Integer(1)), 
          il.Clamda(function1, il.Clamda(a02, il.Clamda(a1, il.Clamda(a01, 
              function1(cont, a01))(a02-a1))(il.Integer(1)))(x))
          (f)))
                           ((a03==a11)))
            (il.Integer(1)))
  (x)))


il.Function(compiled_dao_function, (), il.begin(
  il.Assign(f, 
    il.Lamda((x, cont), il.Clamda(a03, il.Clamda(a11, il.Clamda(v3, il.If(v3, cont(il.Integer(1)), il.Clamda(function1, il.Clamda(a02, il.Clamda(a1, il.Clamda(a01, function1(cont, a01))(a02-a1))(il.Integer(1)))(x))(f)))((a03==a11)))(il.Integer(1)))(x))
    ), 
  f(il.Done(v, v), il.Integer(2))))
'''