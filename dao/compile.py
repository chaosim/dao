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
from dao.compilebase import insert_return_yield, to_code
from dao.interlang import pythonize
from dao import interlang as il

prelude = '''from dao.interlang import LogicVar
from dao.solvebase import Solver, deref

solver = Solver()

'''

def compile_to_python(exp, done=None):  
  if done is None:
    done = il.Done()
  compiler = Compiler()
  env = Environment()
  exp = alpha_convert(exp, env, compiler)
  exp = cps_convert(compiler, exp, done)
  function = compiler.new_var(il.Var('compiled_dao_function'))
  exp = il.Function(function, (), exp)
  #exp = assign_convert(exp, {}, compiler)
  data = OptimizationData()
  optimization_analisys(exp, data)
  exp = optimize(exp, data)
  exp.body = (insert_return_yield(il.begin(*exp.body), il.Yield),)
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
'''
il.Function(compiled_dao_function, (), 
  il.Clamda(x, 
    il.begin(
      il.Assign(x1, il.Deref(x)), 
      il.If(il.IsLogicVar(x1), 
        il.begin(
          il.SetBinding(x1, 1), 
          il.Assign(fc1, il.failcont), 
          il.SetFailCont(
            il.Clamda(v, 
              il.SetFailCont(fc1), il.DelBinding(x1))), 
          True), 
        il.begin(
          il.Assign(y, il.Deref(1)), 
          il.If(il.IsLogicVar(y), 
            il.begin(
              il.SetBinding(y, x1), 
              il.Assign(fc11, il.failcont), 
              il.SetFailCont(
                il.Clamda(v1, 
                    il.SetFailCont(fc11), il.DelBinding(y))), 
              True), 
            il.If((x1==y), 
                  True, 
                  il.failcont(True)))))))
  (1))

il.Function(compiled_dao_function, (), 
  il.begin(il.Assign(x1, il.Deref(x)), 
           il.If(il.IsLogicVar(x1), 
                 il.begin(il.SetBinding(x1, 1), 
                          il.Assign(fc1, il.failcont), 
                          il.SetFailCont(il.Clamda(v, il.SetFailCont(fc1), 
                                                   il.DelBinding(x1))), True), 
                 il.begin(il.Assign(y, il.Deref(1)), 
                          il.If(il.IsLogicVar(y), 
                                il.begin(il.SetBinding(y, x1), 
                                         il.Assign(fc11, il.failcont), 
                                         il.SetFailCont(
                                           il.Clamda(v1,
                                                     il.SetFailCont(fc11), il.DelBinding(y))), True), 
                                il.If((x1==y), True, il.failcont(True)))))))
'''