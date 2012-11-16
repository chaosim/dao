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
# generated file after havingcompiled dao expression.

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
il.Function(compiled_dao_function, (), 
            il.begin(il.Assign(old_parse_state, il.parse_state), il.SetParseState(('aaa', 0)), 
                     il.Assign(fc12, il.failcont), 
                     il.SetFailCont(il.Clamda(v2, 
                                              il.SetParseState(old_parse_state), fc12(False))), 
                     il.Clamda(v, 
                               il.CFunction(any_cont, v, il.begin(il.Assign(fc1, il.failcont), 
                          il.SetFailCont(
                            il.Clamda(v,
                                      il.Clamda(v, 
                                                il.If((il.GetItem(il.parse_state, 1)==il.Len(il.GetItem(il.parse_state, 0))), 
                                                      True, 
                                                      il.failcont(False)))(v), fc1(False)))), 
                                            il.begin(il.AssignFromList(text, pos, il.parse_state), 
                                                     il.If2(False, il.failcont(v)), 
                                                     il.If(('a'==il.GetItem(text, pos)), 
                                                           il.begin(il.Assign(fc11, il.failcont), 
                                                                    il.SetFailCont(
                                                                      il.Clamda(v1, 
                                                                                il.SetParseState((text, pos)), 
                                                                                fc11(False))), il.SetParseState((text, il.add((pos, 1)))), 
                                                                    any_cont(il.GetItem(text, pos))), il.failcont(v))))
                               (None))
                     (True))
                                                                    )

'''