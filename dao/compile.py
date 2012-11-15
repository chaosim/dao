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
  il.begin(
  il.Assign(old_parse_state, il.parse_state), 
  il.SetParseState(('aaa', 0)), 
  il.Assign(fc12, il.failcont), 
  il.SetFailCont(
    il.Clamda(v2, 
        il.SetParseState(old_parse_state), 
        fc12(False))), 
  il.Assign(fc1, il.failcont), 
  il.SetFailCont(
    il.Lamda((v), 
      il.Lamda((v), 
        il.If((il.GetItem(il.parse_state, 1)==il.Len(il.GetItem(il.parse_state, 0))), 
              True, 
              il.failcont(False)))(True), 
      fc1(False))), 
  il.AssignFromList(text, pos, il.parse_state), 
  il.If2((pos>=il.Len(text)), 
         il.failcont(True)), 
  il.If(('a'==il.GetItem(text, pos)), 
        il.begin(
          il.Assign(fc11, il.failcont), 
          il.SetFailCont(
            il.Lamda((v1), 
              il.SetParseState((text, pos)), 
              fc11(False))), 
          il.SetParseState((text, il.add((pos, 1)))), 
          any_cont(il.GetItem(text, pos))), 
        il.failcont(True))))
        '''
