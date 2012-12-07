# -*- coding: utf-8 -*-

'''code for compilation:
alpha convert -> cps convert
-> optimization 
-> tail recursive convert
-> pythonize -> generate code
'''

from dao.compilebase import Environment, Compiler
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
    il.Assign(il.parse_state, il.NONE),
    il.Assign(il.failcont, 
        il.clamda(v, 
              il.RaiseException(il.Call(il.Symbol("NoSolution"), v)))),
    il.Assign(il.cut_cont,il.failcont),
    il.Assign(il.cut_or_cont,il.failcont),
    il.Assign(il.catch_cont_map, il.empty_dict),
    )
  exp = il.begin(solver_prelude, exp)
  exp.analyse(compiler)
  exp = exp.optimize(compiler)
  #exp = exp.tail_recursive_convert()
  function = compiler.new_var(il.LocalVar('compiled_dao_function'))
  exp = il.Function(function, (), exp)
  exp = il.begin(*exp.pythonize(env, compiler)[0])
  exp = exp.statements[0]
  exp.body = exp.body.replace_return_with_yield()
  compiler = Compiler()
  result = exp.to_code(compiler)
  return prelude + result

'''
il.begin(
il.Assign(old_parse_state, il.parse_state), 
il.Assign(il.parse_state, il.Tuple((aaa, 0))), 
il.Assign(fc11, il.fail_cont), 
il.Assign(il.fail_cont, il.Clamda(v6, il.begin(
  il.Assign(il.fail_cont, fc11), 
  il.Assign(il.parse_state, old_parse_state), 
  fc11(False)))), 
il.Clamda(v1,
  il.CFunction(any_cont, v4, il.begin(
    il.Assign(old_fail_cont, il.fail_cont), 
    il.Assign(il.fail_cont, il.Clamda(v4, il.begin(
      il.Assign(il.fail_cont, old_fail_cont), 
      il.Clamda(v2, 
        il.If((il.GetItem(il.parse_state, 1)>=il.Len(il.GetItem(il.parse_state, 0))), 
              True, 
              il.fail_cont(False)))(v4)))), 
    il.AssignFromList((text, pos), il.parse_state), 
    il.If((pos>=il.Len(text)), 
          il.Return(il.fail_cont(None))), 
    il.If((a==il.GetItem(text, pos)), 
      il.begin(
        il.Assign(fc1, il.fail_cont), 
        il.Assign(il.fail_cont, il.Clamda(v5, il.begin(
          il.Assign(il.fail_cont, fc1), 
          il.Assign(il.parse_state, il.Tuple((text, pos))), 
          fc1(False)))), 
        il.Assign(il.parse_state, il.Tuple((text, il.add(pos, 1)))), 
        il.Return(any_cont(il.GetItem(text, pos)))), 
      il.Return(il.fail_cont(None)))))(True))
(True))
'''