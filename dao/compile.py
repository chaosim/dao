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
  function = compiler.new_var(il.LocalVar('compiled_dao_function'))
  exp = il.Function(function, (), exp)
  #exp = assign_convert(exp, {}, compiler)
  data = OptimizationData()
  exp.optimization_analisys(data)
  exp = exp.optimize(data)
  #exp = exp.tail_recursive_convert()
  #exp = exp.trampoline()
  exp = il.begin(*exp.pythonize_exp(env, compiler)[0])
  exp = exp.statements[0]
  #if isinstance(exp.body, il.Begin) and isinstance(exp.body.statements[0], il.GlobalDecl):
    #exp.body.statements = exp.body.statements[1:]
  exp.body = exp.body.replace_return_with_yield()
  coder = CodeGenerator()
  result = exp.to_code(coder)
  return prelude + result

'''
il.Function(compiled_dao_function, (), il.begin(
  il.Assign(old_parse_state, il.parse_state), 
  il.Assign(il.parse_state, il.Tuple((aaa, 0))), 
  il.Assign(fc11, il.fail_cont), 
  il.Assign(il.fail_cont, il.Clamda(v4, il.begin(
    il.Assign(il.fail_cont, fc11), 
    il.Assign(il.parse_state, old_parse_state), 
    fc11(False)))), 
  il.Clamda(v, il.CFunction(any_cont, v2, il.begin(
    il.Assign(old_fail_cont, il.fail_cont), 
    il.Assign(il.fail_cont, il.Clamda(v2, il.begin(
      il.Assign(il.fail_cont, old_fail_cont), 
      v2))), 
    il.AssignFromList(text, pos, il.parse_state), 
    il.If((pos>=il.Len(text)), 
          il.Return(il.fail_cont(None))), 
    il.If((a==il.GetItem(text, pos)), 
      il.begin(
        il.Assign(fc1, il.fail_cont), 
        il.Assign(il.fail_cont, il.Clamda(v3, il.begin(
          il.Assign(il.fail_cont, fc1), 
          il.Assign(il.parse_state, il.Tuple((text, pos))), 
          fc1(False)))), 
        il.Assign(il.parse_state, il.Tuple((text, il.add(pos, 1)))), 
        il.Return(any_cont(il.GetItem(text, pos)))), 
      il.Return(il.fail_cont(None)))))(True))(True)))

il.Function(compiled_dao_function, (), il.begin(
  il.Assign(old_parse_state, il.parse_state), 
  il.Assign(fc11, il.fail_cont), 
  il.Assign(il.parse_state, il.Tuple((aaa, 0))), 
  il.Assign(il.fail_cont, il.Clamda(v4, il.begin(
    il.Assign(il.fail_cont, fc11), 
    il.Assign(il.parse_state, old_parse_state), 
    fc11(False)))), 
  il.CFunction(any_cont, v2, il.begin(
    il.Assign(old_fail_cont, il.fail_cont), 
    il.AssignFromList(text, pos, il.parse_state), 
    il.If((pos>=il.Len(text)), 
      il.Return(il.begin(
        il.Assign(il.fail_cont, old_fail_cont), 
        True))), 
    il.If((a==il.GetItem(text, pos)), 
          il.Return(any_cont(il.GetItem(text, pos))), 
          il.Return(il.begin(
            il.Assign(il.fail_cont, old_fail_cont), 
              True)))))(None)))
'''
