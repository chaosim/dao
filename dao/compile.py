# -*- coding: utf-8 -*-

'''code for compilation:
alpha convert -> cps convert
-> optimize
-> tail recursive convert
-> pythonize -> generate code
'''

from dao.compilebase import Environment, Compiler
from dao.command import element
from dao.compilebase import CompileTypeError, VariableNotBound, import_names
from dao import interlang as il

prelude = '''# -*- coding: utf-8 -*-
# generated file from compiling dao expression.

from dao.builtins import *
from dao.command import LogicVar as DaoLogicVar
from dao.command import Var as DaoVar
from dao.solvebase import Solver, NoSolution
from dao.solvebase import deref, getvalue, LogicVar, DummyVar
from dao.solvebase import UnquoteSplice, ExpressionWithCode
from dao.solvebase import Macro, MacroFunction, MacroRules
from dao.solve import eval as eval_exp
from dao.command import BuiltinFunctionCall
from dao import interlang as il

'''
if import_names:
  prelude += "from dao.compilebase import %s\n"%', '.join(import_names)
prelude += '\nsolver = Solver()\n\n'

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
    done = il.Done(compiler.new_var(il.ConstLocalVar('v')))
  compiler.exit_block_cont_map = {}
  compiler.continue_block_cont_map = {}
  compiler.protect_cont = done
  if env is None: env = Environment()
  exp = element(exp)
  exp = exp.alpha_convert(env, compiler)
  exp = exp.cps_convert(compiler, done)
  exp.analyse(compiler)
  env = Environment()
  exp = exp.optimize(env, compiler)
  #exp = exp.tail_recursive_convert()
  function = compiler.new_var(il.ConstLocalVar('compiled_dao_function'))
  exp = il.Function(function, (), exp)
  exp = il.begin(*exp.pythonize(env, compiler)[0])
  if isinstance(exp, il.Begin):
    exp = exp.statements[0]
  exp.body = exp.body.replace_return_with_yield()
  compiler = Compiler()
  result = exp.to_code(compiler)
  return prelude + result

'''
il.begin(
  il.Assign(f, il.MacroLamda((cont, x, y), 
    il.begin(
      il.Assign(v9, il.EvalExpressionWithCode(y))
      il.Assign(v10, il.EvalExpressionWithCode(x))
      cont(1))))
  il.If(il.IsMacro(f), 
        il.If(il.IsMacroRules(f), 
              f(il.Done(v, v), il.MacroArgs((ExpressionWithCode(println_(concat(1)), il.Lamda((), 
                il.begin(il.Assign(v6, il.Concat(1))
                         il.PrintLn(v6)
                         None))), 
                                             ExpressionWithCode(prin_(concat(2)), il.Lamda((), 
                il.begin(il.Assign(v7, il.Concat(2))
                         il.Prin(v7)
                         None)))))), 
              f(il.Done(v, v), ExpressionWithCode(println_(concat(1)), il.Lamda((), 
                 il.begin(il.Assign(v6, il.Concat(1))
                          il.PrintLn(v6)
                          None))), 
                ExpressionWithCode(prin_(concat(2)), il.Lamda((), 
                  il.begin(il.Assign(v7, il.Concat(2))
                           il.Prin(v7)
                           None))))), 
        il.begin(il.Assign(v4, il.Concat(1))
                 il.PrintLn(v4)
                 il.Assign(v3, il.Concat(2))
                 il.Prin(v3)
                 f(il.Done(v, v), None, None))))
'''