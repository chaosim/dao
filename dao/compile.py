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
il.Function(compiled_dao_function, (), il.Clamda(v3, il.begin(
  il.Assign(f, v3), 
  il.Clamda(v, il.Clamda(function, il.Clamda(a0, 
    function(il.Done(v, v), il.Tuple((a0,))))
      (il.Deref(LogicVar(e))))(f))(v3)))
  (il.Function(rules_function, (cont, params), il.begin(
    il.Assign(arity_fun_1, il.Lamda((), il.begin(
      il.Assign(cut_cont, il.cut_cont), 
      il.Assign(il.cut_cont, il.fail_cont), 
      il.Assign(arg, il.Deref(il.GetItem(params, 0))), 
      il.If(il.IsLogicVar(arg), 
        il.begin(
          il.SetBinding(arg, 1), 
          il.Assign(fc1, il.fail_cont), 
          il.Assign(il.fail_cont, il.Clamda(v7, il.begin(
            il.Assign(il.fail_cont, fc1), 
            il.DelBinding(arg), 
            fc1(False)))), 
          il.Clamda(v5, il.Clamda(v4, il.begin(
            il.Assign(il.cut_cont, cut_cont), 
            cont(v4)))(1))(True)), 
        il.If((arg==1), il.Clamda(v5, il.Clamda(v4, il.begin(
          il.Assign(il.cut_cont, cut_cont), 
          cont(v4)))(1))(True), 
              il.fail_cont(True)))))), 
    il.Assign(arity_body_map, RulesDict({1: arity_fun_1})), 
    il.If(il.In(il.Len(params), arity_body_map), 
          il.GetItem(arity_body_map, il.Len(params))(), 
          il.fail_cont(None))))))
'''