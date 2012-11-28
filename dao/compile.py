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

def pythonize(exp, env, compiler):
  exps, has_any_statement = exp.pythonize_exp(env, compiler)
  return il.begin(*exps)

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
  exp = optimize(exp, data)
  #exp = exp.tail_recursive_convert()
  #exp = exp.trampoline()
  exp = pythonize(exp, env, compiler)
  exp = exp.statements[0]
  #if isinstance(exp.body, il.Begin) and isinstance(exp.body.statements[0], il.GlobalDecl):
    #exp.body.statements = exp.body.statements[1:]
  exp.body = exp.body.replace_return_with_yield()
  coder = CodeGenerator()
  result = exp.to_code(coder)
  return prelude + result

def compile_to_pyfile(exp, env):
  code = compile_to_python(exp, env)
  file = open(r'f:\dao_all\dao\dao\tests\compiled.py', 'w')
  file.write(code)
  file.close()

def cps_to_python(exp, compiler, done=None):
  function = compiler.new_var(il.LocalVar('compiled_dao_function'))
  exp = il.Function(function, (), exp)
  #exp = assign_convert(exp, {}, compiler)
  data = OptimizationData()
  exp.optimization_analisys(data)
  exp = optimize(exp, data)
  #exp = exp.tail_recursive_convert()
  #exp = exp.trampoline()
  env = Environment()
  exp = pythonize(exp, env, compiler)
  exp = exp.statements[0]
  exp.body = exp.body.replace_return_with_yield()
  coder = CodeGenerator()
  result = exp.to_code(coder)
  return prelude + result 

def cps_to_pyfile(exp, compiler):
  code = cps_to_python(exp, compiler)
  file = open(r'f:\dao_all\dao\dao\tests\compiled.py', 'w')
  file.write(code)
  file.close()

'''
il.Function(compiled_dao_function, (), 
il.Clamda(v3, 
il.begin(
  il.Assign(f, v3), 
  il.Clamda(v, il.Clamda(function, 
    il.If(il.IsMacro(function), 
          function(il.Done(v, v), 
            il.MacroArgs((ExpressionWithCode(il.Integer(1), il.Lamda((), il.Clamda(v2, v2)(il.Integer(1)))),))), 
          il.Clamda(a0, 
            function(il.Done(v, v), a0))(il.Integer(1))))(il.Deref(f)))(v3)))
(il.Function(rules_function, (cont, params), 
il.begin(
  il.Assign(arity_fun_1, 
    il.Lamda((), il.begin(
      il.Assign(cut_cont, il.Attr(solver, il.Symbol(cut_cont))), 
      il.Assign(il.Attr(solver, il.Symbol(cut_cont)), 
                il.Attr(solver, il.Symbol(fail_cont))), 
      il.Assign(x, il.GetItem(params, il.Integer(0))), 
      il.Clamda(v5, il.Clamda(a01, il.Clamda(a1, il.Clamda(v4, 
        il.begin(
          il.Assign(il.Attr(solver, il.Symbol(cut_cont)), cut_cont), cont(v4)))(a01+a1))
            (il.Deref(x)))(il.Deref(x)))(il.Bool(True))))), 
  il.Assign(arity_body_map, RulesDict), 
  il.If(il.Call(il.Symbol(len), params)inarity_body_map, 
        il.GetItem(arity_body_map, il.Call(il.Symbol(len), params))(),
        il.Attr(solver, il.Symbol(fail_cont))(il.Atom(None)))))))
        '''
'''
il.Function(compiled_dao_function, (), il.begin(
il.Assign(f, 
  il.Function(rules_function, (cont, params), il.begin(
    il.Assign(arity_fun_1, il.Lamda((), il.begin(
      il.Assign(cut_cont, il.Attr(solver, il.Symbol(cut_cont))), 
      il.Assign(il.Attr(solver, il.Symbol(cut_cont)), il.Attr(solver, il.Symbol(fail_cont))), 
      il.Assign(x, il.GetItem(params, il.Integer(0))), 
      il.Assign(a01, il.Deref(x)), il.Assign(a1, il.Deref(x)), 
      il.Assign(il.Attr(solver, il.Symbol(cut_cont)), cut_cont), cont(a01+a1)))), 
    il.Assign(arity_body_map, RulesDict), 
    il.If(il.Call(il.Symbol(len), params) in arity_body_map, 
          il.GetItem(arity_body_map, il.Call(il.Symbol(len), params))(), 
          il.Attr(solver, il.Symbol(fail_cont))(il.Atom(None)))))), 
il.Assign(function, il.Deref(f)), 
il.If(il.IsMacro(function), 
      function(il.Done(v, v), il.MacroArgs((ExpressionWithCode(il.Integer(1), il.Lamda((), il.Integer(1))),))), 
      function(il.Done(v, v), il.Integer(1)))))
'''