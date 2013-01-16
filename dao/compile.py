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
from dao.solvebase import UnquoteSplice, ExpressionWithCode, MacroFunction
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
  v = compiler.new_var(il.ConstLocalVar('v'))
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
  compiler.lamda_stack = [exp]
  exp.analyse(compiler)
  env = Environment()
  exp = exp.optimize(env, compiler)
  #for x in env.bindings.values():
    #try:
      #if x._removed==il.unknown:
        #x.remove()
    #except: pass
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
 il.Assign(il.parse_state, None)
 il.Assign(il.fail_cont, il.Clamda(v8, 
    il.RaiseException(il.Call(NoSolution, v8))))
 il.Assign(il.cut_cont, il.fail_cont)
 il.Assign(il.cut_or_cont, il.fail_cont)
 il.Assign(il.catch_cont_map, {})
 il.Assign(old_parse_state, il.parse_state)
 il.Assign(il.parse_state, il.Tuple((aaa, 0)))
 il.Assign(fc3, il.fail_cont)
 il.Assign(il.fail_cont, il.Clamda(v7, 
   il.begin(il.Assign(il.fail_cont, fc3)
    il.Assign(il.parse_state, old_parse_state)
    fc3(False))))
 il.Assign(fc, il.fail_cont)
 il.CFunction(lazy_some_fcont, v4, 
   il.begin(il.Assign(il.fail_cont, fc)
     il.AssignFromList((text, pos), il.parse_state)
     il.If(il.Ge(pos, il.Len(text)), 
       il.fail_cont(None), 
       il.If(il.Eq(a, il.GetItem(text, pos)), 
        il.begin(il.Assign(fc1, il.fail_cont)
          il.Assign(il.fail_cont, il.Clamda(v5, 
            il.begin(il.Assign(il.fail_cont, fc1)
            il.Assign(il.parse_state, il.Tuple((text, pos)))
            fc1(False))))
            il.Assign(il.parse_state, il.Tuple((text, il.add(pos, 1))))
            lazy_some_cont(il.GetItem(text, pos))), 
       il.fail_cont(None)))))
 il.CFunction(lazy_some_cont, v4, 
    il.begin(il.Assign(il.fail_cont, lazy_some_fcont)
      il.If(il.Ge(il.GetItem(il.parse_state, 1), il.Len(il.GetItem(il.parse_state, 0))), 
      True, 
      il.fail_cont(False))))
  il.AssignFromList((text1, pos1), il.parse_state)
  il.If(il.Ge(pos1, il.Len(text1)), 
      il.fail_cont(None), 
      il.If(il.Eq(a, il.GetItem(text1, pos1)), 
        il.begin(il.Assign(fc2, il.fail_cont)
          il.Assign(il.fail_cont, il.Clamda(v6, 
            il.begin(il.Assign(il.fail_cont, fc2)
            il.Assign(il.parse_state, il.Tuple((text1, pos1)))
            fc2(False))))
          il.Assign(il.parse_state, il.Tuple((text1, il.add(pos1, 1))))
          lazy_some_cont(il.GetItem(text1, pos1))), 
        il.fail_cont(None))))
'''