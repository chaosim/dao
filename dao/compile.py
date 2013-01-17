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
  #solver_prelude = il.begin(
    ##il.Assign(il.parse_state, il.NONE),
    ##il.Assign(il.failcont, 
        ##il.clamda(v, 
              ##il.RaiseException(il.Call(il.Symbol("NoSolution"), v)))),
    ##il.Assign(il.cut_cont,il.failcont),
    ##il.Assign(il.cut_or_cont,il.failcont),
    ##il.Assign(il.catch_cont_map, il.empty_dict),
    #)
  #exp = il.begin(solver_prelude, exp)
  #compiler.lamda_stack = [exp]
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

'''il.begin(il.Assign(start, il.Lamda((cont4, params4), 
il.begin(il.Assign(arity_fun_14, il.Lamda((), 
il.begin(il.Assign(old_failcont7, il.fail_cont)
 il.Assign(il.fail_cont, il.Clamda(v46, 
il.begin(il.Assign(il.fail_cont, old_failcont7)
 il.Assign(x1, il.GetItem(params4, 0))
 d(cont4, il.Tuple((x1,))))))
 il.Assign(x, il.GetItem(params4, 0))
 a(cont4, il.Tuple((x,))))))
 il.Assign(arity_body_map4, RulesDict({1: arity_fun_14}))
 il.If(il.In(il.Len(params4), arity_body_map4), 
il.GetItem(arity_body_map4, il.Len(params4))(), 
il.fail_cont(None)))))
 il.Assign(a, il.Lamda((cont3, params3), 
il.begin(il.Assign(arity_fun_13, il.Lamda((), 
il.begin(il.Assign(cut_cont, il.cut_cont)
 il.Assign(fc4, il.fail_cont)
 il.Assign(il.fail_cont, il.Clamda(v31, 
il.begin(il.Assign(il.fail_cont, fc4)
 il.Assign(il.cut_cont, cut_cont)
 fc4(False))))
 il.Assign(il.cut_cont, il.fail_cont)
 il.Assign(old_failcont5, il.fail_cont)
 il.Assign(il.fail_cont, il.Clamda(v33, 
il.begin(il.Assign(il.fail_cont, old_failcont5)
 il.Assign(x3, il.GetItem(params3, 0))
 d(il.Clamda(v, 
il.begin(il.Assign(il.cut_cont, cut_cont)
 cont3(v))), il.Tuple((x3,))))))
 il.Assign(x2, il.GetItem(params3, 0))
 b(il.Clamda(v38, 
il.begin(il.Assign(il.fail_cont, il.cut_cont)
 c(il.Clamda(v, 
il.begin(il.Assign(il.cut_cont, cut_cont)
 cont3(v))), il.Tuple((x2,))))), il.Tuple((x2,))))))
 il.Assign(arity_body_map3, RulesDict({1: arity_fun_13}))
 il.If(il.In(il.Len(params3), arity_body_map3), 
il.GetItem(arity_body_map3, il.Len(params3))(), 
il.fail_cont(None)))))
 il.Assign(b, il.Lamda((cont2, params2), 
il.begin(il.Assign(arity_fun_12, il.Lamda((), 
il.begin(il.Assign(old_failcont3, il.fail_cont)
 il.Assign(il.fail_cont, il.Clamda(v22, 
il.begin(il.Assign(il.fail_cont, old_failcont3)
 il.Assign(arg2, il.Deref(il.GetItem(params2, 0)))
 il.If(il.IsLogicVar(arg2), 
il.begin(il.SetBinding(arg2, 4)
 il.Assign(fc2, il.fail_cont)
 il.Assign(il.fail_cont, il.Clamda(v25, 
il.begin(il.Assign(il.fail_cont, fc2)
 il.DelBinding(arg2)
 fc2(False))))
 cont2(b4)), 
il.If(il.Eq(arg2, 4), 
cont2(b4), 
il.fail_cont(True))))))
 il.Assign(arg3, il.Deref(il.GetItem(params2, 0)))
 il.If(il.IsLogicVar(arg3), 
il.begin(il.SetBinding(arg3, 1)
 il.Assign(fc3, il.fail_cont)
 il.Assign(il.fail_cont, il.Clamda(v28, 
il.begin(il.Assign(il.fail_cont, fc3)
 il.DelBinding(arg3)
 fc3(False))))
 cont2(b1)), 
il.If(il.Eq(arg3, 1), 
cont2(b1), 
il.fail_cont(True))))))
 il.Assign(arity_body_map2, RulesDict({1: arity_fun_12}))
 il.If(il.In(il.Len(params2), arity_body_map2), 
il.GetItem(arity_body_map2, il.Len(params2))(), 
il.fail_cont(None)))))
 il.Assign(c, il.Lamda((cont1, params1), 
il.begin(il.Assign(arity_fun_11, il.Lamda((), 
il.begin(il.Assign(arg1, il.Deref(il.GetItem(params1, 0)))
 il.If(il.IsLogicVar(arg1), 
il.begin(il.SetBinding(arg1, 4)
 il.Assign(fc1, il.fail_cont)
 il.Assign(il.fail_cont, il.Clamda(v18, 
il.begin(il.Assign(il.fail_cont, fc1)
 il.DelBinding(arg1)
 fc1(False))))
 cont1(c4)), 
il.If(il.Eq(arg1, 4), 
cont1(c4), 
il.fail_cont(True))))))
 il.Assign(arity_body_map1, RulesDict({1: arity_fun_11}))
 il.If(il.In(il.Len(params1), arity_body_map1), 
il.GetItem(arity_body_map1, il.Len(params1))(), 
il.fail_cont(None)))))
 il.Assign(d, il.Lamda((cont, params), 
il.begin(il.Assign(arity_fun_1, il.Lamda((), 
il.begin(il.Assign(arg, il.Deref(il.GetItem(params, 0)))
 il.If(il.IsLogicVar(arg), 
il.begin(il.SetBinding(arg, 3)
 il.Assign(fc, il.fail_cont)
 il.Assign(il.fail_cont, il.Clamda(v13, 
il.begin(il.Assign(il.fail_cont, fc)
 il.DelBinding(arg)
 fc(False))))
 cont(d3)), 
il.If(il.Eq(arg, 3), 
cont(d3), 
il.fail_cont(True))))))
 il.Assign(arity_body_map, RulesDict({1: arity_fun_1}))
 il.If(il.In(il.Len(params), arity_body_map), 
il.GetItem(arity_body_map, il.Len(params))(), 
il.fail_cont(None)))))
 start(il.Clamda(v6, 
il.Deref(LogicVar(x))), il.Tuple((il.Deref(LogicVar(x)),))))
'''