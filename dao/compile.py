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
from dao.command import LogicVar as DaoLogicVar
from dao.solvebase import Solver, deref, LogicVar
from dao.solvebase import UnquoteSplice
from dao.command import BuiltinFunctionCall
from dao import interlang as il

solver = Solver()

'''

def pythonize(exp, env, compiler):
  exps, has_any_statement = exp.pythonize_exp(env, compiler)
  return il.begin(*exps)

def compile_to_python(exp, done=None): 
  '''assemble steps from dao expression to python code'''
  if done is None:
    done = il.Done()
  compiler = Compiler()
  env = Environment()
  exp = exp.alpha_convert(env, compiler)
  exp = exp.cps_convert(compiler, done)
  function = compiler.new_var(il.LocalVar('compiled_dao_function'))
  exp = il.Function(function, (), exp)
  #exp = assign_convert(exp, {}, compiler)
  data = OptimizationData()
  exp.optimization_analisys(data)
  exp = optimize(exp, data)
  exp = exp.tail_recursive_convert()
  exp = exp.trampoline()
  exp = pythonize(exp, env, compiler)
  exp = exp.statements[0]
  exp.body = exp.body.replace_return_with_yield()
  coder = CodeGenerator()
  result = exp.to_code(coder)
  return prelude + result

def compile_to_pyfile(exp):
  file = open(r'f:\dao_all\dao\dao\tests\compiled.py', 'w')
  file.write(compile_to_python(exp))
  file.close()

'''
il.Function(compiled_dao_function, (), il.begin(
  il.Assign(old_parse_state, il.parse_state), 
  il.SetParseState(il.Tuple((il.String(ab), il.Integer(0)))), 
  il.Assign(fc1, il.failcont), 
  il.SetFailCont(il.Clamda(v1, il.begin(
    il.SetFailCont(fc1), 
    il.SetParseState(old_parse_state), 
    fc1(il.Bool(False))))), 
  il.AssignFromList(text1, pos1, il.parse_state), 
  il.If((pos1>=il.Len(text1)), 
    il.Return(il.failcont(il.Atom(None)))), 
  il.If((il.String(a)==il.GetItem(text1, pos1)), 
    il.begin(
      il.Assign(fc12, il.failcont), 
      il.SetFailCont(il.Clamda(v6, il.begin(
        il.SetFailCont(fc12), 
        il.SetParseState(il.Tuple((text1, pos1))), 
        fc12(il.Bool(False))))), 
      il.SetParseState(il.Tuple((text1, pos1+il.Integer(1)))), 
      il.Return(
        il.begin(il.GetItem(text1, pos1), 
          il.AssignFromList(text, pos, il.parse_state), 
          il.If((pos>=il.Len(text)), 
            il.Return(il.failcont(il.Atom(None)))), 
          il.If((il.String(b)==il.GetItem(text, pos)), 
            il.begin(
              il.Assign(fc11, il.failcont), 
              il.SetFailCont(il.Clamda(v5, il.begin(
                il.SetFailCont(fc11), 
                il.SetParseState(il.Tuple((text, pos))), 
                fc11(il.Bool(False))))), 
              il.SetParseState(il.Tuple((text, pos+il.Integer(1)))), 
              il.Return(il.begin(
                il.Assign(v, il.GetItem(text, pos)), 
                il.SetParseState(old_parse_state), v))), 
            il.Return(il.failcont(il.Atom(None))))))), 
    il.Return(il.failcont(il.Atom(None))))))
'''