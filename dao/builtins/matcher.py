# -*- coding: utf-8 -*-

# TODO: longest: the longest matcher.

# parser predicate
# optional, any, some, times, seplist, ...

from dao.base import classeq
from dao.compilebase import CompileTypeError, VariableNotBound
from dao.command import special
import dao.interlang as il
from dao.interlang import TRUE, FALSE, NONE


v0, fc0 = il.LocalVar('v'), il.LocalVar('fc')

# lazy: match at least, except matchers that followed fail. 尽量少吃进，除非别人逼我多吃一点
# nongreedy, match at most, throw out if matchers followed fail.先尽量多吃，如果别人要再吐出来
# greedy, match at most, don,t throw out even matchers followed fail. 吃进去了就不会吐出来。

greedy, nongreedy, lazy = 0, 1, 2

def may(item, mode=greedy):
  if mode==greedy: return _greedy_may(item)
  elif mode==nongreedy: return _may(item)
  else: return _lazy_may(item)

@special
def _may(compiler, cont, item):
  v = compiler.new_var(v0)
  return cps_convert(compiler, clause, cont, il.Clamda(v,  cont(v)))

@special
def _lazy_may(compiler, cont, item):
  v = compiler.new_var(v0)
  return il.Clamda(v, cont(v, cps_convert(compiler, item, cont)))

@special
def _greedy_may(compiler, cont, item):
  v = compiler.new_var(v0)
  return cps_convert(compiler, item, il.Clamda(v, cont(v)), 
                                      il.Clamda(v, cont(v)))

def any(item, mode=nongreedy):
  if mode==greedy: return _greedy_any(item)
  elif mode==nongreedy: return _any(item)
  else: return _lazy_any(item)

#any
  #item: item, any(item)
  #item: succeed
  
#greedy_any:
  #item: item, cut, greedy_any(item)
  #item: succeed

#lazy_any:
  #item: succeed
  #item: item, lazy_any(item)
  
@special
def _any(compiler, cont, item):
  any_cont = compiler.new_var(il.LocalVar('any_cont'))
  fc = compiler.new_var(il.LocalVar('old_fail_cont'))
  v = compiler.new_var(il.LocalVar('v'))
  return il.cfunction(any_cont, v,
                il.Assign(fc, il.failcont),
                il.SetFailCont(il.clamda(v, 
                  il.SetFailCont(fc),
                  cont(v))),
                item.cps_convert(compiler, any_cont))(TRUE)

@special
def _any2(compiler, cont, item, result, template):
  todo_for_expriment_macro_expand
  result2 = compiler.new_var(Var('result2'))
  macro_expanded = begin(assign(result2, empty_list), 
               _any(begin(item, 
                          logic_list_append(result2, getvalue(template)), 
                          unify(result, result2))))
  return expanded.cps_convert(compiler, cont)

@special
def _lazy_any(compiler, cont, item):
  fcont = compiler.new_var(il.LocalVar('fcont'))
  lazy_any_cont = compiler.new_var(il.LocalVar('lazy_any_cont'))
  lazy_any_fcont = compiler.new_var(il.LocalVar('lazy_any_fcont'))
  v = compiler.new_var(il.LocalVar('v'))
  return  il.begin(
    il.Assign(fcont, il.failcont),
    il.cfunction(lazy_any_cont, v,
        il.SetFailCont(lazy_any_fcont),
        cont(v)),
    il.cfunction(lazy_any_fcont, v,
        il.SetFailCont(fcont),
        cps_convert(compiler, item, lazy_any_cont)),    
    lazy_any_cont(TRUE))
                             
@special
def _any2(compiler, cont, item, result, template):
  result2 = compiler.new_var(Var('result2'))
  macro_expanded = begin(assign(result2, empty_list), 
               _lazy_any(begin(item, 
                          logic_list_append(result2, getvalue(template)), 
                          unify(result, result2))))
  return expanded.cps_convert(compiler, cont)

@special
def _greedy_any(compiler, cont, item):
  fcont = compiler.new_var(il.LocalVar('fcont'))
  greedy_any_fcont = compiler.new_var(il.LocalVar('greedy_any_fcont'))
  greedy_any_cont = compiler.new_var(il.LocalVar('greedy_any_cont'))
  return il.begin(
    il.Assign(fcont, il.failcont),
    il.cfunction(greedy_any_fcont, v,
        il.SetFailCont(fcont),
        cont(v)),    
    il.cfunction(greedy_any_cont, v,
        il.SetFailCont(greedy_any_fcont),
         cps_convert(compiler, item, greedy_any_cont)),
    greedy_any_cont(TRUE))

@special
def _greedy_any2(compiler, cont, item, result, template):
  result2 = compiler.new_var(Var('result2'))
  macro_expanded = begin(assign(result2, empty_list), 
               _greedy_any(begin(item, 
                          logic_list_append(result2, getvalue(template)), 
                          unify(result, result2))))
  return expanded.cps_convert(compiler, cont)

