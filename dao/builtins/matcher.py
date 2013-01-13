# -*- coding: utf-8 -*-

# TODO: longest: the longest matcher.

# parser predicate
# optional, any, some, times, seplist, ...

from dao.base import classeq
from dao.compilebase import CompileTypeError, VariableNotBound
from dao.command import special, element, Var
from special import begin
from term import unify
import dao.interlang as il

# lazy: match at least, except matchers that followed fail. 尽量少吃进，除非别人逼我多吃一点
# nongreedy, match at most, throw out if matchers followed fail.先尽量多吃，如果别人要再吐出来
# greedy, match at most, don,t throw out even matchers followed fail. 吃进去了就不会吐出来。

@special
def may(compiler, cont, item):
  v = compiler.new_var(il.ConstLocalVar('v'))
  return cps_convert(compiler, clause, cont, il.Clamda(v,  cont(v)))

@special
def lazy_may(compiler, cont, item):
  v = compiler.new_var(il.ConstLocalVar('v'))
  return il.Clamda(v, cont(v, cps_convert(compiler, item, cont)))

@special
def greedy_may(compiler, cont, item):
  v = compiler.new_var(il.ConstLocalVar('v'))
  return cps_convert(compiler, item, il.Clamda(v, cont(v)), 
                                      il.Clamda(v, cont(v)))

def greedy_any(item, template=None, result=None):
    if result is None:
      return _greedy_any(item)
    else:
      return _greedy_any2(item, template, result)
    
def lazy_any(item, template=None, result=None):
  if result is None:
    return _lazy_any(item)
  else:
    return _lazy_any2(item, template, result)    

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
def any(compiler, cont, item, template=None, result=None):
  if result is None:
    return _any1(item).cps_convert(compiler, cont)  
  else:
    _result  = compiler.new_var(Var('result'))
    return begin(_any2(item, template, _result), 
                     unify(result, _result)).cps_convert(compiler, cont)  
  
@special
def _any1(compiler, cont, item):
  any_cont = compiler.new_var(il.ConstLocalVar('any_cont'))
  fc = compiler.new_var(il.ConstLocalVar('old_fail_cont'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  return il.cfunction(any_cont, v,
                il.Assign(fc, il.failcont),
                il.SetFailCont(il.clamda(v, 
                  il.SetFailCont(fc),
                  cont(v))),
                item.cps_convert(compiler, any_cont))(il.TRUE)

@special
def _any2(compiler, cont, item, template, result):
  template = template.interlang()
  result = result.interlang()
  any_cont = compiler.new_var(il.ConstLocalVar('any_cont'))
  fc = compiler.new_var(il.ConstLocalVar('old_fail_cont'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  v1 = compiler.new_var(il.ConstLocalVar('v'))
  cont = il.clamda(v1,  
                   il.if2(il.Ne(result, il.empty_list),
                          il.DelItem(result, il.Integer(0))),
                   cont(v1))
  return il.Begin((
    il.Assign(result, il.empty_list),
    il.cfunction(any_cont, v,
                il.Assign(fc, il.failcont),
                il.SetFailCont(il.clamda(v, 
                  il.SetFailCont(fc),
                  cont(v))),
                il.ListAppend(result, il.GetValue(template)),
                item.cps_convert(compiler, any_cont))(il.NONE)))

@special
def _lazy_any(compiler, cont, item):
  fcont = compiler.new_var(il.ConstLocalVar('fcont'))
  lazy_any_cont = compiler.new_var(il.ConstLocalVar('lazy_any_cont'))
  lazy_any_fcont = compiler.new_var(il.ConstLocalVar('lazy_any_fcont'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  return  il.begin(
    il.Assign(fcont, il.failcont),
    il.cfunction(lazy_any_cont, v,
        il.SetFailCont(lazy_any_fcont),
        cont(v)),
    il.cfunction(lazy_any_fcont, v,
        il.SetFailCont(fcont),
        cps_convert(compiler, item, lazy_any_cont)),    
    lazy_any_cont(TRUE))
                             
#@special
#def _any2(compiler, cont, item, result, template):
  #result2 = compiler.new_var(Var('result2'))
  #macro_expanded = begin(assign(result2, empty_list), 
               #_lazy_any(begin(item, 
                          #logic_list_append(result2, getvalue(template)), 
                          #unify(result, result2))))
  #return expanded.cps_convert(compiler, cont)

@special
def _greedy_any(compiler, cont, item):
  fcont = compiler.new_var(il.ConstLocalVar('fcont'))
  greedy_any_fcont = compiler.new_var(il.ConstLocalVar('greedy_any_fcont'))
  greedy_any_cont = compiler.new_var(il.ConstLocalVar('greedy_any_cont'))
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

