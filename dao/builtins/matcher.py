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

@special
def any(compiler, cont, item, template=None, result=None):
  if result is None:
    return any1(item).cps_convert(compiler, cont)  
  else:
    _result  = compiler.new_var(Var('result'))
    return begin(any2(item, template, _result), 
                     unify(result, _result)).cps_convert(compiler, cont)  
  
@special
def any1(compiler, cont, item):
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
def any2(compiler, cont, item, template, result):
  template = template.interlang()
  result = result.interlang()
  any_cont = compiler.new_var(il.ConstLocalVar('any_cont'))
  fc = compiler.new_var(il.ConstLocalVar('old_fail_cont'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  v3 = compiler.new_var(il.ConstLocalVar('v'))
  return il.Begin((
    il.Assign(result, il.empty_list),
    il.cfunction(any_cont, v,
                il.Assign(fc, il.failcont),
                il.SetFailCont(il.clamda(v, 
                  il.SetFailCont(il.clamda(v3, 
                    il.DelListItem(result, il.Integer(-1)),
                    fc(v3))),
                  cont(v))),
                item.cps_convert(compiler, il.clamda(v2, 
                    il.ListAppend(result, il.GetValue(template)),
                    any_cont(v2))))(il.NONE)))

@special
def lazy_any(compiler, cont, item, template=None, result=None):
  if result is None:
    return lazy_any1(item).cps_convert(compiler, cont)  
  else:
    _result  = compiler.new_var(Var('result'))
    return begin(lazy_any2(item, template, _result), 
                     unify(result, _result)).cps_convert(compiler, cont)  

@special
def lazy_any1(compiler, cont, item):
  fc = compiler.new_var(il.ConstLocalVar('fc'))
  lazy_any_cont = compiler.new_var(il.ConstLocalVar('lazy_any_cont'))
  lazy_any_fcont = compiler.new_var(il.ConstLocalVar('lazy_any_fcont'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  return  il.begin(
    il.Assign(fc, il.failcont),
    il.cfunction(lazy_any_fcont, v,
        il.SetFailCont(fc),
        item.cps_convert(compiler, lazy_any_cont)),
    il.cfunction(lazy_any_cont, v,
        il.SetFailCont(lazy_any_fcont),
        cont(il.TRUE))
    (il.TRUE))
                             
@special
def lazy_any2(compiler, cont, item, template, result):
  template = template.interlang()
  result = result.interlang()
  fc = compiler.new_var(il.ConstLocalVar('fc'))
  lazy_any_cont = compiler.new_var(il.ConstLocalVar('lazy_any_cont'))
  lazy_any_fcont = compiler.new_var(il.ConstLocalVar('lazy_any_fcont'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  v1 = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  return  il.begin(
    il.Assign(result, il.empty_list),
    il.Assign(fc, il.failcont),
    il.cfunction(lazy_any_fcont, v,
        il.SetFailCont(fc),
        item.cps_convert(compiler, 
          il.clamda(v2, 
                    il.ListAppend(result, il.GetValue(template)),
                    lazy_any_cont(il.TRUE)))),
    il.cfunction(lazy_any_cont, v,
        il.SetFailCont(lazy_any_fcont),
        cont(il.TRUE))
    (il.TRUE))
                             
@special
def greedy_any(compiler, cont, item, template=None, result=None):
  if result is None:
    return greedy_any1(item).cps_convert(compiler, cont)  
  else:
    _result  = compiler.new_var(Var('result'))
    return begin(greedy_any2(item, template, _result), 
                     unify(result, _result)).cps_convert(compiler, cont)  
    
@special
def greedy_any1(compiler, cont, item):
  v = compiler.new_var(il.ConstLocalVar('v'))
  fc = compiler.new_var(il.ConstLocalVar('old_failcont'))
  greedy_any_fcont = compiler.new_var(il.ConstLocalVar('greedy_any_fcont'))
  greedy_any_cont = compiler.new_var(il.ConstLocalVar('greedy_any_cont'))
  return il.begin(
    il.Assign(fc, il.failcont),
    il.cfunction(greedy_any_fcont, v,
        il.SetFailCont(fc),
        cont(il.TRUE)),    
    il.cfunction(greedy_any_cont, v,
        il.SetFailCont(greedy_any_fcont),
        item.cps_convert(compiler, greedy_any_cont))(il.TRUE))

@special
def greedy_any2(compiler, cont, item, template, result):
  template = template.interlang()
  result = result.interlang()
  item_matched = compiler.new_var(il.Var('item_matched'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  fc = compiler.new_var(il.ConstLocalVar('old_failcont'))
  greedy_any_fcont = compiler.new_var(il.ConstLocalVar('greedy_any_fcont'))
  greedy_any_cont = compiler.new_var(il.ConstLocalVar('greedy_any_cont'))
  return il.begin(
    il.Assign(result, il.empty_list),
    il.Assign(fc, il.failcont),
    il.cfunction(greedy_any_fcont, v,
        il.SetFailCont(fc),
        cont(il.TRUE)),    
    il.cfunction(greedy_any_cont, v,
        il.SetFailCont(greedy_any_fcont),
        item.cps_convert(compiler, 
                         il.clamda(v2, 
                                   il.ListAppend(result, il.GetValue(template)), 
                                   greedy_any_cont(il.TRUE))))(il.TRUE))


@special
def some(compiler, cont, item, template=None, result=None):
  if result is None:
    return some1(item).cps_convert(compiler, cont)  
  else:
    _result  = compiler.new_var(Var('result'))
    return begin(some2(item, template, _result), 
                     unify(result, _result)).cps_convert(compiler, cont)  
  
@special
def some1(compiler, cont, item):
  some_cont = compiler.new_var(il.ConstLocalVar('some_cont'))
  fc = compiler.new_var(il.ConstLocalVar('old_fail_cont'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  some_cont = il.cfunction(some_cont, v,
                il.Assign(fc, il.failcont),
                il.SetFailCont(il.clamda(v, 
                  il.SetFailCont(fc),
                  cont(v))),
                item.cps_convert(compiler, some_cont))
  return item.cps_convert(compiler, some_cont)

@special
def some2(compiler, cont, item, template, result):
  template = template.interlang()
  result = result.interlang()
  some_cont = compiler.new_var(il.ConstLocalVar('some_cont'))
  fc = compiler.new_var(il.ConstLocalVar('old_failcont'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  v3 = compiler.new_var(il.ConstLocalVar('v'))
  append_cont = il.clamda(v2, 
                    il.ListAppend(result, il.GetValue(template)),
                    some_cont(v2))
  return il.Begin((
    il.Assign(result, il.empty_list),
    il.cfunction(some_cont, v,
                 il.Assign(fc, il.failcont),
                il.SetFailCont(il.clamda(v, 
                  il.SetFailCont(il.clamda(v3, 
                    il.DelListItem(result, il.Integer(-1)),
                    fc(v3))),
                 cont(v))),
                item.cps_convert(compiler, append_cont)),
    item.cps_convert(compiler, append_cont)))

@special
def lazy_some(compiler, cont, item, template=None, result=None):
  if result is None:
    return lazy_some1(item).cps_convert(compiler, cont)  
  else:
    _result  = compiler.new_var(Var('result'))
    return begin(lazy_some2(item, template, _result), 
                     unify(result, _result)).cps_convert(compiler, cont)  

@special
def lazy_some1(compiler, cont, item):
  fc = compiler.new_var(il.ConstLocalVar('fc'))
  lazy_some_cont = compiler.new_var(il.ConstLocalVar('lazy_some_cont'))
  lazy_some_fcont = compiler.new_var(il.ConstLocalVar('lazy_some_fcont'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  return  il.begin(
    il.Assign(fc, il.failcont),
    il.cfunction(lazy_some_fcont, v,
        il.SetFailCont(fc),
        lazy_some_cont(il.TRUE)),
    il.cfunction(lazy_some_cont, v,
        item.cps_convert(compiler, il.clamda(v2,
          il.SetFailCont(lazy_some_fcont),
          cont(il.TRUE))))(il.TRUE))
                             
@special
def lazy_some2(compiler, cont, item, template, result):
  template = template.interlang()
  result = result.interlang()
  fc = compiler.new_var(il.ConstLocalVar('fc'))
  lazy_some_cont = compiler.new_var(il.ConstLocalVar('lazy_some_cont'))
  lazy_some_fcont = compiler.new_var(il.ConstLocalVar('lazy_some_fcont'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  v1 = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  return  il.begin(
    il.Assign(result, il.empty_list),
    il.Assign(fc, il.failcont),
    il.cfunction(lazy_some_fcont, v,
        il.SetFailCont(fc),
        lazy_some_cont(il.TRUE)),
    il.cfunction(lazy_some_cont, v,
        item.cps_convert(compiler, il.clamda(v2,
           il.SetFailCont(lazy_some_fcont),
           il.ListAppend(result, il.GetValue(template)),
           cont(il.TRUE))))(il.TRUE))
                             
@special
def greedy_some(compiler, cont, item, template=None, result=None):
  if result is None:
    return greedy_some1(item).cps_convert(compiler, cont)  
  else:
    _result  = compiler.new_var(Var('result'))
    return begin(greedy_some2(item, template, _result), 
                     unify(result, _result)).cps_convert(compiler, cont)  
    
@special
def greedy_some1(compiler, cont, item):
  v = compiler.new_var(il.ConstLocalVar('v'))
  fc = compiler.new_var(il.ConstLocalVar('old_failcont'))
  greedy_some_fcont = compiler.new_var(il.ConstLocalVar('greedy_some_fcont'))
  greedy_some_cont = compiler.new_var(il.ConstLocalVar('greedy_some_cont'))
  return il.begin(
    il.Assign(fc, il.failcont),
    il.cfunction(greedy_some_fcont, v,
        il.SetFailCont(fc),
        cont(il.TRUE)),    
    il.cfunction(greedy_some_cont, v,
        il.SetFailCont(greedy_some_fcont),
        item.cps_convert(compiler, greedy_some_cont)),
  item.cps_convert(compiler, greedy_some_cont))

@special
def greedy_some2(compiler, cont, item, template, result):
  template = template.interlang()
  result = result.interlang()
  item_matched = compiler.new_var(il.Var('item_matched'))
  v = compiler.new_var(il.ConstLocalVar('v'))
  v2 = compiler.new_var(il.ConstLocalVar('v'))
  fc = compiler.new_var(il.ConstLocalVar('old_failcont'))
  greedy_some_fcont = compiler.new_var(il.ConstLocalVar('greedy_some_fcont'))
  greedy_some_cont = compiler.new_var(il.ConstLocalVar('greedy_some_cont'))
  append_result_cont = il.clamda(v2, 
                                   il.ListAppend(result, il.GetValue(template)), 
                                   greedy_some_cont(il.TRUE))
  return il.begin(
    il.Assign(result, il.empty_list),
    il.Assign(fc, il.failcont),
    il.cfunction(greedy_some_fcont, v,
        il.SetFailCont(fc),
        cont(il.TRUE)),    
    il.cfunction(greedy_some_cont, v,
        il.SetFailCont(greedy_some_fcont),
        item.cps_convert(compiler, 
                         append_result_cont)),
    item.cps_convert(compiler, append_result_cont))

