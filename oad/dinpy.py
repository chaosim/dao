# -*- coding: utf-8 -*-

'''dao embeded in python list display'''

##__all__ = ['var', 'v', 'do', 'loop', 'put']

from oad.syntax import *

from oad.builtins import format
from oad.term import Var, Apply

class DaoApply(OperatorFunction):
  def __call__(self, *args): 
    self.args = args
    return self
  def run(self, data, *args, **kw):
    args = run(self.args, data, *args, **kw)
    args = (args[0],)+args[1]
    return Apply(*args)
daoapply = lead(DaoApply)

builtins_dict = {
  'write': format.write}

_var_cache = {}
def varcache(name):
  try: return _var_cache[name]
  except: 
    _var_cache[name] = Var(name)
    return _var_cache[name]

getvar = ApplyOperatorFunction(varcache)

def name2obj(name):
  try: return builtins_dict[name]
  except: return varcache(name)

getobj = ApplyOperatorFunction(name2obj)
  
##use_item = attr|div(var)|div(str)
##use = 'use'+some(use_item)+optional(use_block)|any(use_item)+use_block\
##          |some(use_item)+div+use_block

## v.a, my.a, globl.a
SingleVar, LocalVar, GlobalVar  = syntax_klasses( 'SingleVar, LocalVar, GlobalVar')
SingleVar[__getattr__(getvar(arg))]
##LocalVar[__getattr__(getvar(arg))]
##GlobalVar[__getattr__(getvar(arg))]

## var.a.b.c
VarList, VarListAttr = syntax_klasses('VarList, VarListAttr')
VarList[__getattr__(have.var_list==[getvar(arg)])]>>VarListAttr
VarListAttr[__getattr__(append.var_list<<getvar(arg))>>toself,
            __iter__(iterator(get.var_list))]

v = lead(SingleVar)
##my = lead(LocalVar)
##globl = lead(GlobalVar)
var = lead(VarList)

when_fun = fun('when')
until_fun = fun('until')
##
##of_word = dotword('of')
# do.write(1).eof, do[ write(1)], do.write(1).until(1), do.write(1).when(1)
Do, DoAttr, DoBlock, DoForm = syntax_klasses('Do, DoAttr, DoBlock, DoForm')

Do[__getattr__(have.caller==getobj(arg))>>DoAttr, 
   __getitem__(have.body==args)>>DoBlock]
DoAttr[__getattr__(
         iff(test.caller==None).then[have.caller==arg]
             .els[append.body<<getobj(get.caller), have.caller==arg])>>toself, 
       call(append.body<<daoapply(get.caller, args), 
            have.caller==None, 
            get.body)>>toself, 
       when_fun, until_fun]>>DoForm
DoBlock[when_fun(
           iff(see.until_conditions).then[error]
               .els[have.when_conditions==args]), 
        until_fun(have.until_conditions==args)]>>DoForm
DoForm[until_fun(iff(see.until_conditions).then[error])]>>DoForm
do = lead(Do)

###loop[write(1)], loop(10).write(1), loop.open, loop(10)[write(1)]
##(Loop, LoopTimes, LoopTimesBlock, LoopBlock, 
## LoopAttr, LoopCall, LoopTimesAttr, LoopTimesCall) = klasses(
##  'Loop, LoopTimes, LoopTimesBlock, LoopBlock'
##  ', LoopAttr, LoopCall, LoopTimesAttr, LoopTimesCall')
##Loop[call(check_arguments_length/1==1)>>LoopTimes, 
##          __getitem__>>LoopBlock, 
##          getattr>>LoopAttr]
##LoopTimes[__getitem__>>LoopTimesBlock, 
##          getattr(copy.times, assign.forms([]), assign.caller(obj_attr))
##                 >>LoopTimesAttr]
##LoopAttr[
##  call>>LoopCall, 
##  getattr(copy.times, assign.forms([]), assign.caller(obj_attr))
##          >>LoopTimesAttr]
##LoopTimesAttr[
##  call>>LoopTimesCall, 
##  getattr(copy.times, assign.forms([]), assign.caller(obj_attr))
##          >>LoopTimesAttr]
##LoopTimesBlock
##LoopBlock
##  
##def get_assign_var(var): return var
##def check_argument_is_int(var): return var
##def argument(var): return var
##    
##Assign, SingleAssign, MultipleAssign = klasses(
##  'Assign, SingleAssign, MultipleAssign')
##my = dotword('my')
##out = dotword('out')
##Assign[init(assign.scope('any')),
##             getattr>>SingleAssign, 
##             my(assign.scope('local')), 
##             out(assign.scope('out')), 
##             bitxor(check_argument_is_int, assign.scope(('out', argument))), 
##             __getitem__>>MultipleAssign]
##SingleAssign[eq]
##MultipleAssign[eq]
##
##put = Assign()

####do_word = dotword('do')
##  
##Let, LetBindings, LetDo, LetAttr = klasses(
##  'Let, LetBindings, LetDo, LetAttr')
##class Check_bindings(Function): pass
##check_bindings = Check_bindings()
### letform -> let({}).do[...]
### letform -> let({})(.attr| .attr(...))+
##Let, SingleAssign, MultipleAssign = klasses(
##  'Let, SingleAssign, MultipleAssign')
##Let[call(check_bindings)>>LetBindings]
##LetBindings[do_word>>LetDo,
##            getattr>>LetAttr]
##LetDo[__getitem__]
##LetAttr[__getattr__, call]>>LetAttr
##let = Let()
##  
##    