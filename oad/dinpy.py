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

getvar = ApplyFunction(varcache)

def name2obj(name):
  try: return builtins_dict[name]
  except: return varcache(name)

getobj = ApplyFunction(name2obj)
  
##use_item = attr|div(var)|div(str)
##use = 'use'+some(use_item)+optional(use_block)|any(use_item)+use_block\
##          |some(use_item)+div+use_block

## v.a, my.a, globl.a
SingleVar, LocalVar, GlobalVar  = syntax_klasses( 'SingleVar, LocalVar, GlobalVar')
SingleVar[getattr(getvar(arg))]
##LocalVar[getattr(getvar(arg))]
##GlobalVar[getattr(getvar(arg))]

## var.a.b.c
VarList, VarListAttr = syntax_klasses('VarList, VarListAttr')
VarList[getattr(have.var_list==[getvar(arg)])]>>VarListAttr
VarListAttr[getattr(append.var_list<<getvar(arg))>>toself,
            iterable(iterator(get.var_list))]

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

Do[getattr(have.caller==getobj(arg))>>DoAttr, 
   getitem(have.body==args)>>DoBlock]
DoAttr[getattr(
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
##          getitem>>LoopBlock, 
##          getattr>>LoopAttr]
##LoopTimes[getitem>>LoopTimesBlock, 
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
##             getitem>>MultipleAssign]
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
##LetDo[getitem]
##LetAttr[getattr, call]>>LetAttr
##let = Let()
##  
##    
##use_item = attr|div(var)|div(str)
##use = 'use'+some(use_item)+optional(use_block)|any(use_item)+use_block\
##          |some(use_item)+div+use_block

def append_var(result, attr): return result+(varcache(attr),)
v = lead(getattr['v_Var'](lambda attr, result: varcache(attr)))

def vars_iter(result):
  for v in result: yield v
var = lead(init(lambda x:())
           |some(getattr(append_var))['var_Vars']
           +iter(vars_iter))

my = lead(getattr['LocalVar'])
out = lead(getattr['OuterVar'])
globl = lead(getattr['GlobalVar'])

def check_form(result, forms): pass
block = getitem(check_form)
calls = any(getattr)|any(getattr+call)+getattr+(getattr('end')|call)
calls = some(getattr)+getattr('end')
calls = getattr+any(getattr+getattr)+getattr+(call|dotword('end'))
calls = getattr+(call|dotword('end'))
stmts = block|calls

def check_let_bindings(result, bindings): pass
let = lead(call(check_let_bindings)+dotword('do')+stmts)

##put = lead(getattr|getitem + eq(check_form))
##
##body = some(getattr|getattr+call) | block 
##do_condition = when_fun | until_fun | when_fun + until_fun
##
##do = lead(body+do_condition)
##
##on = lead(call+getattr('do')+stmts)
##case = lead(call+some(getattr('of')+call+some(stmts))|div(dict))
##iff = lead(call+stmts+any(attr('elsif')+stmts)+optional(getattr('els')+stmts))
##loop = lead(call(int)+stmts)
##
##fun = lead(getattr+(eq+rule_dict|(getattr('at')+rule_list)))
##
##decl = use|var|v|my|out|globl
##stmt = do|put|let|fun|macro|on|case|iff|loop|rule|rules|put
##form = decl|stmt

fun = Fun
when_fun = fun('when')
until_fun = fun('until')

class DotWord(Unary):
  def __init__(self, word):
    self.word = word
    self.functions = ()

dotword = DotWord

of_word = dotword('of')
