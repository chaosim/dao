##use_item = attr|div(var)|div(str)
##use = 'use'+some(use_item)+optional(use_block)|any(use_item)+use_block\
##          |some(use_item)+div+use_block

def append_var(result, attr): return result+(varcache(attr),)

v = lead(getattr(lambda attr, result: varcache(attr)))

var = lead(some(getattr(append.vars<<arg))+iterator(vars_iter))

my = lead(getattr)
out = lead(getattr)
globl = lead(getattr)

stmts = some(
  getattr[append.stmts<<name2obj(arg)]
  |   getattr[have.caller==name2obj(arg)]
    + call[append.stmts<<daoapply(get.caller,args)]
  )  
block = getitem(have.stmts<<arg)
stmts_block = stmts | block
do_condition = opt(when_fun(have.when==args) + opt(until_fun(have.until==args)))
do = lead(stmts_block+do_condition)

def check_let_bindings(result, bindings): pass
let = lead(call(check_let_bindings)+do_word+stmts_block)

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