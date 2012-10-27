from dao.solvebase import BaseCommand, mycont
#from dao.term import DummyVar, rule_head_signatures, closure

from dao.compiler import vop
from dao.compiler import type

class Command(BaseCommand):
  ''' the base class for all the callable object in the dao system.'''
  memorable = False
  
  def __call__(self, *args):
    return CommandCall(self, *args)
  
  def run(self, solver, values):
    cont = solver.scont
    signatures = rule_head_signatures(values)
    
    if 1:#solver.parse_state is None or not self.memorable:
      return self.apply(solver, values, signatures)
      
    #sign_state  = ((self, signatures), hash_parse_state(solver.parse_state))
    #sign_state2cont = solver.sign_state2cont.setdefault(sign_state, [])
    
    ## TODO: greedy, nongreedy, lazy mode
    ## lazy : reverse the order of sign_state2cont
    #memo = False
    #i = 0
    #for path, c, env, bindings, vals in sign_state2cont:
      #if cont==c: 
        #memo = True
        #break
      #if len(solver.call_path)<=len(path): # lazy: >
        #i += 1
        #continue
      #for x, y in zip(path, solver.call_path):
        #if x!=y: break
      #else: 
        #if len(path)==len(solver.call_path) and cont.cont_order<c.order:
          #break 
      #i += 1
    #if not memo:
        #sign_state2cont.insert(i, (solver.call_path, cont, solver.env, solver.env.bindings, values))
    #memo_results = solver.sign_state2results.get(sign_state)
    #if memo_results is not None:
      #if memo_results==[]: return
      #old_env = solver.env
      #for _, c, env, old_bindings, vals in sign_state2cont:
        #if c.cont_order>cont.cont_order: continue
        #for result_head, reached_parse_state, value, memo in memo_results:
          #solver.env = env
          #env.bindings = old_bindings.copy()
          ##env.bindings.update(memo)
          #for _ in unify(vals, result_head, solver.env):
            #solver.parse_state = reached_parse_state
            #yield c, value
          #env.bindings = old_bindings
      #solver.env = old_env 
    
    #have_result = [False]
    #@mycont(cont)
    #def memo_result_cont(value, solver):
      #self
      #have_result[0] = True
      #memo = {}
      #result_head = getvalue(values, solver.env, memo)
      #result = result_head, solver.parse_state, value, memo
      #solver.sign_state2results.setdefault(sign_state, []).append(result)
      #old_env = solver.env
      ## TODO: prevent backtracking for greedy
      #for _, c, env, old_bindings, vals in sign_state2cont:
        #solver.env = env
        #env.bindings = old_bindings.copy()
        ##env.bindings.update(memo)
        #for _ in unify_list(vals, result_head, solver.env):
          #yield c, value
        #env.bindings = old_bindings
      #solver.env = old_env
        
    #if len(sign_state2cont)==1:
      #old_fcont = solver.fcont
      #@mycont(old_fcont)
      #def fcont(value, solver):
        #if not have_result[0]:
          #solver.sign_state2results[sign_state] = []
        #solver.fcont = old_fcont
      #solver.fcont = fcont
      #return self.apply(solver, memo_result_cont, values, signatures)

from dao.compiler.cont import *

class Function(Command):
  
  type = type.function
  memorable = True
  
  def evaluate_type(self, compiler, args):
    args_types = [compiler.get_type(arg) for arg in args]
    return self.type.apply(args_types)
  
  def evaluate_cont(self, solver, exps):
    cont = solver.scont
    @mycont(cont)
    def apply_cont(values, solver): 
      solver.scont = cont
      return self.run(solver, values)
    solver.scont = apply_cont
    return self.evaluate_arguments(solver, exps)
  
  def evaluate_arguments(self, solver, exps):
    cont = solver.scont
    if len(exps)==0: 
      solver.scont = cont
      return ()
    else:
      @mycont(cont)
      def argument_cont(value, solver):
        @mycont(cont)
        def gather_cont(values, solver):
            solver.scont = cont
            return (value,)+values
        solver.scont = gather_cont
        return self.evaluate_arguments(solver, exps[1:])
      return self.get_argument(exps[0], argument_cont, solver)
  
  def get_argument(self, arg, cont, solver):
    # don't pass value by DummyVar
    # see sample: some(statement(_stmt), _stmt, stmt_list)
    if isinstance(arg , DummyVar): 
      solver.scont = cont
      return arg 
    else: 
      solver.scont = solver.cont(arg, cont)
      return True
    
  # compile
  
  def compile_cont(self, compiler, args):
    return compile_function_cont(self, compiler, args, self.__class__)

  @classmethod
  def compile_argument(cls, arg, cont, solver):
    # don't pass value by DummyVar
    # see sample: some(statement(_stmt), _stmt, stmt_list)
    if isinstance(arg , DummyVar): 
      solver.scont = ValueCont(arg, cont)
      return solver.scont 
    else: 
      solver.scont = solver.cont(arg, cont)
      return solver.scont
    
def compile_function_cont(fun, compiler, args, klass):
    apply_cont = ApplyCont(fun, compiler.scont)
    compiler.scont = apply_cont
    compiler.add_cont(apply_cont)
    return compile_arguments(klass, compiler, args)

def compile_arguments(klass, compiler, args):
  cont = compiler.scont
  if len(args)==0: 
    compiler.add_cont(ValueCont((), cont))
    return compiler.scont
  else:
    argc = ArgumentCont()
    compiler.add_cont(argc)
    gc = GatherCont(argc, cont)
    compiler.add_cont(gc)
    compiler.scont = gc
    compile_arguments(klass, compiler, args[:-1])
    compiler.scont = argc
    return klass.compile_argument(args[-1], argc, compiler)

class Macro(Command): 
  
  type = type.macro
  memorable = True
  
  def evaluate_type(self, compiler, args):
    args_types = [compiler.get_type(arg) for arg in args]
    return self.type.apply(args_types)
  
  def evaluate_cont(self, solver, exps):
    exps1 = [(closure(exp, solver.env)) for exp in exps]
    return self.run(solver, exps1)
  
  def compile_cont(self, compiler, args):
    return compile_macro_cont(self, compiler, args)

def compile_macro_cont(macro, compiler, args):
    apply_cont = ApplyCont(macro, compiler.scont)
    compiler.scont = apply_cont
    compiler.add_cont(apply_cont)
    compiler.add_cont(ValueCont([vop.GetClosure(arg) for arg in args], apply_cont))
    return compiler.scont
