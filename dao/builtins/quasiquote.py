from dao import builtin
from dao.builtin import Builtin, Function
from dao.term import CommandCall
from dao.solve import DaoSyntaxError, mycont

# quasiquote and backquote

def evaluate_quasiquote_list_cont(solver, cont, exps):
  @mycont(cont)
  def quasi_cont(result, solver):
    if len(exps)==0: 
      solver.scont = cont
      return result
    else:
      element0 = exps[0]
      left_cont = evaluate_quasiquote_list_cont(solver, cont, exps[1:])
      if element0==():
        solver.scont = left_cont
        return result+((),)
      if not isinstance(element0, tuple):
        if element0==unquote or element0==unquote_splice:
          raise DaoSyntaxError
        else: 
          solver.scont = left_cont
          return result+(element0,)
      elif len(element0)==2:
        if element0[0]==unquote:
          @mycont(quasi_cont)
          def gather_cont(value, solver):
            solver.scont = left_cont
            return result+(value,)
          solver.scont = solver.cont(element0[1], gather_cont)
          return True
        elif element0[0]==unquote_splice:
          @mycont(quasi_cont)
          def gather_cont(value, solver):
            solver.scont = left_cont
            return result+value
          solver.scont = solver.cont(element0[1], gather_cont)
          return True
      elif element0[0]==unquote or element0[0]==unquote_splice:
        raise DaoSyntaxError
      @mycont(quasi_cont)
      def gather_cont(value, solver):
        solver.scont = left_cont
        return result+(value,)
      solver.scont = evaluate_quasiquote_list_cont(solver, gather_cont, element0)
      return ()
  return quasi_cont

@builtin.macro('quasiquote')
def quasiquote(solver, item):
  if not isinstance(item, tuple) or item==():
    return item
  elif len(item)==2:
    if item[0]==unquote:
      solver.scont = solver.cont(item[1], solver.scont)
      return True
    elif item[0]==unquote_splice:
      raise DaoSyntaxError
  elif item[0]==unquote or item[0]==unquote_splice:
    raise DaoSyntaxError
  solver.scont = evaluate_quasiquote_list_cont(solver, solver.scont, item)
  return ()

@builtin.macro('unquote')
def unquote(solver, *args):
  raise DaoSyntaxError

@builtin.macro('unquote_splice')
def unquote_splice(solver, *args):
  raise DaoSyntaxError
