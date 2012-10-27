from dao.solvebase import dao_repr

def code(exp):
  if isinstance(exp, tuple):
    if len(exp)==1:
      result = '(%s,)'%code(exp[0])
    else:
      return '%s(%s)'%(code(exp[0]), ', '.join(code(x) for x in exp[1:]))
  else:
    try: exp_code = exp.code
    except: return repr(exp)
    return exp_code()
    
