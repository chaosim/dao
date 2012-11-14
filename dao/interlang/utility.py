from dao.compile import pythonize

def is_statement(exp):
  try: return exp.is_statement
  except:
    if isinstance(exp, list) or isinstance(exp, tuple) or\
       isinstance(exp, int) or isinstance(exp, float) or\
       isinstance(exp, str) or isinstance(exp, unicode):
      return False
  raise CompileTypeError(exp)
  
def pythonize_list(exps, env, compiler):
  defs = ()
  exps2 = ()    
  for x in exps:
    exp = pythonize(x, env, compiler)
    try: 
      exp.is_function
      defs += (exp,)
      exps2 += (exp.name,)
    except:
      exps2 += (exp,)
  return defs, exps2

