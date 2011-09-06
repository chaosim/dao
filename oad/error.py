from oad.term import Error, UnifyFail #atom, 

class CatchableError(Error):
  def __init__(self, data): self.data = data
class UncaughtError(Error): pass
class UserError(CatchableError): pass
class UncatchableError(Error): pass
class FunctionNotFound(Error): 
  def __init__(self, signature): self.signature = signature
class CutException(Error): 
  def __init__(self, continuation): self.continuation = continuation
def throw_instantiation_error(): raise CatchableError(atom("instantiation_error"))
def throw_type_error(valid_type, obj): 
  raise CatchableError(Term("type_error", [valid_type, obj]))
def throw_domain_error(valid_domain, obj):
  raise CatchableError(Term("domain_error", [valid_domain, obj]))
