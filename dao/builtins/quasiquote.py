from dao import builtin
from dao.builtin import Builtin, Function
from dao.term import CommandCall
from dao.solve import DaoSyntaxError, mycont

# quasiquote and backquote

## dao and t language
## (if (> i 1) i (+ i 1))
##`(if  ,i>1: ,i; else ,i+1)

def evaluate_quasiquote_list_cont(solver, exps):
  @mycont(cont)
  def quasi_cont(result, solver):
    if len(exps)==0: 
      yield cont, result
    else:
      element0 = exps[0]
      left_cont = evaluate_quasiquote_list_cont(solver, exps[1:])
      if element0==():
        yield left_cont, result+((),)
        return
      if not isinstance(element0, tuple):
        if element0==unquote or element0==unquote_splice:
          raise DaoSyntaxError
        else: 
          yield left_cont, result+(element0,)
          return
      elif len(element0)==2:
        if element0[0]==unquote:
          @mycont(quasi_cont)
          def gather_cont(value, solver):
            yield left_cont, result+(value,)
          yield solver.cont(element0[1], gather_cont), True
          return
        elif element0[0]==unquote_splice:
          @mycont(quasi_cont)
          def gather_cont(value, solver):
            yield left_cont, result+value
          yield solver.cont(element0[1], gather_cont), True
          return
      elif element0[0]==unquote or element0[0]==unquote_splice:
        raise DaoSyntaxError
      @mycont(quasi_cont)
      def gather_cont(value, solver):
        yield left_cont, result+(value,)
      yield evaluate_quasiquote_list_cont(solver, gather_cont, element0), ()
  return quasi_cont

@builtin.macro('quasiquote')
def quasiquote(solver, item):
  if not isinstance(item, tuple) or item==():
    yield cont, item
    return
  elif len(item)==2:
    if item[0]==unquote:
      yield solver.cont(item[1], cont), True
      return
    elif item[0]==unquote_splice:
      raise DaoSyntaxError
  elif item[0]==unquote or item[0]==unquote_splice:
    raise DaoSyntaxError
  
  yield evaluate_quasiquote_list_cont(solver, item), ()

@builtin.macro('unquote')
def unquote(solver, *args):
  raise DaoSyntaxError

@builtin.macro('unquote_splice')
def unquote_splice(solver, *args):
  raise DaoSyntaxError

##Back when JAR first suggested making quasiquote standard, I transcribed
##my quasiquote implementation from the C-coded reader into Scheme-coded
##syntactic-extensions.  I promised to send the code to David Bartley at
##TI and figured some of the rest of you might be interested as well.
##
##I believe that this gives different results from JAR's, because it can
##actually fold up explicit calls to "list" and "list*" (for better or for
##worse).  It also insists that quasiquote, unquote, and unquote-splice
##forms be well-formed, rather than ignoring those that aren't.  As with
##JAR's, nested quasiquotes work properly.
##
##Because quasiquote and company are expanded at compile time rather than
##read time, it is reasonable to write code that produces quasiquote forms.  
##
##"list*" (Common Lisp's name) is the same as JAR's "cons*".  The meaning
##of everything else should be obvious.
##
##(let ((check
##         (lambda (x)
##            (unless (and (pair? (cdr x)) (null? (cddr x)))
##               (ferror (car x) "invalid form ~s" x)))))
##   (define-macro! quasiquote (x)
##      (recur f ((x x))
##         (cond
##            ((not (pair? x)) `',x)
##            ((eq? (car x) 'quasiquote) (check x) (f (f (cadr x))))
##            ((eq? (car x) 'unquote) (check x) (cadr x))
##            ((eq? (car x) 'unquote-splice)
##             (ferror 'unquote-splice "invalid context for ~s" x))
##            ((and (pair? (car x)) (eq? (caar x) 'unquote-splice))
##             (check (car x))
##             (let ((d (f (cdr x))))
##                (if (equal? d '(quote ()))
##                    (cadar x)
##                    `(append ,(cadar x) ,d))))
##            (else
##             (let ((a (f (car x))) (d (f (cdr x))))
##                (if (pair? d)
##                    (if (eq? (car d) 'quote)
##                        (if (and (pair? a) (eq? (car a) 'quote))
##                            `'(,(cadr a) . ,(cadr d))
##                            (if (null? (cadr d))
##                                `(list ,a)
##                                `(list* ,a ,d)))
##                        (if (memq (car d) '(list list*))
##                            `(,(car d) ,a ,@(cdr d))
##                            `(list* ,a ,d)))
##                   `(list* ,a ,d))))))))
##
##(define-macro! unquote (x)
##   (ferror 'unquote
##      "unquote form ,~s not valid outside of quasiquote"
##      x))
##
##(define-macro! unquote-splice (x)
##   (ferror 'unquote
##      "unquote-splice form ,@~s not valid outside of quasiquote"
##      x))

##;; usage:  (qq (x y (uq (+ 1 2)) (uq@ (list 1 2 3))))
##  ;;  ==>  (x y 3 1 2 3)
##
##  (define-macro (qq s-expr)
##    (qq-eval s-expr))
##
##
##  ;; Since qq is a macro you can't use (args) within.
##  ;; Use qq-eval instead which is not a macro and thus
##  ;; (args) will not capture the qq's (args).
##
##  ;; usage:  (qq-eval '(x y (uq (+ 1 2)) (uq@ (list 1 2 3))))
##  ;;  ==>  (x y 3 1 2 3)


##  (define (qq-eval s-expr , i)
##    (if (list? s-expr)
##      (begin
##        (setq i 0)
##        (while (< i (length s-expr))
##          (let ((ss-expr (nth i s-expr)))
##            (if (list? ss-expr)
##              (cond 
##                ((= 'uq (first ss-expr))
##                  (nth-set i s-expr (eval (qq-eval (last ss-expr))))
##                  (inc 'i))
##                ((= 'uq@ (first ss-expr))
##                  (let ((ss-exprs (eval (qq-eval (last ss-expr)))))
##                    (if (list? ss-exprs)
##                      (begin
##                        (pop s-expr i)
##                        (dotimes (j (length ss-exprs))
##                          (push (nth j ss-exprs) s-expr i)
##                          (inc 'i)))
##                      (begin
##                        (nth-set i s-expr ss-exprs)
##                        (inc 'i)))))
##                 (true
##                   (nth-set i s-expr (qq-eval ss-expr))
##                   (inc 'i)))
##              (begin
##                (inc 'i)
##                s-expr))))
##           s-expr)
##      s-expr))
##


##  ;; Abbreviation for lambda or fn
##
##  (define-macro (\ )
##    (eval (qq-eval '(lambda (uq (first (args))) (uq@ (rest (args)))))))
##
##
##  ;; Abbreviation for define
##
##  (define-macro (: _var-or-fn _value)
##    (if (list? _var-or-fn)
##      (eval (qq-eval '(define (uq _var-or-fn) (uq@ (rest (args))))))
##      (eval (qq (set _var-or-fn (uq _value))))))
##
