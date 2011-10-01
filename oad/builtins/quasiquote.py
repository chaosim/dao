from oad import builtin
from oad.builtin import Builtin, Function
from oad.term import Apply

# quasiquote and backquote

##@builtin.macro('quasiquote')
##def quasiquote(solver, cont, item):
##  if isinstance(item, list):
##    if item[0]==backquote:
##      if len(item)!=2: raise Exception
##      if quote_level==0: 
##        return solver.solve(item[1], cont)
##      else: 
##        return quasiquote(item[1], quote_level-1)
##    else:
##      
##  if isinstance(item, tuple):
##    return tuple(quasiquote(x) for x in item)
##  yield solver.cont(func, lambda value, solver:
##    ((cont, True) for _ in var.unify(value, solver.env))), True

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

##(if (> i 1) i (+ i 1))
##`(if  ,i>1: ,i; else ,i+1)

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
