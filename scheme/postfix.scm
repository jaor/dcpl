(define-module (dcpl postfix))

(define-syntax postfix
  (syntax-rules ()
    ((postfix arg-no a ...)
     (postfix* arg-no (post-args a ...)))))

(define-syntax post-args
  (syntax-rules ()
    ((post-args) '())
    ((post-args (a ...) b ...) (cons (list a ...) (post-args b ...)))
    ((post-args a b ...) (cons a (post-args b ...)))))

(define (postfix* arg-no cmds)
  (lambda stack
    (if (< (length stack) arg-no)
        (error "Not enough arguments" stack)
        (check-num (car (exec-postfix cmds stack)) "postfix"))))

(define (exec-postfix cmds stack)
  (define (to-cmd cmd)
    (if (procedure? cmd) cmd (push cmd)))
  (cond ((null? cmds) stack)
        (else (exec-postfix (cdr cmds)
                            ((to-cmd (car cmds)) stack)))))

(define (err name msg)
  (error (string-append name ": " msg)))

(define (check-num n name)
  (if (not (number? n))
      (err name "not a number")
      n))

(define (check-underflow stack len name)
  (if (< (length stack) len)
      (err name "stack underflow")))

(define subr list)

(define exec
  (lambda (stack)
    (check-underflow stack 1 "exec")
    (let ((cmds (car stack)))
      (if (not (list? cmds))
          (error "exec: not a command")
          (exec-postfix cmds (cdr stack))))))

(define (push v)
  (lambda (stack) (cons v stack)))

(define pop cdr)

(define sel
  (lambda (stack)
    (check-underflow stack 3 "sel")
    (let ((z (car stack))
          (o (cadr stack))
          (v (check-num (caddr stack) "sel")))
      (cons (if (= 0 v) z o) (cdddr stack)))))

(define swap
  (lambda (stack)
    (check-underflow stack 2 "swap")
    (cons (cadr stack) (cons (car stack) (cddr stack)))))

(define nget
  (lambda (stack)
    (let ((n (check-num (car stack) "nget"))
          (rest (cdr stack)))
      (check-underflow rest n "nget")
      (cons (list-ref rest (- n 1)) rest))))

(define (make-arith op name)
  (lambda (stack)
    (check-underflow stack 2 name)
    (let ((x (check-num (car stack) name))
          (y (check-num (cadr stack) name)))
      (cons (op y x) (cddr stack)))))

(define add (make-arith + "add"))

(define sub (make-arith - "sub"))

(define mul (make-arith * "mul"))

(define div (make-arith / "div"))

(define rem (make-arith remainder "rem"))

(define (make-rel op name)
  (make-arith (lambda (x y) (if (op x y) (push 1) (push 0))) name))

(define lt (make-arith < "lt"))

(define gt (make-arith > "gt"))

(define eqp (make-arith eqv? "eqp"))

(define notp (subr 0 1 sel))
