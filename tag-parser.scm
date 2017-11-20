(load "qq.scm")

;;;;;;;;;;;;;;;;;;;;;; helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define *reserved-words*
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))



; (define handle-multi-begin
;   (lambda (x)
;     (cond ((null? x) '())
;     ((and (list? (car x)) (equal? (caar x) 'seq))
;      `(,@(cadar x) ,@(handle-multi-begin (cdr x))))
;      (else `(,(car x) ,@(handle-multi-begin (cdr x)))))
; ))

; (define handle-seq
;   (lambda (exps)
;     (if (null? (cdr exps))
; 	(parse (car exps))
; 	`(seq ,(handle-multi-begin (map parse exps)))))
; )

; (define begin?
; (pattern-rule `(begin ,@(? 'exps))
; (lambda (exps)
;       (handle-seq exps))
; ))

; ;;;;;;;;;;;;;;;;;;;;;; const ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (define ^void?
;   (lambda (x) 
;       (equal? (void) x)
;     )
; )


; (define quoted-constant?
;   (pattern-rule
;       `(quote ,(? 'e))
;       (lambda (e) `(const ,e))
; ))                   

; (define constant?
;     (pattern-rule
;         (? 'c const?)
;         (lambda (e)
;             `(const ,e))
; )) 

; (define void?
;     (pattern-rule
;         (? 'c ^void?)
;         (lambda (e)
;             `(const ,e))
; ))   

; ;;;;;;;;;;;;;;;;;;;;;; var ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define var?
;   (lambda (var) (and (not (member var *reserved-words*)) (symbol? var))
; ))
 
; ;;;;;;;;;;;;;;;;;;;;;; if Condition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (define if-condition?
;   (pattern-rule
;       `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
;       (lambda (test dit dif)
; 	`(if3 ,(parse test) ,(parse dit) ,(parse dif)))
; ))



; ;;;;;;;;;;;;;;;;;;;;;; if-without-else Condition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define if-without-else-condition?
;   (pattern-rule
;       `(if ,(? 'test) ,(? 'dit))
;       (lambda (test dit)
; 	`(if3 ,(parse test) ,(parse dit) ,(parse (void))))
; ))



; ;;;;;;;;;;;;;;;;;;;;;; or ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; (define or?
;   (pattern-rule
;       `(or ,(? 'first) . ,(? 'rest))
;       (lambda (first rest)
; 	`(or ,`(,(parse first) ,@(map parse rest))))
; ))





; ;;;;;;;;;;;;;;;;;;;;;; lambda-simple ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (define lambda-simple?
;   (pattern-rule
;       `(lambda ,(? 'args (lambda (x)  (or (null? x) (and (list? x) (map var? x))))) ;(and (not (contains-duplicates? x))
;        ,(? 'first-body) . ,(? 'rest-bodies))
;       (lambda (args body other-bodies)
; 	`(lambda-simple ,args ,(parse `(begin ,body ,@other-bodies))))
; ))
; ;;;;;;;;;;;;;;;;;;;;;; parse function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define handle-multi-begin
  (lambda (x)
    (cond ((null? x) '())
    ((and (list? (car x)) (equal? (caar x) 'seq))
     `(,@(cadar x) ,@(handle-multi-begin (cdr x))))
     (else `(,(car x) ,@(handle-multi-begin (cdr x)))))
))

(define handle-seq
  (lambda (exps)
    (if (equal? (length exps) 1)
	(parse (car exps))
	`(seq ,(handle-multi-begin (map parse exps)))))
)

(define flatten
  (lambda (x)
    (cond 
      ((null? x) '())
      ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
      (else (list x)))
))  

(define contains-duplicates?
  (lambda (lst)
    (not (andmap 
      (lambda (x) 
	(equal? 
	  (- (length lst) 1) 
	  (length (filter (lambda (y) (not (equal? x y))) 
		    lst))
	)) 
    lst))
)) 

(define improper-list?
  (lambda (x) (and (pair? x) (not (list? x)))))

(define var?
  (lambda (var) (and (not (member var *reserved-words*)) (symbol? var))
))

(define check-simple-params?
(lambda (x) (and (list? x) (not (contains-duplicates? x)) (or (null? x) (and (list? x) (andmap var? x))))))


(define constant?
  (lambda (x) 
    (or 
      (and (symbol? x) (null? x)) 
      (vector? x) 
      (boolean? x) 
      (char? x) 
      (number? x) 
      (string? x) 
      (equal? (void) x)
    )
))

(define parse
	(lambda (exp)
		(cond 
			;const
			((constant? exp) `(const ,exp))
			;quote-const
			  ((and (pair? exp) (eq? (car exp) 'quote)) `(const ,@(cdr exp)))
			  ;var
			  ((var? exp) `(var ,exp))
			  ;if
			  ((and (list? exp) (= (length exp) 4)
			  	(eq? (car exp) 'if)) `(if3 ,(parse (cadr exp)) ,(parse (caddr exp)) ,(parse (cadddr exp))))
			  ;if without else
			  ((and (list? exp) (= (length exp) 3)
			  	(eq? (car exp) 'if)) `(if3 ,(parse (cadr exp)) ,(parse (caddr exp)) ,(parse (void))))
			  ;or
			  ((and (pair? exp) (eq? (car exp) 'or)) `(or ,(handle-seq (cdr exp))))
			  ;lambda-simple
			  ((and (list? exp) (> (length exp) 2) (check-simple-params? (cadr exp)) (eq? (car exp) 'lambda))
			   `(lambda-simple ,(cadr exp) ,(handle-seq (cddr exp))))
			  ;lambda-opt
			  ((and (list? exp) (> (length exp) 2) (improper-list? (cadr exp)) (eq? (car exp) 'lambda))
			  	(let* ((args-list (flatten (cadr exp)))
	      			   (rev-args-list (reverse args-list)))
				`(lambda-opt ,(reverse (cdr rev-args-list)) ,(car rev-args-list) ,(handle-seq (cddr exp)))))
			  ; lambda-var
			   ((and (list? exp) (> (length exp) 2) (var? (cadr exp)) (eq? (car exp) 'lambda))
			   	`(lambda-var () ,(cadr exp) ,(handle-seq (cddr exp))))
			  ;regular-define
			   ((and (list? exp) (> (length exp) 2) (var? (cadr exp)) (eq? (car exp) 'define))
			   	`(def ,(parse (cadr exp)) ,(handle-seq (cddr exp))))
			   ;mit-style-define
			   ((and (list? exp) (> (length exp) 2) (list? (cadr exp)) (eq? (car exp) 'define))
			   	(parse `(define ,(car (cadr exp)) ,`(lambda ,(cdr (cadr exp)) ,@(cddr exp)))))

			   ;set!
			   ((and (list? exp) (> (length exp) 2) (var? (cadr exp)) (eq? (car exp) 'set!))
			   	 `(set ,(parse (cadr exp)) ,(handle-seq (cddr exp))))

			   ;applic
			   ((and (list? exp) (not (member (car exp) *reserved-words*)))
			   	`(applic ,(parse (car exp)) ,(map parse (cdr exp))))

			   ;let-without-bindings
			   ((and (list? exp) (> (length exp) 2) (null? (cadr exp)) (eq? (car exp) 'let))
			   	(parse `((lambda () ,(caddr exp) ,@(cdddr exp)))))
			   ;let-with-bindings
			   ((and (list? exp) (> (length exp) 2) (eq? (car exp) 'let))
			   	(let* ((bindings (cadr exp))
			   		   (args-names (map car bindings))
			   		   (args-values (map cadr bindings)))
	      		(parse `((lambda ,args-names ,(caddr exp) ,@(cdddr exp)) ,@args-values))))





			(else 1)))
; (let ((run (compose-patterns 
; 	or?
; 	void?
; 	quoted-constant?
;     constant? 
;     variable? 
;     if-without-else-condition?
;     if-condition? 
;     lambda-simple?
;     begin?
;     )))
; 	(lambda (sexpr)
;                 (run sexpr (lambda () (error 'parse (format "ERROR in ~s" sexpr))))))
)
  
