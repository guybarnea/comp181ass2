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

(define quote-const?
  (lambda (exp)
  	(and  (pair? exp) (eq? (car exp) 'quote))))

(define if?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'if) (= (length exp) 4))))

(define if-without-else?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'if) (= (length exp) 3))))

(define or?
  (lambda (exp)
  	(and (pair? exp)) (eq? (car exp) 'or)))

(define lambda-simple?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (check-simple-params? (cadr exp)))))

(define lambda-opt?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (improper-list? (cadr exp)))))

(define lambda-var?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (var? (cadr exp)))))

(define regular-define?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (var? (cadr exp)))))

(define mit-style-define?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (list? (cadr exp)))))

(define set?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'set!) (> (length exp) 2) (var? (cadr exp)))))

(define applic?
  (lambda (exp)
  	(and (list? exp) (not (member (car exp) *reserved-words*)))))

(define let-without-bindings?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'let) (> (length exp) 2) (null? (cadr exp)))))


(define let-with-bindings?
  (lambda (exp)
	(and (list? exp)  (eq? (car exp) 'let) (> (length exp) 2))))

(define let-star-without-bindings?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'let*) (> (length exp) 2) (null? (cadr exp)))))

(define let-star-with-bindings?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'let*) (> (length exp) 2))))

(define letrec-without-bindings?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'letrec) (> (length exp) 2) (null? (cadr exp)))))

(define letrec-with-bindings?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'letrec) (> (length exp) 2))))

(define and-without-args?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'and) (= (length exp) 1))))

(define and-with-one-arg?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'and) (= (length exp) 2))))

(define and?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'and) (> (length exp) 2))))

(define parse
	(lambda (exp)
		(cond 
			;const
			((constant? exp) `(const ,exp))

			;quote-const
			((quote-const? exp) `(const ,@(cdr exp)))

			  ;var
			((var? exp) `(var ,exp))

			  ;if
			((if? exp)
				(let ((test (cadr exp))
					  (then (caddr exp))
					  (else (cadddr exp)))
					  `(if3 ,(parse test) ,(parse then) ,(parse else))))

			  ;if without else
			((if-without-else? exp)
				(let ((test (cadr exp))
					  (then (caddr exp)))
					   `(if3 ,(parse test) ,(parse then) ,(parse (void)))))

			  ;or
			  ((or? exp) `(or ,(map parse (cdr exp))))

			  ;lambda-simple
			  ((lambda-simple? exp)
			  	(let ((args (cadr exp))
                     (body (cddr exp)))
			   `(lambda-simple ,args ,(handle-seq body))))

			  ;lambda-opt
			  ((lambda-opt? exp)
			  	(let* ((args-list (flatten (cadr exp)))
	      			   (rev-args-list (reverse args-list)))
				`(lambda-opt ,(reverse (cdr rev-args-list)) ,(car rev-args-list) ,(handle-seq (cddr exp)))))

			  ; lambda-var
			   ((lambda-var? exp)
			   	(let ((arg (cadr exp))
			   		  (body (cddr exp)))
			   	`(lambda-var () ,arg ,(handle-seq body))))

			  ;regular-define
			   ((regular-define? exp)
			   	(let ((var (cadr exp))
			   		  (val (cddr exp)))
			   	`(def ,(parse var) ,(handle-seq val))))

			   ;mit-style-define
			   ((mit-style-define? exp)
			   	(let* ((var (car (cadr exp)))
			   		   (args (cdr (cadr exp)))
			   		   (body (cddr exp)))
			   	(parse `(define ,var ,`(lambda ,args ,@body)))))

			   ;set!
			   ((set? exp)
			   	(let ((var (cadr exp))
			   		  (val (cddr exp)))
			   	 `(set ,(parse var) ,(handle-seq val))))

			   ;applic
			   ((applic? exp)
			   	(let ((first-body (car exp))
			   		  (rest-bodies (cdr exp)))
			   	`(applic ,(parse first-body) ,(map parse rest-bodies))))

			   ;let-without-bindings
			   ((let-without-bindings? exp)
			   	(let ((first-body (caddr exp))
			   		  (rest-bodies (cdddr exp)))
			   	(parse `((lambda () ,first-body ,@rest-bodies)))))

			   ;let-with-bindings
			   ((let-with-bindings? exp)
			   	(let* ((bindings (cadr exp))
			   		   (args-names (map car bindings))
			   		   (args-values (map cadr bindings)))
	      		(parse `((lambda ,args-names ,(caddr exp) ,@(cdddr exp)) ,@args-values))))

			   ;let-star-without-bindings
			   ((let-star-without-bindings? exp)
			   	(let ((first-body (caddr exp))
			   		  (rest-bodies (cdddr exp)))
			   	(parse `(let () ,first-body ,@rest-bodies))))

			   ;let-star-with-bindings
			   ((let-star-with-bindings? exp)
			   	(let ((handle-bindings (lambda (bindings body other-bodies)
				  (if (null? bindings)
				    `(,body ,@other-bodies)
				    `((let* ,bindaings ,body ,@other-bodies)))))
			   		  (key (caaadr exp))
			   		  (val (cdaadr exp))
			   		  (other-bindings (cdadr exp))
			   		  (body (caddr exp))
			   		  (other-bodies (cdddr exp)))
				(parse `(let ((,key ,@val)) ,@(handle-bindings other-bindings body other-bodies)))))

			   ;letrec-without-bindings
			   ((letrec-without-bindings? exp)
			   	(let ((first-body (caddr exp))
			   		  (rest-bodies (cdddr exp)))
			   	(parse `(let () (let () ,first-body  ,@rest-bodies)))))

			   ;letrec-with-bindings
			   ((letrec-with-bindings? exp)
			   	(let* ((key (caaadr exp))
			   		  (val (cdaadr exp))
			   		  (other-bindings (cdadr exp))
			   		  (body (caddr exp))
			   		  (other-bodies (cdddr exp))
			   		  (args-names (append (list key) (map car other-bindings)))
			   		  (set-args (map (lambda (x) `(set! ,(car x) ,(cadr x))) (append (list (list key (car val))) other-bindings))))
			   		  (parse `((lambda ,args-names ,@set-args ,`(let () ,body ,@other-bodies)) ,@(make-list (length args-names) #f)))))


			   ;and-without-args
			   ((and-without-args? exp) (parse #t))

			   ;and-with-one-arg
			   ((and-with-one-arg? exp) (parse (cadr exp)))

			   ;and
			   ((and? exp)
			   (let ((first (cadr exp))
			   		(rest (cddr exp)))
			   `(if3 ,(parse first) ,(parse `(and ,@rest)) ,(parse #f))))





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
  