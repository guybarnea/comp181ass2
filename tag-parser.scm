(load "qq.scm")

;;;;;;;;;;;;;;;;;;;;;; helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






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
; 	(parse2 (car exps))
; 	`(seq ,(handle-multi-begin (map parse2 exps)))))
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
; 	`(if3 ,(parse2 test) ,(parse2 dit) ,(parse2 dif)))
; ))



; ;;;;;;;;;;;;;;;;;;;;;; if-without-else Condition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define if-without-else-condition?
;   (pattern-rule
;       `(if ,(? 'test) ,(? 'dit))
;       (lambda (test dit)
; 	`(if3 ,(parse2 test) ,(parse2 dit) ,(parse2 (void))))
; ))



; ;;;;;;;;;;;;;;;;;;;;;; or ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; (define or?
;   (pattern-rule
;       `(or ,(? 'first) . ,(? 'rest))
;       (lambda (first rest)
; 	`(or ,`(,(parse2 first) ,@(map parse2 rest))))
; ))





; ;;;;;;;;;;;;;;;;;;;;;; lambda-simple ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (define lambda-simple?
;   (pattern-rule
;       `(lambda ,(? 'args (lambda (x)  (or (null? x) (and (list? x) (map var? x))))) ;(and (not (contains-duplicates? x))
;        ,(? 'first-body) . ,(? 'rest-bodies))
;       (lambda (args body other-bodies)
; 	`(lambda-simple ,args ,(parse2 `(begin ,body ,@other-bodies))))
; ))
; ;;;;;;;;;;;;;;;;;;;;;; parse2 function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *reserved-words*
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

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
	(parse2 (car exps))
	`(seq ,(handle-multi-begin (map parse2 exps)))))
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

(define or-without-args?
  (lambda (exp)
  	 (and (list? exp) (= (length exp) 1) (eq? (car exp) 'or))))

(define or-with-one-args?
  (lambda (exp)
	 (and (list? exp) (= (length exp) 2) (eq? (car exp) 'or))))


(define or?
  (lambda (exp)
  	(and (list? exp) (> (length exp) 2) (eq? (car exp) 'or))))

(define lambda-simple?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (check-simple-params? (cadr exp)))))

(define lambda-opt?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (improper-list? (cadr exp)))))

(define lambda-var?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (var? (cadr exp)))))

(define lambda-var-improper?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'lambda-im) (> (length exp) 2) (var? (cadr exp)))))

(define regular-define?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (var? (cadr exp)))))

(define mit-style-define?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (list? (cadr exp)))))

(define mit-style-improper-define-one-arg?
	(lambda(exp)
		(let ((lst (cadr exp)))
			(and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (improper-list? lst) (= (length-improper-left lst) 1)))))

;same as before for now...
(define mit-style-improper-define-more-than-one-arg?
	(lambda(exp)
		(and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (improper-list? (cadr exp)))))

(define length-improper-left
	(lambda(im-lst)
		(if (pair? im-lst)
			(+ 1 (length-improper-left (cdr im-lst)))
			0 )))
			

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

(define cond-with-one-exp?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'cond) (= (length exp) 2))))

(define cond-with-else?
	(lambda(exp)
	(and (list? exp) (eq? (car exp) 'cond) (else-in-the-end? (cdr exp)))))

(define cond-without-else?
	(lambda(exp)
	(and (list? exp) (eq? (car exp) 'cond) (not(else-in-the-end? (cdr exp))))))

;return true if the last expression is else
(define else-in-the-end?
	(lambda(lst)
		(equal? (car(get-last-exp lst))  'else)))

;get a list of expressions and return the last expression
(define get-last-exp
		(lambda(lst)
			(if (= (length lst) 1)
				(car lst)
				(get-last-exp (cdr lst)))))

#| (define cond?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'cond) (> (length exp) 2)))) |#

(define else?
	(lambda(exp)
		(and (list? exp) (eq? (car exp) 'else) (= (length exp) 2))))

;get list and return the same list without the last element
(define get-all-but-last
	(lambda(lst)
		(if (> (length lst) 2)
			(cons (car lst) (get-all-but-last (cdr lst)))
			(cons (car lst) '()))))

;get all elements except first element
(define get-all-but-first
	(lambda(lst) (cdr lst)))

;get a list and return the same list without the  two last element
(define get-all-but-last-two
	(lambda(lst)
		(if (> (length lst) 3)
			(cons (car lst) (get-all-but-last-two (cdr lst)))
			(cons (car lst) '()))))

;get a list and return all of the list except first and two last elements
(define get-all-but-first-and-last-two
	(lambda(lst)
		(let ((without-first (get-all-but-first lst)))
			(get-all-but-last-two without-first))))

;input: list. return: two last elements
(define get-two-last
	(lambda(lst)
		(if(> (length lst) 2)
			 (get-two-last (cdr lst))
			 lst)))


; ################################################################
(define quasiquote?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'quasiquote) (= (length exp) 2))))

(define parse2
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
					  `(if3 ,(parse2 test) ,(parse2 then) ,(parse2 else))))

			  ;if without else
			((if-without-else? exp)
				(let ((test (cadr exp))
					  (then (caddr exp)))
					   `(if3 ,(parse2 test) ,(parse2 then) ,(parse2 (void)))))


			  ;or-without-args
			  ((or-without-args? exp) (parse2 #f))

			  ;or-with-one-args
			  ((or-with-one-args? exp) (parse2 (cadr exp)))
			  ;or
			  ((or? exp) `(or ,(map parse2 (cdr exp))))

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

			   ; lambda-var-improper-more-than-one-arg
			   ((lambda-var-improper? exp)
			   	(let ((arg (cadr exp))
			   		  (body (cddr exp)))
			   	`(lambda-var ,arg  ,(handle-seq body))))

			  ;regular-define
			   ((regular-define? exp)
			   	(let ((var (cadr exp))
			   		  (val (cddr exp)))
			   	`(def ,(parse2 var) ,(handle-seq val))))

			   ;mit-style-define
			   ((mit-style-define? exp)
			   	(let* ((var (car (cadr exp)))
			   		   (args (cdr (cadr exp)))
			   		   (body (cddr exp)))
			   	(parse2 `(define ,var ,`(lambda ,args ,@body)))))

			   ;mit-style-improper-define
			   ((mit-style-improper-define-one-arg? exp)
			    (let* ((var (car (cadr exp)))
			   		   (args (cdr (cadr exp)))
			   		   (body (cddr exp)))
			     (display "exp1 = ") (display exp) (display "\n")
			   	(parse2 `(define ,var ,`(lambda-im ,args ,@body)))))

			   ;mit-style-improper-define-more-than-one-arg?
			   ((mit-style-improper-define-more-than-one-arg? exp)
			    (let* ((var (car (cadr exp)))
			   		   (args (cdr (cadr exp)))
			   		   (body (cddr exp)))
			    (display "exp2 = ") (display exp) (display "\n")
			   	(parse2 `(define ,var ,`(lambda ,args ,@body)))))


;(define (foo x y . z) (if x y z) #t): Failed! ☹ , 
;Expected: (def (var foo) (lambda-opt (x y) z (seq ((if3 (var x) (var y) (var z)) (const #t))))), 
;Actual:   (def (var foo) (applic (var lambda-im) ("what are you doing here\n" (if3 (var x) (var y) (var z)) (const #t))))



			   ;set!
			   ((set? exp)
			   	(let ((var (cadr exp))
			   		  (val (cddr exp)))
			   	 `(set ,(parse2 var) ,(handle-seq val))))

			   ;applic
			   ((applic? exp)
			   	(let ((first-body (car exp))
			   		  (rest-bodies (cdr exp)))
			   	`(applic ,(parse2 first-body) ,(map parse2 rest-bodies))))

			   ;let-without-bindings
			   ((let-without-bindings? exp)
			   	(let ((first-body (caddr exp))
			   		  (rest-bodies (cdddr exp)))
			   	(parse2 `((lambda () ,first-body ,@rest-bodies)))))

			   ;let-with-bindings
			   ((let-with-bindings? exp)
			   	(let* ((bindings (cadr exp))
			   		   (args-names (map car bindings))
			   		   (args-values (map cadr bindings)))
	      		(parse2 `((lambda ,args-names ,(caddr exp) ,@(cdddr exp)) ,@args-values))))

			   ;let-star-without-bindings
			   ((let-star-without-bindings? exp)
			   	(let ((first-body (caddr exp))
			   		  (rest-bodies (cdddr exp)))
			   	(parse2 `(let () ,first-body ,@rest-bodies))))

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
				(parse2 `(let ((,key ,@val)) ,@(handle-bindings other-bindings body other-bodies)))))

			   ;letrec-without-bindings
			   ((letrec-without-bindings? exp)
			   	(let ((first-body (caddr exp))
			   		  (rest-bodies (cdddr exp)))
			   	(parse2 `(let () (let () ,first-body  ,@rest-bodies)))))

			   ;letrec-with-bindings
			   ((letrec-with-bindings? exp)
			   	(let* ((key (caaadr exp))
			   		  (val (cdaadr exp))
			   		  (other-bindings (cdadr exp))
			   		  (body (caddr exp))
			   		  (other-bodies (cdddr exp))
			   		  (args-names (append (list key) (map car other-bindings)))
			   		  (set-args (map (lambda (x) `(set! ,(car x) ,(cadr x))) (append (list (list key (car val))) other-bindings))))
			   		  (parse2 `((lambda ,args-names ,@set-args ,`(let () ,body ,@other-bodies)) ,@(make-list (length args-names) #f)))))


			   ;and-without-args
			   ((and-without-args? exp) (parse2 #t))

			   ;and-with-one-arg
			   ((and-with-one-arg? exp) (parse2 (cadr exp)))

			   ;and
			   ((and? exp)
			   (let ((first (cadr exp))
			   		(rest (cddr exp)))
			   `(if3 ,(parse2 first) ,(parse2 `(and ,@rest)) ,(parse2 #f))))

			   ;cond-with-one-exp
			   ((cond-with-one-exp? exp)
			   	  (let ((cond (caadr exp))
			   	  		(body (cdadr exp)))
			   		(if (equal? (caadr exp) 'else)
		      		(handle-seq (cdadr exp))
		      `(if3 ,(parse2 cond) ,(handle-seq body) ,(parse2 (void))))))

			   ;cond-without-else
			   ((cond-without-else? exp)
			   	(let ((first-cond (caadr exp))
			   		  (first-body (cdadr exp))
			   		  (rest-conds (cddr exp)))
			   	`(if3 ,(parse2 first-cond) ,(handle-seq first-body) ,(map (lambda(cond-clause)
			   		`(if3 ,(parse2 (car cond-clause)) ,(handle-seq (cdr cond-clause)))) rest-conds))))

   			   ;cond-with-else
			   ((cond-with-else? exp)
			   	(let ((lst-except-two (get-all-but-first-and-last-two exp))
			   		  (lst-two-last (get-two-last exp)))
			   	;need to sequence those two actions
			   		(map (lambda(cond-clause) `(if3 ,(parse2 (car cond-clause)) ,(handle-seq (cdr cond-clause)))) lst-except-two)
			   		`(if3 ,(parse2 (caar lst-two-last)) ,(handle-seq (cdar lst-two-last)) ,(handle-seq (cdadr lst-two-last))))) 


 ;cond
#| 			   ((cond? exp)
			   	(let ((first-cond (caadr exp))
			   		  (first-body (cdadr exp))
			   		  (rest-conds (caddr exp)))
			   	`(if3 ,(parse first-cond) ,(handle-seq first-body) ,(parse `(cond ,rest-conds))))) |#
;(cond ((zero? n) 1) ((positive? n) 2) (else 5)): Failed! ☹ , 
;Expected: (if3 (applic (var zero?) ((var n))) (const 1) (if3 (applic (var positive?) ((var n))) (const 2) (const 5)))
;Actual:   (if3 (applic (var zero?) ((var n))) (const 1) (if3 (applic (var positive?) ((var n))) (const 2)) (const 5))  

			   	;quasi-quote
			   	((quasiquote? exp)
			   	  (parse2 (expand-qq (cadr exp))))
			(else "what are you doing here\n")))
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
;                 (run sexpr (lambda () (error 'parse2 (format "ERROR in ~s" sexpr))))))
)
  
