;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Comp171 - ASS2 - Part 1 - Tests

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Change to your own location
(load "tag-parser.scm")
(load "tagparser.so")

(define my-parse-func parse)
(define staff-parse-func parse)

(define try-catch
  (lambda (try-thunk catch-thunk)
    (guard (c (else (catch-thunk)))
     (try-thunk))))

(define testVSstaff
	(lambda (input)
		(let* ((my-res (try-catch (lambda () (my-parse-func input)) (lambda () "ERROR")))
		      (staff-res (try-catch (lambda () (staff-parse-func input)) (lambda () (display "\033[1;34m !!Negative Test!! \033[0m ") "ERROR"))))
			(display (format "~s:" input))
			;(display my-res)
			(cond ((equal? my-res staff-res)
				(display (format "\033[1;32m Success! ☺ \033[0m \n")) #t)
				(else 
				(display (format "\033[1;31m Failed! ☹\033[0m , Expected: ~s, Actual: ~s \n" staff-res my-res)) #f))
			)))
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "==============================================")
	(newline)
	(let ((results (map testVSstaff lst)))
	(newline)
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)	
		(display (format "\033[1;32m~s Tests: SUCCESS! ☺ \033[0m\n \n" tests-name)) #t)		
		(else
		(display (format "\033[1;31m~s Tests: FAILED! ☹ \033[0m\n \n" tests-name)) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m !!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n"))
		(else (display "\033[1;31m #####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n")))
		(newline))
))

(define ifTests
  (list
    `(if a b c)
    `(if a b)
    `(if a 4)
    `(if #t 'abc)
    `(if (if a b c) 'c '(x y z))
    `(if (if a b c) 'c (if "abc" 'x 123))    
))

(define condTests
  (list
     ;without sequences     
     `(cond (x 1))
     `(cond (x 1) (c 12))
     `(cond (x 1) (b 2) (c 3))
     
     ; with sequences
     `(cond (x 1 2))
     `(cond (x 1 2 abc) (c 12))
     `(cond (x 1 2 abc1) (b #f) (c (or 1 2) #t))
     `(cond (x 1 2 abc1) (b #f) (c (or 1 2) #t) (else #f))
     `(cond (x 1 2 abc1) (b #f) (c (or 1 2) #t) (else 112))
     `(cond (x 1 2 abc1) (b #f) (c (or 1 2) #t) (else (display "I'm in Else") (f a b c)))
))

(define orTests
  (list
    `(or)
    `(or 1)
    `(or 1 2)
    `(or 1 2 3)
    `(or (or "abc" '123) (if a b c))
    `(or 'shaham)
    `(or "naimark")
))

(define andTests
  (list
    `(and)
    `(and 1)
    `(and 1 2)
    `(and 1 2 3 4)
    `(and 1 2 3 abc)
    `(and 1 2 3 13 4 5 4 7 3)
    `(and (or "abc" '123) (if a b c))
    `(and 'shaham "naimark")
    `(and "naimark")
))


(define variableTests
  (list
    'abc
    '123x
    'AbC1gfj5j959b949jm5b9gk5kg
    'let2*
    'set-bang!
))

(define letTests
  (list
    '(let () body)
    '(let ((x 1)) body)
    '(let ((x 1) (y 21)) body)
    '(let ((x 1) (y 21) (abcde fghijKlmnOP123)) body (if (> x 5) 4 3) #t)
    
    '(let* () body)
    '(let* ((x 1) (y 21)) body)
    '(let* ((x 1) (x abv)) body)
    '(let* ((x 1) (y 21) (abcde fghijKlmnOP123)) body (if (> x 5) 4 3) #t)
    
    '(letrec () body)
    '(letrec ((x 1)) b)
    '(letrec ((x 1) (y 2)) b1 b2)
    '(letrec ((x 1) (y 2) (a 5)) b1 b2 (or 1 2 3) (if 1 2 3) #t)
))

(define constantTests
  (list
    ''()   
    ''#(2)
    ''#(1 (2 3 4) 2 3)
    #f
    #\a  
    34
    "abc"
    '(quote a)
    '(quote (a b c))
    '(quote (quote a b c))
))

(define lambdaTests
  (list
    ; lambda-simple
    '(lambda () body)
    '(lambda (x) a)
    '(lambda (exp rest) (if a 1 "abc"))
    '(lambda (a b c d e f Symbol1) E1 E2 E3 E4 (f1 a))
    '(lambda (exp rest) (or a b c) (if a 1 "abc"))
    '(lambda (a b c d e arg154) (if a 1 "abc"))
    '(lambda () (or 1))
    '(lambda (Sym1 Symbol2 Symbol1234567890) (display "Akuna Matata"))       
    
    ;lambda-opt
    '(lambda (x y . z) a)
    '(lambda (abc . def) abc)
    '(lambda (exp . rest) (or a b c) (if a 1 "abc"))
    
    ;lambda-Variables
    '(lambda args (if a b c))
    '(lambda args args)
    '(lambda args (or a b c) (if a 1 "abc"))
))

(define defineTests
  (list
    ;regular-define
    '(define x 5)
    '(define x (lambda (x) x))
    '(define a b c d)
    '(define a (b c d))
    
    ;mit-style-define
    '(define (id x) x)
    '(define (id x) x y)
    '(define (foo x y z) (if x y z))
    '(define (foo x y . z) (if x y z) #t)
    '(define (list . args) args)
))

(define applicationTests
  (list
    '(a)
    '(a b c)
    '((a b) (a c) (a d))
    '((lambda (x y z . e) (e (f x y z123))))
))

(define quasiquoteTests
  (list
    `(quasiquote (a b c))
    `(quasiquote (a ,b c))
    `(quasiquote (a ,b ,@c))    
    `(quasiquote (,@a ,b c))
    `(quasiquote (,@a ,@b ,@c))    
    `(quasiquote (,a ,b ,c))  
    `(quasiquote (,a ,((f a) x) ,c))  
))

(define beginTests
  (list
    '(begin)
    '(begin 1)
    '(begin (or 1 2 3))
    '(begin (or 1 2) (if 1 2 3))   
    '(begin (begin a b) a)
    '(begin (begin a (begin c (begin d e f g))) a (a b c))
    '(begin (begin a b c) (begin d e f) (begin e f) g)
    '(begin (begin a c) b (begin d) g (begin e f) (or 1 2 3))
    '(begin (if (if 1 2 3) (and 2 "a" #f) #t) (begin d e) (lambda args a b c) (begin e f) (or 1 2 3))
    '(begin a (begin b (begin c (begin d e (begin f g h) "Akuna Matata"))))
))

(define setTests
  (list
    '(set! x 3)
    '(set! v (f x))
))

(define parserTests
  '(4 
	-3 
	#t 
	#f 
	#\a 
	#\A 
	#\space 
	#\tab
	'3 
	3 
	'"abc" 
	"abc" 
	'#\a 
	#\a 
	''a 
	''''''a
	'abc 
	abc 
	'#(a b c) 
	'#() 
	'#(() 
	(#()))
	(a b c)
	(if (zero? n) 1 (* n (fact (- n 1))))
	(cond ((foo? x) foo-x)
		((goo? x) goo-x)
		((boo? x) boo-x)
		(else poo-x))
	(begin e1)
	(begin e1 e2 e3)
	(lambda a b c)
	(lambda a b)
	(lambda (a b . c) (list a b c))
	(lambda (a b c) (list a b c))
	(let ((a 1) (b 2) (c 3))
	  (+ a b c))
	(let* ((a 1)
		   (a (+ a 1))
		   (a (+ a a))
		   (a (* a a)))
	  (set-car! x a)
	  a)
	(define a 3)
	(define a (lambda (x) x))
	(define (fact n) (if (zero? n) 1 (* n (fact (- n 1)))))
	(define (foo a b . c) (list a b c))
	(define (foo . a) (list a))
	(define (foo a b c) (list a b c))
))

(define negativeTests
  (list
    '(if)
    '(cond)
    '(let)
    '(lambda)
    '(letrec)
    '(let*)
    '(lambda (a b c a) (f x))
    '(let ((AbC 5) (Sym123 "abc") (AbC 12)) (if (= AbC 12) #t (begin (display "WOW") #f)))
    '(letrec ((AbC (lambda (x) (AbC x))) (Sym123 "abc") (AbC 12)) (if (= AbC 12) #t (begin (display "WOW") #f)))
))

(define LidanHifiAndKenSaggyTests
  (list
    #f
    #\a
    34
    "abc"
    '(quote a)
    '(quote (a b c))
    '(quote (quote a b c))
    'abc
    '123x
    '(if a b c)
    '(if (if a b c) 'x '(x y z))
    '(if a b)
    '(if a 4)
    '(if #t 'abc)
    '(lambda (x y z) (if x y z))
    '(lambda () a)
    '(lambda () 'ok)
    '(lambda (a b c) b1 b2 b3)
    '(lambda (x y z . rest) (if x y z))
    '(lambda (x . rest) rest)
    '(lambda (a b . c) b1 b2 b3)
    '(lambda args (if x y z))
    '(lambda args args)
    '(lambda v b1 b2 b3)
    '(lambda (x . rest) rest)
    '(define x 5)
    '(define x (lambda (x) x))
    '(define (id x) x)
    '(define (foo x y z) (if x y z))
    '(define (foo x y . z) (if x y z))
    '(define (list . args) args)
    '(a)
    '(a b c)
    '((a b) (a c))
    '(begin)
    '(begin e1 e2 e3 e4)
    '(begin (quote a) abc (quote b))
    '(lambda (x) (quote a) x (quote b))
    '(lambda a a a)
    '(lambda (a . b) a b)
    '(lambda (a b) a b)
    '(let ((v1 e1) (v2 e2)) b1 b2 b3)
    '(let ((v1 e1)) (* v1 2))
    '(let ((x 3) (y 4) (z 5)) (+ x y) #t #f)
    '(let ((a 1)) 5 a)
    '(let () 5)
    '(let ((a 1) (b 2)) (+ a b))
    '(letrec ((f1 (lambda (x) (+ 1 x))) (f2 (lambda (y) (* 1 y)))) 1)
    '(let* ((v1 e1) (v2 (+ v1 1))) b1 b2 b3)
    '(let* ((v1 e1) (v2 e2)) b1 b2 b3)
    '(let* ((a 1) (a 2)) a)
    '(let* ((a 1)) a b)
    '(let* ((a 1) (b 2)) a)
    '(let* () a b)
    '(and)
    '(and a)
    '(and a b)
    '(and a b c)
    '(and e1 e2 e3 e4)
    `(and a b c d e f)
    '(and 1 (if #f #f) 4)
    '(cond (else x))
    '(cond (else 3))
    '(cond (1 2 3) (else 4))
    '(cond (#f 2))
    '(cond (#f 2) (else 3))
    '(cond (x y))
    '(cond (x y) (else z))
    '(cond (a b) (c d))
    '(cond (a b) (c d) (else e))
    '(cond ((p1? x) e1 e2 e3) ((p2? y) e4 e5) ((p3? z) e6 e7 e8 e9) (else e10))
    '(cond ((zero? n) 1))
    '(cond ((zero? n) 1) ((positive? n) 2))
    '(cond ((zero? n) 1) (else 5))
    '(cond ((zero? n) 1) ((positive? n) 2) (else 5))
    '(cond (1 2 3) (else 4))
    '(define (fact n) (if (zero? n) 1 (* n (fact (- n 1)))))
    '(quasiquote (a b c))
    '(quasiquote (a ,b c))
    '(quasiquote (a ,b c))
    '(quasiquote (a ,b ,@c))
    '(quasiquote (,@a ,b c))
    
    
))

(display (format "\033[1mComp171 - Part 1 Tests\033[0m\n================================\n"))

(runAllTests
  (list
      (cons "Lambda" lambdaTests)     
      (cons "Or" orTests)   
      (cons "And" andTests) 
      (cons "If" ifTests)    
      (cons "Cond" condTests)      
      (cons "Constants" constantTests)    
      (cons "Variables" variableTests)     
      (cons "Define" defineTests)  
      (cons "Let" letTests)   
      (cons "Begin" beginTests)  
      (cons "Set" setTests) 
      (cons "Application" applicationTests)    
      (cons "QuasiQuote" quasiquoteTests) 
      (cons "Parser" parserTests)
      (cons "Negative" negativeTests)
      (cons "Comp151 - Lidan Hifi and Ken Saggy tests" LidanHifiAndKenSaggyTests)
))