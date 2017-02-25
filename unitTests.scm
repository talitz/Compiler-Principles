;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Comp171 - Compiler -  Unit Tests

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "compiler.scm")

(define assert
	(lambda (f input output)
		(let* ((my-res (apply f input)))
			(display (format "~s:\n ==>" input))
			(display (format "\033[1;34m ~s \033[0m" my-res))
			(cond ((equal? my-res output)
				(display (format "\033[1;32m Success! ☺ \033[0m \n")) #t)
				(else 
				(display (format "\033[1;31m Failed! ☹\033[0m , \n\nExpected:\n ~s, \n\nActual:\n ~s \n\n" output my-res)) #f))
			)))
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "==============================================")
	(newline)
	(let ((results (map (lambda (el) (assert (car el) (cadr el) (caddr el))) lst)))
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
		(display "\033[1;32m !!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n") #t)
		(else (display "\033[1;31m #####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n") #f))
		(newline))
))

(define parse-opt
    (lambda (x)
	(annotate-tc 
	  (pe->lex-pe
	    (box-set
	      (remove-applic-lambda-nil
		(eliminate-nested-defines
		  (parse x))))))))

;; (define create-sub-constants-tests
;;   (list
;;     ;; Format (list func-name list-of-args expected-result)
;;     (list create-sub-constants (list '(1 2 3)) '(1 2 3 () (3) (2 3) (1 2 3)))
;; ))

(define create-constants-table-tests
  (list
    ;; Format (list func-name list-of-args expected-result)
    
    ;; Empty exp
    (list create-constants-table (list (parse-opt ''())) 
      (list (list (void) 1) '(() 2) '(#t 3) '(#f 5)))  
      
    ;; Sybmol  
    (list create-constants-table (list (parse-opt '('a 'b "b" "a"))) 
      (list (list (void) 1) '(() 2) '(#t 3) '(#f 5) '("a" 7) '(a 10) '("b" 12) '(b 15)))   
      
    ;; String
    (list create-constants-table (list (parse-opt '("a" "b" "c"))) 
      (list (list (void) 1) '(() 2) '(#t 3) '(#f 5) '("a" 7) '("b" 10) '("c" 13))) 
      
    ;; Empty vec
    (list create-constants-table (list (parse-opt ''#()))
      (list (list (void) 1) '(() 2) '(#t 3) '(#f 5) '(#() 7)))
      
    ;; Empty String
    (list create-constants-table (list (parse-opt '(f ""))) 
      (list (list (void) 1) '(() 2) '(#t 3) '(#f 5) '("" 7)))      
    
    ;; Chars
    (list create-constants-table (list (parse-opt '(#\a #\b))) 
      (list (list (void) 1) '(() 2) '(#t 3) '(#f 5) '(#\a 7) '(#\b 9)))      
      
    (list create-constants-table (list (parse-opt '(f "abc"))) 
      (list (list (void) 1) '(() 2) '(#t 3) '(#f 5) '("abc" 7))) 
      
      
    (list create-constants-table (list (parse-opt '(f #t #f))) 
      (list (list (void) 1) '(() 2) '(#t 3) '(#f 5)))     
      
    (list create-constants-table (list '(const 1)) 
      (list (list (void) 1) '(() 2) '(#t 3) '(#f 5) '(1 7)))
    (list create-constants-table (list '((const 1) (const 2))) 
      (list (list (void) 1) '(() 2) '(#t 3) '(#f 5) '(1 7) '(2 9)))	
    (list create-constants-table (list '(const (1 2 3))) 
	  (list (list (void) 1) '(() 2) '(#t 3) '(#f 5) '(1 7) '(2 9) '(3 11) '((3) 13) '((2 3) 16) '((1 2 3) 19)))
    (list create-constants-table (list (parse-opt ''#(1 2 3))) 
      (list (list (void) 1) '(() 2) '(#t 3) '(#f 5) '(1 7) '(2 9) '(3 11) '(#(1 2 3) 13))) 	
      
    (list create-constants-table (list (parse-opt ''#(1 (1 2 3) #t #f))) 
      (list (list (void) 1) '(() 2) '(#t 3) '(#f 5) '(1 7) '(2 9) '(3 11) '((3) 13) 
			     '((2 3) 16) '((1 2 3) 19) '(#(1 (1 2 3) #t #f) 22))) 
			     
    (list create-constants-table (list (parse-opt ''#(1 (1 2 3) #t #f #(#\a 1 -1 "StR1")))) 
      (list (list (void) 1) '(() 2) '(#t 3) '(#f 5) '(1 7) '(2 9) '(3 11) '((3) 13) '((2 3) 16) '((1 2 3) 19) 
	'(#\a 22) '(-1 24) '("StR1" 26) '(#(#\a 1 -1 "StR1") 32) '(#(1 (1 2 3) #t #f #(#\a 1 -1 "StR1")) 38))) 			     
))

(define create-fvar-table-tests
  (list
     (list create-fvar-table (list (parse-opt '(x x y z)) 1) '((x 1) (y 2) (z 3)))
     (list create-fvar-table (list (parse-opt '(x x y z)) 100) '((x 100) (y 101) (z 102)))
     
     (list create-fvar-table (list (parse-opt '((lambda (x y z) (a b c)) 1 2 3)) 2) '((a 2) (b 3) (c 4)))
     
     (list create-fvar-table (list (parse-opt '((lambda (x y z) (lambda (d e f) (a b c))) 1 2 3)) 2) '((a 2) (b 3) (c 4)))
))

(define constant-symbols-tests
  (list
     (list constant-symbols (list '((-12/34 58) ("x" 15) (x 1) (1 5) (#\a 7))
     '((-12/34 58) ("x" 15) (x 1) (1 5) (#\a 7))) '(("x" 15)))
))


(display (format "\033[1mComp171 - Compiler Unit Tests\033[0m\n================================\n"))

(runAllTests
  (list  
      ;(cons "create-sub-constants" create-sub-constants-tests)           
      ;(cons "create-constants-table" create-constants-table-tests)    
      (cons "constant-symbols" constant-symbols-tests) 
      ;(cons "create-fvar-table" create-fvar-table-tests) 
))