;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Comp171 - Compiler - Tests

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "compiler.scm")	    

(define tests-counter 0)
(define failed-tests-counter 0)

;;;; Configuration
(define show-passed-tests #t)
(define show-summary #t)
(define catch-exceptions #f)

(define try-catch
  (lambda (try-thunk catch-thunk)
    (if catch-exceptions
      (guard (c (else (catch-thunk)))
      (try-thunk))
      (try-thunk))
))
		
(define tests-file->string
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
	(lambda ()
	  (let ((ch (read-char in-port)))
	    (if (eof-object? ch)
	      (begin
		(close-input-port in-port)
		'())
	      (cons ch (run)))))))
	(list->string (run))))
))

(define tests-string->file
  (lambda (str out-file)
    (letrec ((out-port (open-output-file out-file))
	  (run (lambda (lst)
		  (if (null? lst) #t
		      (begin (write-char (car lst) out-port)
			     (run (cdr lst)))))))
	(begin
	  (run (string->list str))
	  (close-output-port out-port)))
	    
))

 (define prepare-func
   (lambda (lst)
     (map (lambda (x) (annotate-tc 
 		  (pe->lex-pe
 		    (box-set
 		      (remove-applic-lambda-nil
 			(eliminate-nested-defines
 			  (parse x))))))) lst)
))

(define test-input
  (lambda (input)
    (system "rm -f outResult")
    (system "rm -f outFile")
    (system "rm -f outFile.c")
    (system "rm -f outFile.scm")
    (tests-string->file input "outFile.scm")
    (compile-scheme-file "outFile.scm" "outFile.c")
    (system "gcc -o outFile outFile.c")
    (system "./outFile > outResult")
    (let ((result (tests-file->string "outResult")))
      result)
))

(define test-func
  (lambda (input)
    (system "rm -f outResult")
    (system "rm -f outFile")
    (system "rm -f outFile.c")
    (system "rm -f outFile.scm")
    (tests-string->file input "outFile.scm")
    (compile-scheme-file "outFile.scm" "outFile.c")
    (system "gcc -o outFile outFile.c")
    (system "./outFile > outResult")
    (let ((result (tests-file->string "outResult")))
      (system "rm -f outResult")
      (system "rm -f outFile")
      (system "rm -f outFile.c")
      (system "rm -f outFile.scm")
      result)
))

(define expected-result
  (lambda (input)
    (let* ((exp-res (eval input))
	  (exp-res-str (tests-file->string (tests-string->file exp-res "expRes.scm"))))
      exp-res-str)
))
		
(define assert
	(lambda (input expected-output)
		(set! tests-counter (+ 1 tests-counter))
		(try-catch (lambda ()
		(let ((actual-output (test-func input)))			
			(cond ((equal? actual-output expected-output)
				(if show-passed-tests
				  (begin (display (format "~s) ~a\n" tests-counter input))
				  (display (format "\033[1;32m Success! ☺ \033[0m \n\n")))) 
				  #t)
				(else
				  (set! failed-tests-counter (+ 1 failed-tests-counter))
				  (display (format "~s) ~a\n" tests-counter input))
				  (display (format "\033[1;31mFailed! ☹\033[0m\n\n\033[1;34mExpected:\n ~s\033[0m\n\n\033[1;29mActual:\n ~a\033[0m\n\n" expected-output actual-output))
				#f))))
			(lambda () (set! failed-tests-counter (+ 1 failed-tests-counter))
				(display (format "~s) ~a\n" tests-counter input))
				(display 
				    (format "\n\033[1;31mEXCEPTION OCCURED. PLEASE CHECK MANUALLY THE INPUT:\n ~s\033[0m\n\n" input)) #f))
			))
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "==============================================")
	(newline)
	(let ((results (map (lambda (x) (assert (car x) (cdr x))) lst)))
	(newline)
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)	
		(display (format "\033[1;32m~s Tests: SUCCESS! ☺ \033[0m\n \n" tests-name)) #t)		
		(else
		(display (format "\033[1;31m~s Tests: FAILED! ☹ \033[0m\n \n" tests-name)) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
	(if show-summary
	  (begin
	    (display (format "Summary\n=============================\n\033[1;32mPassed: ~s of ~s tests ☺\033[0m\n" (- tests-counter failed-tests-counter) tests-counter))
	    (if (> failed-tests-counter 0)
	      (display (format "\033[1;31mFailed: ~s of ~s tests ☹\033[0m\n\n" failed-tests-counter tests-counter)))))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m!!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n") #t)
		(else (display "\033[1;31m#####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n") #f))
		(newline))
))

(define constants-table-tests
  (list
  
    ;;Numbers
    (cons "0" "0\n")
    (cons "1" "1\n")
    (cons "5" "5\n")
    (cons "-10" "-10\n")
    
    ;;Fractions
    (cons "2/4" "1/2\n")
    (cons "-3/5 " "-3/5\n")
    
    ;;Strings
    (cons "\"123abc\"" "\"123abc\"\n")
    (cons "\"AbCDeFGHiJKLMNOPQRSTUVWXYZ01234567891011121314151617181920\"" 
	  "\"AbCDeFGHiJKLMNOPQRSTUVWXYZ01234567891011121314151617181920\"\n")
    (cons "\"aA\"" "\"aA\"\n")
    (cons "\"\"" "\"\"\n")
    
    ;;Sybmols
    (cons "'a" "a\n")
    (cons "'AsAF" "asaf\n")
    (cons "'this-is-a-symbol12345" "this-is-a-symbol12345\n")
    
    ;;Boolean
    (cons "#t" "#t\n")
    (cons "#f" "#f\n")
    
    ;;Chars
    (cons "#\\a" "#\\a\n")
    (cons "#\\space" "#\\space\n")
    (cons "#\\newline" "#\\newline\n")

    ;;Lists
    (cons "'()" "()\n") 
    (cons "'(1 2 3)" "(1 . (2 . (3 . ())))\n")
    (cons "'(1 2 3 a)" "(1 . (2 . (3 . (a . ()))))\n")
    
    ;;Vectors
    (cons "'#()" "#0()\n") 
    (cons "'#(1 2 3)" "#3(1 2 3)\n")
    (cons "'#(1 2 3 a b c)" "#6(1 2 3 a b c)\n")
    (cons "'#(1 (1 2 3) #t #f)" "#4(1 (1 . (2 . (3 . ()))) #t #f)\n")
    (cons "'#((1 2) 3 4 #t #f -8/17 #(5 6))" "#7((1 . (2 . ())) 3 4 #t #f -8/17 #2(5 6))\n")
    (cons "'#(#(1))" "#1(#1(1))\n") 
    (cons "'#(#(5 6))" "#1(#2(5 6))\n")
    
    ;Quasi-Quote
    (cons "(define x 5) `(,x)" "(5 . ())\n")
    (cons "(define x 5) `(,@x)" "5\n")
    (cons "(quasiquote (1 2 (unquote (+ 3 4))))" "(1 . (2 . (7 . ())))\n")
))

(define or-if-begin-tests
(list
    (cons "(or)" "#f\n")
    (cons "(or #t)" "#t\n")
    (cons "(or #f)" "#f\n")
    (cons "(or (or #f #f) (or #t))" "#t\n")
    (cons "(or (or #f #f) (or 0))" "0\n")
    
    (cons "(or #f #f)" "#f\n")
    (cons "(or #f #t)" "#t\n")
    (cons "(if #t 1 0)" "1\n")
    
    ;;Nested If
    (cons "(if #t (if #t (if #t (if #t (if #t
	    (if #t (if #t (if #t (if #t 
	    (if #t (if #t (if #t (if #t (if #t #t #f) #f) #f) #f) #f) #f) #f) #f) #f) 
	    #f) #f ) #f) #f) #f)" "#t\n")
    
    
    (cons "(if #f 1 0)" "0\n")
    (cons "(or 25 #t #f 1 2 3 #f)" "25\n")
    (cons "(and 25 #t #f 1 2 3 #f)" "#f\n")
    
    (cons "(begin (or #t #t) (or #f))" "#f\n")
    (cons "(begin #\\a)" "#\\a\n")
    (cons "(begin (or #t #t) (or #f) (begin 1 2 3 45 \"Its a String\"))" "\"Its a String\"\n")
))

(define lambda-simple-tests
  (list
    (cons "((lambda () 1))" "1\n")
    (cons "((lambda () #t))" "#t\n")
    (cons "((lambda () -1/2))" "-1/2\n")
    (cons "((lambda () '(1 2 3 4 5 6)))" "(1 . (2 . (3 . (4 . (5 . (6 . ()))))))\n")
    (cons "((lambda () '#(1 2 3)))" "#3(1 2 3)\n")
    (cons "((lambda () \"abCdE123\"))" "\"abCdE123\"\n")
    (cons "((lambda (x) x) 5)" "5\n")
    (cons "((lambda (a) a) #t)" "#t\n")
    (cons "((lambda (x y) x) 5 6)" "5\n")
    (cons "((lambda (x y) y) 5 6)" "6\n")
    (cons "((lambda (x y) '(4 5)) 5 6)" "(4 . (5 . ()))\n")
    (cons "((lambda (x y z) z) 5 6 #t)" "#t\n")
    (cons "((lambda (x y z) z) 5 6 #f)" "#f\n")
    (cons "((lambda (x y z) x y z) 5 6 #f)" "#f\n")
    (cons "((lambda (x y z) (if x y z)) 5 6 #f)" "6\n")
    (cons "((lambda (x y z) (or x y z)) \"AbC1234567890\" 6 #f)" "\"AbC1234567890\"\n")
    (cons "((lambda (x y z) (if x y z)) #f -8/17 '#((1 2) 3 4 #(5 6)))" "#4((1 . (2 . ())) 3 4 #2(5 6))\n")
    
    ;;Nested Lambdas
    (cons "((lambda () ((lambda () 5))))" "5\n")
    (cons "((lambda (x) ((lambda () x))) -24)" "-24\n")
    (cons "((lambda (x y z) 
	      ((lambda () z))) 10 12 -24/36)" "-2/3\n")
    (cons "((lambda (x y z) 
	      ((lambda () x))) 10 12 -24/36)" "10\n")
    (cons "((lambda (x y z) 
	      ((lambda () y))) 10 '#(1 2) -24/36)" "#2(1 2)\n")
    (cons "((lambda () 
	      ((lambda (a) a) #t)))" "#t\n")		      
    (cons "((lambda (x y z) 
	      ((lambda (a) a) #t)) 10 '#(1 2) -24/36)" "#t\n")	
    (cons "((lambda (x y z) 
	      ((lambda (y) y) #f)) 10 '#(1 2) -24/36)" "#f\n")

    (cons "((lambda (x y z) ((lambda (a) x) 5)) 1 2 3)" "1\n")	      
    (cons "((lambda (x y z) ((lambda (a) y) 5)) 1 2 3)" "2\n")
    (cons "((lambda (x y z) ((lambda (a) z) 5)) 1 2 3)" "3\n")    
	      
    ;; Parameters and Bound Variables	      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      z) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
			  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "5\n")
    
    
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      y) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
			  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "4\n") 
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      x) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
			  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "3\n")       
	
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      w) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
			  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "2\n")  
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      v) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
			  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "1\n") 
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      u) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
			  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "0\n")      

    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      t) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
			  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "9\n")   
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      s) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
			  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "8\n")  
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      r) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "7\n")  

    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      q) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "6\n") 
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      p) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "#\\z\n")  
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      o) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "#1(1)\n")  
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      n) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "()\n")          
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      m) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "#f\n")   
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      l) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "#t\n")    
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      k) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "\"Matata\"\n") 
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      j) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "\"Akuna\"\n") 
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      i) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "\"BCD\"\n") 
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      h) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "\"A\"\n")   
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      g) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "#\\g\n")  
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      f) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "#\\f\n")   
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      e) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "#\\e\n") 
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      d) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "#\\d\n")       
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      c) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "#\\c\n")   
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      b) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "#\\b\n") 
      
    (cons
      "((lambda (a b c d e f g) 
	      ((lambda (h i j k)
		((lambda (l m n o p)
		  ((lambda (q r s t u)
		    ((lambda (v w x y z)
		      a) 1 2 3 4 5))
			6 7 8 9 0)) #t #f '() '#(1) #\\z))
		  \"A\" \"BCD\" \"Akuna\" \"Matata\")) #\\a #\\b #\\c #\\d #\\e #\\f #\\g)"
      "#\\a\n") 
      
      ;; Mayer Comp161 Lambda Simple Torture Test
      (cons "(((((lambda (a)
	(lambda (b)
	  (((lambda (a) (lambda (b) ((a b) (lambda (x) (lambda (y) y)))))
	    ((lambda (n)
	      ((n (lambda (x) (lambda (x) (lambda (y) y))))
		(lambda (x) (lambda (y) x))))
	    (((lambda (a)
		(lambda (b)
		  ((b (lambda (n)
			((lambda (p) (p (lambda (a) (lambda (b) b))))
			  ((n (lambda (p)
				(((lambda (a)
				    (lambda (b) (lambda (c) ((c a) b))))
				  ((lambda (n)
				    (lambda (s)
				      (lambda (z) (s ((n s) z)))))
				  ((lambda (p)
				      (p (lambda (a) (lambda (b) a))))
				    p)))
				((lambda (p)
				    (p (lambda (a) (lambda (b) a))))
				  p))))
			  (((lambda (a)
			      (lambda (b) (lambda (c) ((c a) b))))
			    (lambda (x) (lambda (y) y)))
			    (lambda (x) (lambda (y) y)))))))
		    a)))
	      a)
	      b)))
	  ((lambda (n)
	      ((n (lambda (x) (lambda (x) (lambda (y) y))))
	      (lambda (x) (lambda (y) x))))
	    (((lambda (a)
		(lambda (b)
		  ((b (lambda (n)
			((lambda (p) (p (lambda (a) (lambda (b) b))))
			((n (lambda (p)
			      (((lambda (a)
				  (lambda (b) (lambda (c) ((c a) b))))
				((lambda (n)
				    (lambda (s)
				      (lambda (z) (s ((n s) z)))))
				  ((lambda (p)
				    (p (lambda (a) (lambda (b) a))))
				  p)))
				((lambda (p)
				  (p (lambda (a) (lambda (b) a))))
				p))))
			  (((lambda (a)
			      (lambda (b) (lambda (c) ((c a) b))))
			    (lambda (x) (lambda (y) y)))
			  (lambda (x) (lambda (y) y)))))))
		  a)))
	      b)
	    a)))))
      ((lambda (n)
	((lambda (p) (p (lambda (a) (lambda (b) b))))
	  ((n (lambda (p)
		(((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
		  ((lambda (n) (lambda (s) (lambda (z) (s ((n s) z)))))
		  ((lambda (p) (p (lambda (a) (lambda (b) a)))) p)))
		(((lambda (a)
		    (lambda (b)
		      ((b (a (lambda (a)
				(lambda (b)
				  ((a (lambda (n)
					(lambda (s)
					  (lambda (z) (s ((n s) z))))))
				  b)))))
			(lambda (x) (lambda (y) y)))))
		  ((lambda (p) (p (lambda (a) (lambda (b) a)))) p))
		  ((lambda (p) (p (lambda (a) (lambda (b) b)))) p)))))
	  (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
	    (lambda (x) x))
	    (lambda (x) x)))))
      (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))
    (((lambda (a)
	(lambda (b)
	  ((b (a (lambda (a)
		    (lambda (b)
		      ((a (lambda (n)
			    (lambda (s) (lambda (z) (s ((n s) z))))))
		      b)))))
	    (lambda (x) (lambda (y) y)))))
      (((lambda (a)
	  (lambda (b)
	    ((b (a (lambda (a)
		      (lambda (b)
			((a (lambda (n)
			      (lambda (s) (lambda (z) (s ((n s) z))))))
			b)))))
	      (lambda (x) (lambda (y) y)))))
	((lambda (x) (lambda (y) (x (x (x y)))))
	  (lambda (x) (lambda (y) (x (x y))))))
	(lambda (x) (lambda (y) (x (x (x y)))))))
      (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))
    #t)
  #f)" "#t\n")
))

(define lambda-opt-tests
  (list
    (cons "((lambda (x . y) y) 1 2)" "(2 . ())\n")
    (cons "((lambda (x . y) y) 1 2 3 4 5 6)" "(2 . (3 . (4 . (5 . (6 . ())))))\n")
    (cons "((lambda (x y . z) x) 1 2)" "1\n")
    (cons "((lambda (x y . z) y) 1 2)" "2\n")
    (cons "((lambda (x y . z) z) 1 2)" "()\n")
    (cons "((lambda (x y . z) z) 1 2 3)" "(3 . ())\n")
    (cons "((lambda (x y . z) z) 1 2 3 4 5 6 7 #t)" "(3 . (4 . (5 . (6 . (7 . (#t . ()))))))\n")
    (cons "((lambda (a b . c) c) 1 2 -3/4 5/6)" "(-3/4 . (5/6 . ()))\n")
    (cons "((lambda (x) ((lambda (a b . c) c) 1 2 -3/4 5/6)) 12)" "(-3/4 . (5/6 . ()))\n")
    (cons "((lambda (x y . z) ((lambda (a b . c) z) 1 2 -3/4 5/6)) 12 13 14)" "(14 . ())\n")
    (cons "((lambda (x y . z) 
	      ((lambda (a b . c) z) 1 2 -3/4 5/6)) 12 13)" "()\n")
    (cons "((lambda (x y . z) 
	      ((lambda (a b . c) ((lambda (d e f . g) g) 1 2 3)) 1 2 -3/4 5/6)) 12 13)" "()\n")	 
    (cons "((lambda (x y . z) 
	      ((lambda (a b . c) ((lambda (d e f . g) g) 1 2 3 4 5 6 7 8 9 0)) 1 2 -3/4 5/6)) 12 13)" 
	  "(4 . (5 . (6 . (7 . (8 . (9 . (0 . ())))))))\n")		      		      

))

(define lambda-var-tests
  (list
    (cons "((lambda x x) 1 #t #f 2 3)" "(1 . (#t . (#f . (2 . (3 . ())))))\n")
    (cons "((lambda x x) 1)" "(1 . ())\n")
    (cons "((lambda x x))" "()\n")
    (cons "((lambda x ((lambda y x))))" "()\n")
    (cons "((lambda x ((lambda y y))))" "()\n")
    (cons "((lambda x ((lambda y y))) 1 2 3 4 5 #\\a 7 8 9 0 100)" "()\n")
    (cons "((lambda x ((lambda y x))) 1 2 3 4 5 '#(1 2 3))" 
	  "(1 . (2 . (3 . (4 . (5 . (#3(1 2 3) . ()))))))\n")
    (cons "((lambda x ((lambda y y) #t #f \"AbC123\")) 1 2 3 4 5 '#(1 2 3))" 
	  "(#t . (#f . (\"AbC123\" . ())))\n")

    ;; Lambda-var in Lambda-simple
    (cons "((lambda (a) ((lambda x x))) -12/36)" "()\n")
    (cons "((lambda (a) ((lambda x x) \"Akuna\")) -12/36)" "(\"Akuna\" . ())\n")
    (cons "((lambda (a) ((lambda x x) \"Akuna\" \"Matata\")) -12/36)" 
	  "(\"Akuna\" . (\"Matata\" . ()))\n")
    (cons "((lambda (a) ((lambda x a) \"Akuna\" \"Matata\")) -12/36)" 
	  "-1/3\n")
    (cons "(((lambda (z) (lambda x x)) 2) 1)" 
      "(1 . ())\n")
	  
    ;; Lambda-var in Lambda-opt
    (cons "((lambda (a . b) ((lambda x x))) -12/36)" "()\n")
    (cons "((lambda (a . b) ((lambda x x) \"Akuna\")) -12/36 98)" "(\"Akuna\" . ())\n")
    (cons "((lambda (a b . c) ((lambda x x) \"Akuna\" \"Matata\")) -12/36 #t)" 
	  "(\"Akuna\" . (\"Matata\" . ()))\n")
    (cons "((lambda (a b c . d) ((lambda x a) \"Akuna\" \"Matata\")) -12/36 #\\a '#())" 
	  "-1/3\n")
    (cons "((lambda (a b c . d) ((lambda x d) \"Akuna\" \"Matata\")) -12/36 #\\a '#())" 
	  "()\n")
	  
    (cons "((lambda x x) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
			  '#(1 2 3 4 5 6 7 8 9 #t #f)
			  '(1 2 3 4 5 6 7 8 9 -12/35))" 
			  "(1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . (9 . (10 . (11 . (12 . (13 . (14 . (15 . (16 . (17 . (18 . (19 . (20 . (#11(1 2 3 4 5 6 7 8 9 #t #f) . ((1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . (9 . (-12/35 . ())))))))))) . ()))))))))))))))))))))))\n")

    (cons "(let ()
  ((lambda s
     (let ()
       ((lambda s s) s s s)))
   #t))" "((#t . ()) . ((#t . ()) . ((#t . ()) . ())))\n")

))

(define comp161-torture-test-for-compiler-unsorted
  (list
    (cons "(let ((x #f))
	    (let () x))" "#f\n")
	    
    (cons "(let ((x #f) (y #t))
  (let ((x #f))
    (let ((x #f) (z #f) (t #f))
      (let ((x #f) (t #f))
	y))))" "#t\n")

  (cons "((((lambda (x)
     (lambda (y)
       y))
   (lambda (p)
     (p (lambda (x y)
	  (lambda (p)
	    (p y x))))))
  (lambda (z) (z #t #f)))
 (lambda (x y) x))" "#t\n")
 
 (cons "((((lambda (x)
     (lambda (y)
       (x y)))
   (lambda (p)
     (p (lambda (x y)
	  (lambda (p)
	    (p y x))))))
  (lambda (z) (z #t #f)))
 (lambda (x y) x))" "#f\n")
 
 (cons "((((lambda (x)
     (lambda (y)
       (x (x y))))
   (lambda (p)
     (p (lambda (x y)
	  (lambda (p)
	    (p y x))))))
  (lambda (z) (z #t #f)))
 (lambda (x y) x))" "#t\n")
 
 (cons "(((((lambda (x) ((x x) (x x)))
    (lambda (x)
      (lambda (y)
	(x (x y)))))
   (lambda (p)
     (p (lambda (x y)
	  (lambda (p)
	    (p y x))))))
  (lambda (z) (z #t #f)))
 (lambda (x y) x))" "#t\n")
 
))

(define tc-applic-tests
  (list
    (cons "((lambda (x) ((lambda (y) y) 5)) 6)" "5\n")
    (cons "((lambda (x) ((lambda (y) ((lambda (z) z) 1)) 5)) 6)" "1\n")
    (cons "((lambda (x) ((lambda (y) ((lambda (z) y) 1)) 5)) 6)" "5\n")
    (cons "((lambda (x) ((lambda (y) ((lambda (z) x) 1)) 5)) 6)" "6\n")
    (cons "((lambda (x) 
	    (if x ((lambda (z) z) 1) ((lambda (y . z) z) 1 2))) #f)" 
	  "(2 . ())\n")
    (cons "((lambda (x) 
	(or x ((lambda (z) z) 1) ((lambda (y . z) z) 1 2))) #f)" 
      "1\n")
    (cons "((lambda (x) 
	(begin x ((lambda (z) z) 1) ((lambda (y . z) z) 1 2))) #f)" 
      "(2 . ())\n")
    (cons "((lambda (x) 
	((lambda (z) z) 1) ((lambda (y . z) ((lambda x x))) 1 2)) #f)" 
      "()\n")
))

(define set-tests
  (list
    ;; set pvar
    (cons "((lambda (x) (set! x 1)) 4)" "")
    (cons "((lambda (x) (set! x 12) x) 5)" "12\n")
    (cons "((lambda (x y . z) (set! z #t) z) 5 9 34)" "#t\n")
    (cons "((lambda (x y . z) (set! y '(1 2 3 4 5)) y) 5 9)" 
	  "(1 . (2 . (3 . (4 . (5 . ())))))\n")
    (cons "((lambda x (set! x '#(1 2 3)) x) 5 6 7)" "#3(1 2 3)\n")
    (cons "((lambda (x . y) ((lambda (x) y x (set! x #f) x) 58)) 34)" "#f\n")
    
    ;; set bvar
    (cons "((lambda (x) ((lambda (y) (set! x 12)) 3)) 5)" "")
    (cons "((lambda (x) ((lambda (y) (set! x 12) x) 3)) 5)" "12\n")
    (cons "((lambda (x) ((lambda (y) ((lambda (z) (set! x '(1 2 3)) x) 3)) 85)) 5)"
	  "(1 . (2 . (3 . ())))\n")
	  
    ;;set fvar
    (cons "(set! x 5) x" "5\n")
    (cons "(begin (set! x 5) (set! y x) y)" "5\n")
    (cons "(begin (set! x 5) (set! y -12/35) y)" "-12/35\n")
    (cons "(begin (set! x 5) (set! x -12/35) x)" "-12/35\n")
    
    ;;;; box-set box-get box test
    (cons "((lambda (x) ((lambda (y) (set! x 12) 2) 3) x) 5)" "12\n")
))

(define pvar-bvar-tests
  (list
    ;; pvar
    (cons "((lambda (x) x) 5)" "5\n")
    (cons "((lambda (x y z) y) 5 6 7)" "6\n")
    (cons "((lambda (x y z) z) 5 6 7)" "7\n")
    (cons "((lambda x x) 5 6 7)" "(5 . (6 . (7 . ())))\n")
    (cons "((lambda (x . y) y) 5 6 7)" "(6 . (7 . ()))\n")
    (cons "((lambda (x . y) x) 5 6 7)" "5\n")

    ;; bvar
    (cons "((lambda (x) ((lambda (y) x) 12)) 5)" "5\n")
    (cons "((lambda (x y z) ((lambda (z) y) 12)) 5 6 7)" "6\n")
    (cons "((lambda (x y z) ((lambda (y) z) 12)) 5 6 7)" "7\n")
    (cons "((lambda x ((lambda (y) x) 12)) 5 6 7)" "(5 . (6 . (7 . ())))\n")
    (cons "((lambda (x . y) ((lambda (z . a) y) 12 13)) 5 6 7)" "(6 . (7 . ()))\n")
    (cons "((lambda (x . y) ((lambda y x) 12)) 5 6 7)" "5\n")    
))

(define define-tests
  (list
    (cons "(define x 5) x" "5\n")
    (cons "(define x 5) (define b 6) x" "5\n")
    (cons "(define x 5) (define b 6) b" "6\n")
    (cons "(define x (lambda (x y) y)) (x 1 2)" "2\n")
    (cons "(define x (lambda (x y) x)) (x 1 2)" "1\n")
    (cons "(define not (lambda (x) (if x #f #t))) (not 1)" "#f\n")
    (cons "(define not (lambda (x) (if x #f #t))) (not #f)" "#t\n")
))

(define primitive-functions-tests
  (list
    ; car, cdr and combinations
    (cons "(car '(a b))" "a\n")
    (cons "(car '(a))" "a\n")
    (cons "(car '(a b c))" "a\n")
    (cons "(car (cons 3 4))" "3\n")
    
    (cons "(cdr '(a b))" "(b . ())\n")
    (cons "(cdr '(a))" "()\n")
    (cons "(cdr '(a b c))" "(b . (c . ()))\n")
    (cons "(cdr (cons 3 4))" "4\n")
    ;(cons "(caaaar '((((a))) b))" "a\n")
    ;(cons "(cdadr '((((a))) (((b) c)) (((c))) (((d)))))" "()\n")
    ;(cons "(cdaadr '((((a))) (((b e) c)) (((c))) (((d)))))" "(c . ())\n")
    
    ;list
    (cons "(list)" "()\n")
    (cons "(list 1 2 3 #t 'a)" "(1 . (2 . (3 . (#t . (a . ())))))\n")
    (cons "(list (if #f #f))" "(#<void> . ())\n")
    (cons "(list 'a 1 'b (if #f #f))" "(a . (1 . (b . (#<void> . ()))))\n")
    (cons "(list 1 2 3)" "(1 . (2 . (3 . ())))\n")
    (cons "(list 3 2 1)" "(3 . (2 . (1 . ())))\n")
    
    ;not
    (cons "(not 1)" "#f\n")
    (cons "(not #f)" "#t\n")
    (cons "(not #t)" "#f\n")
    (cons "(not '())" "#f\n")
    (cons "(not (< 4 5))" "#f\n")    
    
    ;apply
    (cons "(apply car '((a)))" "a\n")
    (cons "(apply (lambda (x y z) (list x y z)) '(1 2 3))" "(1 . (2 . (3 . ())))\n")
    (cons "((lambda x ((lambda (y) (apply car x)) 5)) '(1 2 3 4))" "1\n")
    (cons "(apply + '(4 5))" "9\n") 
    (cons "(define min 
		(lambda lst
		  (if (null? (cdr lst)) (car lst)
		    (if (< (car lst) (apply min (cdr lst)))
			  (car lst)
			  (apply min (cdr lst))))))
	    (apply min '(6 8 3 2 5))" "2\n") 

    (cons "(apply vector '(a b c d e))" "#5(a b c d e)\n")

    (cons "(define first
      (lambda (l)
	(apply (lambda (x . y) x)
		l)))
    (define rest
      (lambda (l)
	(apply (lambda (x . y) y) l)))
    (first '(a b c d))" "a\n")

    (cons "(define first
      (lambda (l)
	(apply (lambda (x . y) x)
		l)))
    (define rest
      (lambda (l)
	(apply (lambda (x . y) y) l)))
    (rest '(a b c d))" "(b . (c . (d . ())))\n")    
    
    ;cons
    (cons "(cons 1 2)" "(1 . 2)\n")
    (cons "(cons 1 (cons 2 (cons 3 '())))" "(1 . (2 . (3 . ())))\n")
    (cons "(cons 'a '())" "(a . ())\n")
    (cons "(cons 'a '(b c))" "(a . (b . (c . ())))\n")
    (cons "(cons 3 4)" "(3 . 4)\n")    
    
    ;null?
    (cons "(null? '())" "#t\n")
    (cons "(null? (list))" "#t\n")
    (cons "(null? (list 1))" "#f\n")
    (cons "(null? '(1 2 3 a))" "#f\n")
    (cons "(null? '(a))" "#f\n")
    (cons "(null? (cdr '(a)))" "#t\n")
    (cons "(null? 3)" "#f\n")
    (cons "(null? #f)" "#f\n")    

    ;map
    (cons "(map (lambda (x) x) '(1 2 3))" "(1 . (2 . (3 . ())))\n")
    (cons "(map car '((1) (2) (3)))" "(1 . (2 . (3 . ())))\n")
    ;(cons "(map caar '(((1)) ((2)) ((3))))" "(1 . (2 . (3 . ())))\n")
    (cons "(map cdr (list))" "()\n")
    (cons "(define abs (lambda (x) (if (< x 0) (- x) x)))
	  (map abs '(1 -2 3 -4 5 -6))" "(1 . (2 . (3 . (4 . (5 . (6 . ()))))))\n")    
    ;(cons "(map (lambda (x y) (cons x y)) '(1 2) '(3 4))" "((1 . 3) . ((2 . 4) . ()))\n")
    ;(cons "(map list '(1 2) '(3 4) '(5 6) '(7 8) '(9 10))" "((1 . (3 . (5 . (7 . (9 . ()))))) . ((2 . (4 . (6 . (8 . (10 . ()))))) . ()))\n")

    ;append
    (cons "(append '(a b c) '())" "(a . (b . (c . ())))\n")
    (cons "(append '() '(a b c))" "(a . (b . (c . ())))\n")
    (cons "(append '(a b) '(c d))" "(a . (b . (c . (d . ()))))\n")
    (cons "(append '(a b) 'c)"  "(a . (b . c))\n")
    (cons "(let ((x (list 'b)))
	    (eq? x (cdr (append '(a) x))))" "#t\n")
    (cons "(append #t)" "#t\n")    
    (cons "(append '(1 2) '(3 4 5))" "(1 . (2 . (3 . (4 . (5 . ())))))\n")
    (cons "(append '(1 2) '())" "(1 . (2 . ()))\n")
    (cons "(append '(1) '(2))" "(1 . (2 . ()))\n")
    (cons "(append '() '(1 2))" "(1 . (2 . ()))\n")
    (cons "(append)" "()\n")
    (cons "(append '(1 2 3) '(4) '(5 6) '(#t a b c) '())" 
	  "(1 . (2 . (3 . (4 . (5 . (6 . (#t . (a . (b . (c . ()))))))))))\n")
    (cons
      "(define x '(1 2))
       (define y '(3 4))
       (define z '(5 6))
       (define f (append x y z))
       f" "(1 . (2 . (3 . (4 . (5 . (6 . ()))))))\n")
    (cons
      "(define x '(1 2))
       (define y '(3 4))
       (define z '(5 6))
       (define f (append x y z))
       (set-car! x 'a)
       f" "(1 . (2 . (3 . (4 . (5 . (6 . ()))))))\n") 
    (cons
      "(define x '(1 2))
       (define y '(3 4))
       (define z '(5 6))
       (define f (append x y z))
       (set-car! x 'a)
       (set-car! y 'b)
       f" "(1 . (2 . (3 . (4 . (5 . (6 . ()))))))\n") 
    (cons
      "(define x '(1 2))
       (define y '(3 4))
       (define z '(5 6))
       (define f (append x y z))
       (set-car! x 'a)
       (set-car! y 'b)
       (set-car! z 'c)
       f" "(1 . (2 . (3 . (4 . (c . (6 . ()))))))\n")     
    (cons "(append '(a b) 'c)" "(a . (b . c))\n")
    (cons "(append '(1 2) '(3 4))" "(1 . (2 . (3 . (4 . ()))))\n")
    (cons "(append '(1 2) 3)" "(1 . (2 . 3))\n")
    (cons "(append '(a b) '(#t #f) 'e)" "(a . (b . (#t . (#f . e))))\n")
    (cons "(define x '(5)) 
	   (define y (append '(a b) '(#t #f) '(1 2 3 4) '(5 6 7) '(8 9 -1/2) x))
	   y
	   (set-car! x #t)
	   y" 
	 "(a . (b . (#t . (#f . (1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . (9 . (-1/2 . (5 . ())))))))))))))))
(a . (b . (#t . (#f . (1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . (9 . (-1/2 . (#t . ())))))))))))))))\n")    
	
    ;boolean?
    (cons "(boolean? #t)" "#t\n")
    (cons "(boolean? #f)" "#t\n")
    (cons "(boolean? #\\a)" "#f\n")
    (cons "(boolean? 1)" "#f\n")
    (cons "(boolean? \"AbC\")" "#f\n")
    (cons "(boolean? '(#t))" "#f\n")
    (cons "(boolean? 't)" "#f\n")
    (cons "(boolean? '())" "#f\n")    
    
    ;char?
    (cons "(char? #t)" "#f\n")
    (cons "(char? #f)" "#f\n")
    (cons "(char? #\\a)" "#t\n")
    (cons "(char? #\\B)" "#t\n")
    (cons "(char? #\\space)" "#t\n")
    (cons "(char? 1)" "#f\n")
    (cons "(char? \"AbC\")" "#f\n")
    (cons "(char? '(#t))" "#f\n")
    (cons "(char? 'a)" "#f\n")
    (cons "(char? 97)" "#f\n")
    (cons "(char? \"a\")" "#f\n")
    (cons "(char? (string-ref (make-string 1) 0))" "#t\n")    
    
    ;integer?
    (cons "(integer? #t)" "#f\n")
    (cons "(integer? #f)" "#f\n")
    (cons "(integer? #\\a)" "#f\n")
    (cons "(integer? 1)" "#t\n")
    (cons "(integer? -52)" "#t\n")
    (cons "(integer? -48/36)" "#f\n")
    (cons "(integer? 1/12)" "#f\n")
    (cons "(integer? '(1/12))" "#f\n")
    (cons "(integer? \"AbC\")" "#f\n")
    (cons "(integer? '(#t))" "#f\n")
    (cons "(integer? 1901)" "#t\n")
    (cons "(integer? -2/3)" "#f\n")
    (cons "(integer? 'a)" "#f\n")    
    
    ;pair?
    (cons "(pair? 1)" "#f\n")
    (cons "(pair? (cons 1 2))" "#t\n")
    (cons "(pair? '(1 2 3))" "#t\n")
    (cons "(pair? (list 1 2))" "#t\n")
    (cons "(pair? #t)" "#f\n")
    (cons "(pair? '(a b c))" "#t\n")
    (cons "(pair? '(3 . 4))" "#t\n")
    (cons "(pair? '())" "#f\n")
    (cons "(pair? '#(a b))" "#f\n")
    (cons "(pair? 3)" "#f\n")    
    
    ;number?
    (cons "(number? 1)" "#t\n")
    (cons "(number? -58)" "#t\n")
    (cons "(number? 2/12)" "#t\n")
    (cons "(number? -3/14)" "#t\n")
    (cons "(number? 'a)" "#f\n")
    (cons "(number? #t)" "#f\n")
    (cons "(number? '(1))" "#f\n")
    (cons "(number? '#(1))" "#f\n")
    (cons "(number? 1901)" "#t\n")
    (cons "(number? -2/3)" "#t\n")
    (cons "(number? #\\a)" "#f\n")    

    ;rational?
    (cons "(rational? 1)" "#t\n")
    (cons "(rational? -58)" "#t\n")
    (cons "(rational? 2/12)" "#t\n")
    (cons "(rational? -3/14)" "#t\n")
    (cons "(rational? 'a)" "#f\n")
    (cons "(rational? #t)" "#f\n")
    (cons "(rational? '(1))" "#f\n")
    (cons "(rational? '#(1))" "#f\n")
    (cons "(rational? 1901)" "#t\n")
    (cons "(rational? -2/3)" "#t\n")
    (cons "(rational? '(a b c))" "#f\n")    
    
    ;char->integer
    (cons "(char->integer #\\a)" "97\n")
    (cons "(char->integer #\\A)" "65\n")
    (cons "(char->integer #\\space)" "32\n")
    (cons "(char->integer #\\h)" "104\n")
    (cons "(char->integer #\\newline)" "10\n")    
    
    ;integer->char
    (cons "(integer->char 97)" "#\\a\n")
    (cons "(integer->char 65)" "#\\A\n")
    (cons "(integer->char 32)" "#\\space\n")
    (cons "(integer->char 2)" "#\\002\n")
    (cons "(integer->char 48)" "#\\0\n")
    (cons "(integer->char 101)" "#\\e\n")    
    
    ;string-length
    (cons "(string-length \"abcde123\")" "8\n")
    (cons "(string-length \"this-is a STRING !\")" "18\n")
    (cons "(string-length \"\")" "0\n")
    (cons "(string-length \"abc\")" "3\n")
    (cons "(string-length \"hi there\")" "8\n")
    (cons "(string-length (make-string 1000000))" "1000000\n")    
    
    ;denominator
    (cons "(denominator 1)" "1\n")
    (cons "(denominator -54)" "1\n")
    (cons "(denominator 1/2)" "2\n")
    (cons "(denominator -1/45)" "45\n")
    (cons "(denominator -12/2)" "1\n")
    (cons "(denominator -15/10)" "2\n")
    (cons "(denominator 9)" "1\n")
    (cons "(denominator 2/3)" "3\n")
    (cons "(denominator -9/4)" "4\n")    
    
    ;numerator
    (cons "(numerator 1)" "1\n")
    (cons "(numerator -54)" "-54\n")
    (cons "(numerator 1/2)" "1\n")
    (cons "(numerator -1/45)" "-1\n")
    (cons "(numerator -12/2)" "-6\n")
    (cons "(numerator -15/10)" "-3\n")
    (cons "(numerator 9)" "9\n")
    (cons "(numerator 2/3)" "2\n")
    (cons "(numerator -9/4)" "-9\n")    
    
    ;make-string
    (cons "(make-string 1)" "\"\\000\"\n")
    (cons "(make-string 3)" "\"\\000\\000\\000\"\n")    
    (cons "(make-string 1 #\\a)" "\"a\"\n")
    (cons "(make-string 5 #\\A)" "\"AAAAA\"\n")
    (cons "(make-string 0 #\\space)" "\"\"\n")
    (cons "(make-string 0)" "\"\"\n")
    (cons "(make-string 0 #\\x)" "\"\"\n")
    (cons "(make-string 5 #\\x)" "\"xxxxx\"\n")    
    
    ;make-vector
    (cons "(make-vector 1 #\\a)" "#1(#\\a)\n")
    (cons "(make-vector 5 #\\A)" "#5(#\\A #\\A #\\A #\\A #\\A)\n")
    (cons "(make-vector 0 #\\space)" "#0()\n") 
    (cons "(make-vector 0 #\\Z)" "#0()\n") 
    (cons "(make-vector 12 #t)" "#12(#t #t #t #t #t #t #t #t #t #t #t #t)\n")  
    (cons "(make-vector 4 -6/8)" "#4(-3/4 -3/4 -3/4 -3/4)\n")  
    (cons "(make-vector 1)" "#1(0)\n")
    (cons "(make-vector 3)" "#3(0 0 0)\n")
    (cons "(make-vector 0)" "#0()\n")
    (cons "(make-vector 0 'a)" "#0()\n")
    (cons "(make-vector 5 'a)" "#5(a a a a a)\n")
    
    ;procedure?
    (cons "(procedure? 1)" "#f\n")
    (cons "(procedure? 1)" "#f\n")
    (cons "(procedure? (lambda (x) x))" "#t\n")
    (cons "(define f (lambda x x)) (procedure? f)" "#t\n")
    (cons "(define f (lambda (a b . c) c)) (procedure? f)" "#t\n")
    (cons "(procedure? car)" "#t\n")
    (cons "(procedure? 'car)" "#f\n")
    (cons "(procedure? (lambda (x) x))" "#t\n")
    (cons "(procedure? '(lambda (x) x))" "#f\n")
    
    ;vector-length
    (cons "(vector-length '#())" "0\n")
    (cons "(vector-length '#(1))" "1\n")
    (cons "(vector-length '#(1 2 3))" "3\n")
    (cons "(vector-length '#(1 2 3 #t #f 6/8))" "6\n")
    (cons "(vector-length '#(1 2 3 #t #f 6/8 (#\\a #\\b #\\C \"StR\")))" "7\n")
    (cons "(vector-length '#(a b c))" "3\n")
    (cons "(vector-length (vector 1 2 3 4))" "4\n")
    (cons "(vector-length (make-vector 300))" "300\n")    
    
    ;string?
    (cons "(string? 1)" "#f\n")
    (cons "(string? \"\")" "#t\n")
    (cons "(string? #\\a)" "#f\n")
    (cons "(string? \"this is A STRING!\")" "#t\n")
    (cons "(string? (make-string 20 #\\Y))" "#t\n")
    (cons "(string? \"hi\")" "#t\n")
    (cons "(string? 'hi)" "#f\n")
    (cons "(string? #\\h)" "#f\n")    
    
    ;symbol?
    (cons "(symbol? 'a)" "#t\n")
    (cons "(symbol? 'this-is-a-symbol)" "#t\n")
    (cons "(symbol? #t)" "#f\n")
    (cons "(symbol? \"Str\")" "#f\n")
    (cons "(symbol? 1)" "#f\n")
    (cons "(symbol? #\\c)" "#f\n")
    (cons "(symbol? 't)" "#t\n")
    (cons "(symbol? \"t\")" "#f\n")
    (cons "(symbol? '(t))" "#f\n")
    (cons "(symbol? #\\t)" "#f\n")
    (cons "(symbol? 3)" "#f\n")
    (cons "(symbol? #t)" "#f\n")    
    
    ;vector?
    (cons "(vector? '#())" "#t\n")
    (cons "(vector? '(1))" "#f\n")
    (cons "(vector? #t)" "#f\n")
    (cons "(vector? 'symbol1)" "#f\n")
    (cons "(vector? (make-vector 1 2))" "#t\n")
    (cons "(vector? (make-vector 100 #t))" "#t\n")
    (cons "(vector? '#(1))" "#t\n")
    (cons "(vector? '#(1 2 3))" "#t\n")
    (cons "(vector? '#(1 2 3 #t #f 6/8))" "#t\n")
    (cons "(vector? '#(1 2 3 #t #f 6/8 (#\\a #\\b #\\C \"StR\")))" "#t\n")
    (cons "(vector? '#(a b c))" "#t\n")
    (cons "(vector? (vector 'a 'b 'c))" "#t\n")
    (cons "(vector? '())" "#f\n")
    (cons "(vector? '(a b c))" "#f\n")
    (cons "(vector? \"abc\")" "#f\n")    
    
    ;zero?
    (cons "(zero? 1)" "#f\n")
    (cons "(zero? 0)" "#t\n")
    (cons "(zero? 0/5)" "#t\n")
    (cons "(zero? -0/12)" "#t\n")
    (cons "(zero? 2/13)" "#f\n")
    (cons "(zero? -0)" "#t\n")
    (cons "(zero? (- 3 3))" "#t\n")
    (cons "(zero? (+ 1/2 1/2))" "#f\n")    
    
    ;vector
    (cons "(vector)" "#0()\n")
    (cons "(vector 1)" "#1(1)\n")
    (cons "(vector #\\a #\\A #\\space)" "#3(#\\a #\\A #\\space)\n")
    (cons "(vector 1 2 3 #t #f -1/2 \"a\" #\\b)" "#8(1 2 3 #t #f -1/2 \"a\" #\\b)\n")
    (cons "(vector 'a 'b 'c)" "#3(a b c)\n")
  
    ;string-ref
    (cons "(string-ref \"ABcD1234\" 0)" "#\\A\n")
    (cons "(string-ref \"ABcD1234\" 1)" "#\\B\n")
    (cons "(string-ref \"ABcD1234\" 2)" "#\\c\n")
    (cons "(string-ref \"ABcD1234\" 3)" "#\\D\n")
    (cons "(string-ref \"ABcD1234\" 4)" "#\\1\n")
    (cons "(string-ref \"ABcD1234\" 5)" "#\\2\n")
    (cons "(string-ref \"ABcD1234\" 6)" "#\\3\n")
    (cons "(string-ref \"ABcD1234\" 7)" "#\\4\n") 
    (cons "(string-ref \"hi there\" 0)" "#\\h\n")
    (cons "(string-ref \"hi there\" 5)" "#\\e\n")    
    
    ;vector-ref
    (cons "(vector-ref '#(\"ABC\" 1 #t 2 #f 3 (a)) 0)" "\"ABC\"\n")
    (cons "(vector-ref '#(\"ABC\" 1 #t 2 #f 3 (a)) 1)" "1\n")
    (cons "(vector-ref '#(\"ABC\" 1 #t 2 #f 3 (a)) 2)" "#t\n")
    (cons "(vector-ref '#(\"ABC\" 1 #t 2 #f 3 (a)) 3)" "2\n")
    (cons "(vector-ref '#(\"ABC\" 1 #t 2 #f 3 (a)) 4)" "#f\n")
    (cons "(vector-ref '#(\"ABC\" 1 #t 2 #f 3 (a)) 5)" "3\n")
    (cons "(vector-ref '#(\"ABC\" 1 #t 2 #f 3 (a)) 6)" "(a . ())\n")
    (cons "(vector-ref '#(a b c) 0)" "a\n")
    (cons "(vector-ref '#(a b c) 1)" "b\n")
    (cons "(vector-ref '#(x y z w) 3)" "w\n")
    
    ;+
    (cons "(+ 3/2 3/2)" "3\n")
    (cons "(+ -3/2 -3/2)" "-3\n")
    (cons "(+ 3/2 -3/2)" "0\n")
    (cons "(+ 1/2 2)" "5/2\n")
    (cons "(+ 1 -1/2)" "1/2\n")
    (cons "(+ 1 -1/2 -1/2)" "0\n")
    (cons "(+ 11 12)" "23\n")
    (cons "(+ 11 -12)" "-1\n")
    (cons "(+ 10/10 -120/110)" "-1/11\n")
    (cons "(+ 1 2 3 -1/2 -3/4 -5/6 -7/8 9 12/5)" "1733/120\n")
    (cons "(+ (+ 1 2 3) (+ -1/2 -3/4) (+ -5/6 -7/8 9) 12/5)" "1733/120\n")    
    (cons "(+)" "0\n")
    (cons "(+ -5/6)" "-5/6\n")
    (cons "(+ (+ 1 2) 1/3)" "10/3\n")
    (cons "(+ 1 2)" "3\n")
    (cons "(+ 1/2 2/3)" "7/6\n")
    (cons "(+ 3 4 5)" "12\n")
    (cons "(apply + '(1 2 3 4 5))" "15\n")
    
    ;-
    (cons "(- 5)" "-5\n")
    (cons "(- 5 6 7)" "-8\n")
    (cons "(- 5/6 6/7 7/8 9)" "-1663/168\n")
    (cons "(- (- 5/6 6/7) (- 7/8 9))" "1361/168\n")
    (cons "(- 3)" "-3\n")
    (cons "(- 0 3)" "-3\n")
    (cons "(- -2/3)" "2/3\n")
    (cons "(- 4 3)" "1\n")
    (cons "(- 4 3 2 1)" "-2\n")
    (cons "(apply - '(-1 -2 -3 -4 -5))" "13\n")    
    
    ;*
    (cons "(*)" "1\n")
    (cons "(* -2/3)" "-2/3\n")
    (cons "(* -2/3 5/6 1 2 3)" "-10/3\n")
    (cons "(* -2/3 5/6 1 2 3 0)" "0\n")
    (cons "(* 3/4)" "3/4\n")
    (cons "(* 1 1/2)" "1/2\n")
    (cons "(* 3 4 11/2)" "66\n")
    (cons "(* 3 (* 4 11/2))" "66\n")
    (cons "(* 3 (* 4 -11/2))" "-66\n")
    (cons "(apply * '(1 2 3 4 5))" "120\n")    
    
    ;/
    (cons "(/ 2)" "1/2\n")
    (cons "(/ -2)" "-1/2\n")
    (cons "(/ -2/3)" "-3/2\n")
    (cons "(/ -2/4)" "-2\n")
    (cons "(/ 1 2 3)" "1/6\n")
    (cons "(/ -2/3 4/5 1 2 12/13)" "-65/144\n")
    (cons "(/ -2/3 4/5 1 2 -12/13)" "65/144\n")
    (cons "(/ 1/2 1/4)" "2\n")
    (cons "(/ 1/2 -1/4)" "-2\n")
    (cons "(/ -1/2 1/4)" "-2\n")
    (cons "(/ -1/2 -1/4)" "2\n")
    (cons "(/ -17)" "-1/17\n")
    (cons "(/ 1/2)" "2\n")
    (cons "(/ 3 4)" "3/4\n")    
    (cons "(/ 60 5 4 3 2)" "1/2\n")
    (cons "(apply / '(60 5 4 3 2))" "1/2\n")
    
    ;>
    (cons "(> 0)" "#t\n")
    (cons "(> 100)" "#t\n")
    (cons "(> 2/3)" "#t\n")
    (cons "(> 2 -1)" "#t\n")
    (cons "(> 2 -1 -1/2)" "#f\n")
    (cons "(> 2 -1 1/2)" "#f\n")
    (cons "(> 5 4 2 3)" "#f\n")
    (cons "(> 5 4 3 2 5/4 1 1/2 0 -1/2 -1)" "#t\n")
    (cons "(> 3/4 1/2)" "#t\n")
    (cons "(> 5/4 1)" "#t\n")
    (cons "(> 1 5/4)" "#f\n")
    (cons "(> 2 5/4)" "#t\n")
    (cons "(> 1 2 2 3 3 4)" "#f\n")
    (cons "(apply > '(4 3 3 2))" "#f\n")
    
    ;<
    (cons "(< -1/2)" "#t\n")
    (cons "(< 1 -1/2)" "#f\n")
    (cons "(< -1 -1/2)" "#t\n")
    (cons "(< -1 -1/2 -2)" "#f\n")
    (cons "(< 1/2 3/4)" "#t\n")
    (cons "(< 3/4 1/2)" "#f\n")
    (cons "(< -1 -1/2 0 1 3/2 2 5/2 3 7/2 4 100)" "#t\n")
    (cons "(< -5 -4 -3/2 0 1 2 3 4 9/2 10/2 11/2 12/2 13/2 12)" "#t\n")
    (cons "(< -5 -4 -3/2 0 1 2 3 4 9/2 10/2 11/2 12/2 13/2 -1/2 12)" "#f\n")
    (cons "(< -1 -1/2 0 1 3/2 2 5/2 3 7/2 -5 100)" "#f\n")
    (cons "(< 1 -1/2 0 1 3/2 2 5/2 3 7/2 -5 100)" "#f\n")
    (cons "(< 1/2 2/3 3/4)" "#t\n")
    (cons "(apply < '(1 2 3 4))" "#t\n")
    
    ;=
    (cons "(= 1/2)" "#t\n")
    (cons "(= 1 1 1 1 1 1 1 2/2 4/4 1 1)" "#t\n")
    (cons "(= 1 1 1 1 1 1 1 2/2 -4/4 1 1)" "#f\n")
    (cons "(= 2/4 1/2 1/2 1/2)" "#t\n")
    (cons "(= -2/4 -1/2 -1/2 -1/2)" "#t\n")
    (cons "(= -2/4 1/2 -1/2 -1/2)" "#f\n")
    (cons "(= 1 2 3 4 5 6)" "#f\n")
    (cons "(= 7 7)" "#t\n")
    (cons "(= 7 9)" "#f\n") 
    (cons "(apply = (list 1 1 1 1 1 1 1 2/2 -4/4 1 1))" "#f\n")
    
    ;remainder
    (cons "(remainder 2 1)" "0\n")
    (cons "(remainder 2 10)" "2\n")
    (cons "(remainder 10 2)" "0\n")
    (cons "(remainder 10 3)" "1\n")
    (cons "(remainder -10 3)" "-1\n")
    (cons "(remainder -10 -3)" "-1\n")
    (cons "(remainder 0 10)" "0\n")
    (cons "(remainder -4 10)" "-4\n")
    (cons "(remainder -4 -10)" "-4\n")
    (cons "(remainder 16 4)" "0\n")
    (cons "(remainder 5 2)" "1\n")
    (cons "(remainder -45 7)" "-3\n")
    (cons "(remainder 10 -3)" "1\n")
    (cons "(remainder -17 -9)" "-8\n")
    
    ;string-set!
    (cons "(string-set! \"ABC\" 0 #\\E)" "")
    (cons "(define x \"ABC\") (string-set! x 0 #\\E) x" "\"EBC\"\n")
    (cons "(define x \"ABC\") (string-set! x 1 #\\e) x" "\"AeC\"\n")
    (cons "(define x \"ABC\") (string-set! x 2 #\\1) x" "\"AB1\"\n")
    (cons 
	"(let ((x \"A1bC2345cdefGh\"))
	      (string-set! x 0 #\\Z)
	      (string-set! x 5 #\\m)
	      (string-set! x 10 #\\2)
	      x)" "\"Z1bC2m45cd2fGh\"\n")
    (cons "(let ((str \"hi three\"))
		(string-set! str 5 #\\e)
		(string-set! str 6 #\\r)
		str)" "\"hi there\"\n")	      
	
    ;vector-set!
    (cons "(define x '#(1 2 3 4 a)) (vector-set! x 0 \"String\") x" 
	  "#5(\"String\" 2 3 4 a)\n")
    (cons "(define x '#(1 2 3 4 a)) (vector-set! x 1 #t) x" 
	  "#5(1 #t 3 4 a)\n")	  
    (cons "(define x '#(1 2 3 4 a)) (vector-set! x 2 #\\2) x" 
	  "#5(1 2 #\\2 4 a)\n")
    (cons "(define x '#(1 2 3 4 a)) (vector-set! x 3 '(1)) x" 
	  "#5(1 2 3 (1 . ()) a)\n")
    (cons "(define x '#(1 2 3 4 a)) (vector-set! x 4 '#(\"Akuna Matata\")) x" 
	  "#5(1 2 3 4 #1(\"Akuna Matata\"))\n")
    (cons 
      "(let ((x (make-vector 3 'a)))
	    (vector-set! x 0 1)
	    (vector-set! x 1 -3/4)
	    (vector-set! x 1 '(-5/6 12/36))
	    (vector-set! x 2 -3/4)
	    x)" "#3(1 (-5/6 . (1/3 . ())) -3/4)\n")
    (cons "(let ((v (vector 'a 'b 'c 'd 'e)))
      (vector-set! v 2 'x)
      v)" "#5(a b x d e)\n")	    
	
    ;set-car!
    (cons "(define x '(a b)) (set-car! x 1) x" "(1 . (b . ()))\n")
    (cons "(define x '(a . b)) (set-car! x 1) x" "(1 . b)\n")
    (cons 
      "(let ((x (cons #\\a 2)))
	      (set-car! x -1/2)
	      (set-car! x #f)
	      x)" "(#f . 2)\n")
    (cons "(let ((x '(a b c)))
      (set-car! x 1)
      x)" "(1 . (b . (c . ())))\n")	      

    ;set-cdr!
    (cons "(define x '(a . b)) (set-cdr! x 'c) x" "(a . c)\n")
    (cons "(define x '(a #t #f 1 2 3)) (set-cdr! x \"ABC\") x" "(a . \"ABC\")\n")
    (cons "(define x '(a #t #f 1 2 3)) (set-cdr! (cdr x) \"ABC\") x" "(a . (#t . \"ABC\"))\n")	
    (cons "(let ((x '(a b c)))
	  (set-cdr! x 1)
	  x)" "(a . 1)\n")
    
    ;symbol->string
    (cons "(symbol->string 'AbC)" "\"abc\"\n")
    (cons "(symbol->string 'abcdefghijkl123456)" "\"abcdefghijkl123456\"\n")
    (cons "(symbol->string 'xyz)" "\"xyz\"\n")
    (cons "(symbol->string (string->symbol \"Hi\"))" "\"Hi\"\n")
    (cons "(symbol->string (string->symbol \"()\"))" "\"()\"\n")    
    
    ;string->symbol
    (cons "(begin 'a 'b 'c 'd 'e 'f 'g 'g234 'abc (string->symbol \"a\"))" "a\n")
    (cons "(begin 'a 'b 'c 'd 'e 'f 'g 'g234 'abc (string->symbol \"b\"))" "b\n")
    (cons "(begin 'a 'b 'c 'd 'e 'f 'g 'g234 'abc (string->symbol \"c\"))" "c\n")
    (cons "(begin 'a 'b 'c 'D 'e 'f 'g 'g234 'abc (string->symbol \"D\"))" "D\n")
    (cons "(begin 'a 'b 'c 'D 'e 'f 'g 'g234 'abc (string->symbol \"e\"))" "e\n")
    (cons "(begin 'a 'b 'c 'D 'e 'f 'g 'g234 'abc (string->symbol \"f\"))" "f\n")
    (cons "(begin 'a 'b 'c 'D 'e 'f 'g 'g234 'abc (string->symbol \"g\"))" "g\n")
    (cons "(begin 'a 'b 'c 'D 'e 'f 'g 'g234 'abc (string->symbol \"g234\"))" "g234\n")
    (cons "(begin 'a 'b 'c 'd 'e 'f 'g 'g234 'abc (string->symbol \"abc\"))" "abc\n")
    (cons "(begin 'a 'b 'c 'd 'D 'e 'f 'g 'g234 'abc (string->symbol \"D\"))" "D\n")
    (cons "(string->symbol (make-string 1 #\\d))" "d\n")
    (cons "(begin 'a 'b 'c 'd 'D 'e 'f 'g 'g234 'abc (string->symbol (make-string 1 #\\d)))" "d\n")
    (cons "(string->symbol \"abc\")" "abc\n")
    (cons "(string->symbol \"aBc\")" "aBc\n")
    (cons "(begin 'aBc (string->symbol \"aBc\"))" "aBc\n")
))

(define eq-tests
  (list
    ;void
    (cons "(begin (define x 5) (eq? (set! x 6) (set! x -1/2)))" "#t\n")
    
    ;nil
    (cons "(eq? '() (list))" "#t\n")
    (cons "(eq? '() (append))" "#t\n")
    
    ;char
    (cons "(eq? #\\a #\\a)" "#t\n")
    
    ;string
    (cons "(eq? \"a\" (make-string 1 #\\a))" "#f\n")
    ;(cons "(eq? (symbol->string 'abc) (symbol->string 'abc))" "#t\n")
    
    ;integer
    (cons "(eq? 1 (+ 1/2 1/2))" "#t\n")
    (cons "(eq? 1 (+ 1/2 1/4))" "#f\n")
    
    ;fraction
    (cons "(eq? 1/2 (/ 2 4))" "#t\n")
    
    ;boolean
    (cons "(eq? #t (zero? 0))" "#t\n")
    (cons "(eq? #t (integer? 1))" "#t\n")
    (cons "(eq? #f (integer? 1/2))" "#t\n")
    
    ;list
    (cons "(eq? '(1) '(1))" "#t\n")
    (cons "(eq? '(1) (list 1))" "#f\n")
    (cons "(eq? (list 1) (list 1))" "#f\n")
    
    ;vector
    (cons "(eq? '#(1) '#(1))" "#t\n")
    (cons "(eq? '#(1) (make-vector 1 1))" "#f\n")
    (cons "(eq? (make-vector 1 1) (make-vector 1 1))" "#f\n")
    
    ;symbol
    (cons "(eq? (string->symbol \"a\") (string->symbol (make-string 1 #\\a)))" "#t\n")
    (cons "(eq? (string->symbol (make-string 1 #\\a)) (string->symbol (make-string 1 #\\a)))" "#t\n")
    (cons "(eq? (string->symbol \"bb\") (string->symbol (make-string 2 #\\b)))" "#t\n")
    (cons "(eq? (string->symbol \"aa\") (string->symbol (make-string 2 #\\b)))" "#f\n")
    (cons "(eq? (string->symbol \"x\") 'x)" "#t\n")
    (cons "(eq? (string->symbol \"X\") 'x)" "#f\n")
    
    ;closures
    (cons "(eq? car car)" "#t\n")
    (cons "(eq? map map)" "#t\n")
    (cons "(eq? car cdr)" "#f\n")
    (cons "(eq? car 'car)" "#f\n")
    (cons "(eq? (lambda (x) x) (lambda (x) x))" "#f\n")
    
    ;others
    (cons "(eq? #t 1)" "#f\n")
    (cons "(eq? #t '#(t))" "#f\n")
    (cons "(eq? 'a #\\a)" "#f\n")
    (cons "(eq? -1/2 1)" "#f\n")
    (cons "(eq? \"A\" \"a\")" "#f\n")
))

(define internal-helper-procedures-tests
  (list
    ;one-list-map
    (cons "(asaf-lior-one-list-map (lambda (x) x) '(1 2 3))" "(1 . (2 . (3 . ())))\n")
    (cons "(asaf-lior-one-list-map car '((1) (2) (3)))" "(1 . (2 . (3 . ())))\n")
    ;(cons "(asaf-lior-one-list-map caar '(((1)) ((2)) ((3))))" "(1 . (2 . (3 . ())))\n")
    (cons "(asaf-lior-one-list-map cdr (list))" "()\n")
    
    ;foldr
    (cons "(asaf-lior-foldr cons '() '(1 2 3))" "(1 . (2 . (3 . ())))\n")
    
    ;binary-append
    ;(cons "(binary-append '(1 2) '(3 4 5))" "(1 . (2 . (3 . (4 . (5 . ())))))\n")
    ;(cons "(binary-append '(1 2) '())" "(1 . (2 . ()))\n")
    ;(cons "(binary-append '(1) '(2))" "(1 . (2 . ()))\n")
    ;(cons "(binary-append '() '(1 2))" "(1 . (2 . ()))\n")
    
    ;list-to-vector - helper procedure
    (cons "(asaf-lior-list-to-vector '())" "#0()\n")
    (cons "(asaf-lior-list-to-vector '(1))" "#1(1)\n")
    (cons "(asaf-lior-list-to-vector (list 1 2 3 #t #f -1/2 \"a\" #\\b))" "#8(1 2 3 #t #f -1/2 \"a\" #\\b)\n")
   
    
    ;box-get box-set box
;;     (cons "((lambda (x) (box-get x)) (box 5))" "5\n")
;;     (cons "((lambda (y) ((lambda (x) (box-get y)) 5)) (box '(a b c)))" "(a . (b . (c . ())))\n")
;;     (cons "((lambda (y) ((lambda (x) (begin (box-set y 12) (box-get y))) 5)) (box '(a b c)))" "12\n")
;;     (cons "((lambda (x) (begin (box-set x \"AbC\") (box-get x))) (box 5))" "\"AbC\"\n")
;;     (cons "((lambda (y) ((lambda (x) (set! x (box x)) (box-get x)) 1)) (box '(a b c)))" "1\n")
;;     (cons
;; 	  "(define *example*
;; 	    (let ((a 0))
;; 	      (begin (lambda () a)
;; 	      (lambda () (set! a (+ a 1)))
;; 	      ((lambda (b) (set! a b) a) a)))) *example*" "0\n")
	      
    ;+
    (cons "(asaf-lior-reduce-num 10/2)" "5\n")
    (cons "(asaf-lior-binary-int-frac-plus 3 5/3)" "14/3\n")
    (cons "(asaf-lior-binary-int-int-plus 3 24)" "27\n")
    (cons "(asaf-lior-binary-frac-frac-plus 3/2 3/2)" "12/4\n")
    (cons "(asaf-lior-binary-frac-frac-plus -3/2 -3/2)" "-12/4\n") 
    (cons "(asaf-lior-reduce-num 1/2)" "1/2\n")
    (cons "(asaf-lior-opposite-num 1)" "-1\n")
    (cons "(asaf-lior-opposite-num 1/2)" "-1/2\n")
    
    ;*
    (cons "(asaf-lior-binary-int-int-mul 1 2)" "2\n")
    (cons "(asaf-lior-binary-int-int-mul -3 12)" "-36\n")
    (cons "(asaf-lior-binary-int-int-mul 5 0)" "0\n")
    (cons "(asaf-lior-binary-int-frac-mul 2 1/2)" "2/2\n")
    (cons "(asaf-lior-reduce-num (asaf-lior-binary-int-frac-mul 2 1/2))" "1\n")
    (cons "(asaf-lior-reduce-num (asaf-lior-binary-int-frac-mul 12 4/6))" "8\n")
    (cons "(asaf-lior-reduce-num (asaf-lior-binary-int-frac-mul 5 2/4))" "5/2\n")
    (cons "(asaf-lior-binary-frac-frac-mul 1/3 3/2)" "3/6\n")
    (cons "(asaf-lior-reduce-num (asaf-lior-binary-frac-frac-mul 1/3 3/2))" "1/2\n")
    (cons "(asaf-lior-reduce-num (asaf-lior-binary-frac-frac-mul -2/3 3/2))" "-1\n")
    
    (cons "(asaf-lior-inverse-num -2)" "-1/2\n")
    (cons "(asaf-lior-inverse-num 2/3)" "3/2\n")
    (cons "(asaf-lior-inverse-num -2/3)" "-3/2\n")
    
    (cons "(asaf-lior-greater-than-int-int 1 2)" "#f\n")
    (cons "(asaf-lior-greater-than-int-int 3 -2)" "#t\n")
    
    (cons "(asaf-lior-binary-gt 2 -1)" "#t\n")
    (cons "(asaf-lior-binary-gt 3/4 1/2)" "#t\n")
    (cons "(asaf-lior-binary-gt 5/4 1)" "#t\n")
    (cons "(asaf-lior-binary-gt 1 5/4)" "#f\n")
    (cons "(asaf-lior-binary-gt 2 5/4)" "#t\n")
    
    (cons "(asaf-lior-binary-lt 1 2)" "#t\n")
    (cons "(asaf-lior-binary-lt 1 -1/2)" "#f\n")
    (cons "(asaf-lior-binary-lt -1 -1/2)" "#t\n")
    (cons "(asaf-lior-binary-lt -1/2 -3/4)" "#f\n")
    (cons "(asaf-lior-binary-lt 1/2 3/4)" "#t\n")
    (cons "(asaf-lior-binary-lt 3/4 1/2)" "#f\n")
    
    (cons "(asaf-lior-binary-eq 1 1)" "#t\n")
    (cons "(asaf-lior-binary-eq 1 2)" "#f\n")
    (cons "(asaf-lior-binary-eq 1/2 1/4)" "#f\n")
    (cons "(asaf-lior-binary-eq 1/2 2/4)" "#t\n")
    (cons "(asaf-lior-binary-eq 2 4/2)" "#t\n")
    (cons "(asaf-lior-binary-eq -1 -1)" "#t\n")
    
    ;(cons "(string-equal \"a\" \"a\")" "#t\n")
    ;(cons "(string-equal \"aa\" (make-string 3 #\\a))" "#f\n")
    ;(cons "(string-equal \"abc\" \"abd\")" "#f\n")
    ;(cons "(string-equal \"aaa\" (make-string 3 #\\a))" "#t\n")
    ;(cons "(string-equal \"aaa\" (make-string 3 #\\A))" "#f\n")
    ;(cons "(string-equal \"aab\" (make-string 3 #\\a))" "#f\n")
    ;(cons "(begin 'abc (string-equal \"abc\" (symbol->string 'abc)))" "#t\n")    
))

;;; Tests list for debugging purposes
(define tests
  (list   
))    


(display (format "\033[1mComp171 - Compiler Tests\033[0m\n================================\n"))

(runAllTests
  (list      
      (cons "Constants Table" constants-table-tests)  
      (cons "Or, If and Begin" or-if-begin-tests)      
      (cons "Lambda-simple" lambda-simple-tests)
      (cons "Lambda-opt" lambda-opt-tests)
      (cons "Lambda-var" lambda-var-tests)
      (cons "tc-applic-tests" tc-applic-tests)
      (cons "comp161 torture test for compiler unsorted" comp161-torture-test-for-compiler-unsorted)
      (cons "Set" set-tests)
      (cons "pvar-bvar" pvar-bvar-tests)
      (cons "Define" define-tests)
      (cons "Primitive Functions" primitive-functions-tests)
      (cons "eq?" eq-tests) 
      ;(cons "Internal Helper Procedures" internal-helper-procedures-tests)
      ;(cons "Debugging" tests)  
      
))