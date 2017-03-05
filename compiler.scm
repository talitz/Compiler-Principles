(load "./pc.scm")
(load "./pattern-matcher.scm")

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))
     
(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))
	 
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       done))
       
(define <infix-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
	<sexpr-comment>))
	
(define <comment-infix>
  (disj <line-comment>
	<infix-comment>))

(define <skip>
  (disj <comment>
	<whitespace>))
	
(define <skip-infix>
  (disj <comment-infix>
	<whitespace>))
	
(define times-4
  (lambda (<p>)
    (disj (times <p> 4) (times <p> 3) (times <p> 2) (times <p> 1))
  ))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))

(define ^<skipped-infix*> (^^<wrapped> (star <skip-infix>)))

(define <Boolean>
	(new    (*parser (char #\#))
	        (*parser (char-ci #\t))
	        (*caten 2)
	        (*parser (char #\#))
	        (*parser (char-ci #\f))
	        (*caten 2)
		(*disj 2)
		(*pack-with 
		  (lambda (a b)
		    (if (or (eq? b #\t) (eq? b #\T)) #t #f)))
        done))
	     
(define <CharPrefix>
      (new     (*parser (char #\#))
               (*parser (char #\\))
               (*caten 2)
       done))
       
(define <VisibleSimpleChar> (range #\! #\xff))

(define <NamedChar>
	(new    (*parser (word-ci "lambda"))
	        (*parser (word-ci "newline"))
            (*parser (word-ci "nul"))
            (*parser (word-ci "page")) 
	        (*parser (word-ci "return"))
	        (*parser (word-ci "space"))
	        (*parser (word-ci "tab"))
		(*disj 7) 
                (*pack
                  (lambda (a)
                    (cond ((equal? (list->string a) "lambda") #\x03bb)
                          ((equal? (list->string a) "newline") #\newline)
                          ((equal? (list->string a) "nul") #\nul)
                          ((equal? (list->string a) "page") #\page)
                          ((equal? (list->string a) "return") #\return)
                          ((equal? (list->string a) "space") #\space)
                          ((equal? (list->string a) "tab") #\tab)
                          (else ("error")))))
	     done))
	     
(define <HexChar>
	(new    (*parser (range #\0 #\9))
	        (*parser (range #\a #\f))
		    (*disj 2) 
	     done))
	     
(define <HexUnicodeChar>
	(new    (*parser (char-ci #\x))
	        (*parser (times-4 <HexChar>))
		    (*caten 2)
		    (*parser <HexChar>)
            *not-followed-by
		    (*pack-with (lambda (a b)
		       (integer->char (string->number (list->string b) 16))))
	     done))
	     
(define <SymbolChar>
        (new    (*parser (range #\0 #\9))
                (*parser (range #\a #\z))
                (*parser (range #\A #\Z))
                (*parser (char #\!))
                (*parser (char #\$))
                (*parser (char #\^))
                (*parser (char #\*))
                (*parser (char #\-))
                (*parser (char #\_))
                (*parser (char #\=))
                (*parser (char #\+))
                (*parser (char #\>))
                (*parser (char #\<))
                (*parser (char #\?))
                (*parser (char #\/))
                (*parser (char #\:))
                (*disj 16)
        done))	    

(define <Char>
	(new    (*parser <CharPrefix>)
            (*parser <NamedChar>)
            (*parser <HexUnicodeChar>)
	        (*parser <VisibleSimpleChar>)
	        (*parser <HexChar>)
            *not-followed-by
            (*disj 3)
            (*caten 2)
            (*pack-with (lambda (a b) b))
	     done))
	     
(define <Natural>
	(new    (*parser (range #\0 #\9)) *plus
	        (*pack (lambda (a)
	            (string->number (list->string a))))
         done))
	     
(define <Integer>
	(new    (*parser (char #\+))
            (*parser (char #\-))
		    (*disj 2)
		    (*parser <Natural>)
		    (*caten 2)
		    (*pack-with (lambda (a b)
		       (if (equal? a #\+) b (- b))))
		    (*parser <Natural>)
		    (*disj 2)
	     done))
	     
(define <Fraction>
	(new    (*parser <Integer>)
	        (*parser (char #\/))
		    (*parser <Natural>)
		    (*caten 3)
		    (*pack-with (lambda (a b c)
		        (/ a c)))
	     done))

(define <Number>
	(new   (*parser <Fraction>) 
	       (*parser <Integer>)
	       (*disj 2) 
	       (*parser <SymbolChar>)
	       (*parser <Natural>)
	       *diff
	       *not-followed-by
	     done))
	     
(define <StringVisibleChar> (diff <any-char> (char #\\)))
	     
(define <StringMetaChar>
	(new    (*parser (char #\\)) 
	        (*parser (char #\\)) 
		    (*parser (char #\"))
		    (*parser (char-ci #\t))
		    (*parser (char-ci #\f))
		    (*parser (char-ci #\n))
            (*parser (char-ci #\r))
		    (*disj 6)
		    (*caten 2)
		    (*pack-with (lambda (a b)
		         (cond ((equal? b #\\) #\\)
		               ((equal? b #\") #\")
		               ((or (equal? b #\t) (equal? b #\T)) #\tab)
		               ((or (equal? b #\f) (equal? b #\F)) #\xc)
		               ((or (equal? b #\n) (equal? b #\N)) #\xa)
		               ((or (equal? b #\r) (equal? b #\R)) #\xd))))
	     done))

(define <StringHexChar>
	(new    (*parser (char #\\))
            (*parser (char-ci #\x))
            (*caten 2)
		    (*parser (times-4 <HexChar>))
		    (*parser (char #\;))
            (*caten 3)
            (*pack-with (lambda (a b c)
                (integer->char (string->number (list->string b) 16))))
	     done))
	     
(define <StringChar>
        (new    (*parser <StringHexChar>)
                (*parser <StringMetaChar>)
                (*parser <StringVisibleChar>)
                (*disj 3)
             done))
	     
(define <String>
	(new    (*parser (char #\"))  
		    (*parser <StringChar>) (*parser (char #\")) *diff *star
		    (*parser (char #\"))
		    (*caten 3) 
		    (*pack-with (lambda (a b c)
		       (list->string b)))
	     done))	
	     
	     
(define <Symbol>
        (new    (*parser <SymbolChar>) *plus
                (*pack (lambda (a)
                    (string->symbol (string-downcase (list->string a)))))
        done))
	     
(define <ProperList>
        (new    (*parser (char #\())
                (*delayed (lambda () <sexpr>)) *star
                (*parser (char #\)))
                (*caten 3)
                (*pack-with (lambda (a b c) b))
        done))

(define <ImproperList>
        (new    (*parser (char #\())
                (*delayed (lambda () <sexpr>)) *plus
                (*parser (char #\.))
                (*delayed (lambda () <sexpr>))
                (*parser (char #\)))
                (*caten 5)
                (*pack-with (lambda (a b c d e)
                    `(,@b . ,d))
                        )
        done))
        
(define <Vector>
        (new    (*parser (char #\#))
                (*parser (char #\())
                (*delayed (lambda () <sexpr>)) *star
                (*parser (char #\)))
                (*caten 4)
                (*pack-with (lambda (a b c d)
                    (apply vector c)))
        done))
        
(define <Quoted>
        (new    (*parser (char #\'))
                (*delayed (lambda()  <sexpr>))
                (*caten 2)
                (*pack-with (lambda (a b)
                    `',b))
        done))
        
(define <QuasiQuoted>
        (new    (*parser (char #\`))
                (*delayed (lambda()  <sexpr>))
                (*caten 2)
                (*pack-with (lambda (a b)
                     (list 'quasiquote b)))
        done))
        
(define <Unquoted>
        (new    (*parser (char #\,))
                (*delayed (lambda()  <sexpr>))
                (*caten 2)
                (*pack-with (lambda (a b)
                     (list 'unquote b)))                
        done))        
        
(define <UnquotedAndSpliced>
        (new    (*parser (char #\,))
                (*parser (char #\@))
                (*delayed (lambda()  <sexpr>))
                (*caten 3)
                (*pack-with (lambda (a b c)
                     (list 'unquote-splicing c)))
        done))
        
(define last-elem (lambda (l) (car (reverse l))))

(define remove-last-elem (lambda (l) (reverse (cdr (reverse l)))))

(define char->symbol (lambda (a)
    (cond ((equal? a #\+) '+)
          ((equal? a #\-) '-)
          ((equal? a #\/) '/)
          ((equal? a #\*) '*))))

(define infix-pack-l (lambda (a b)
        (if (null? b) a
           (fold-left (lambda (x y) `(,(char->symbol (car y)) ,x ,(cadr y))) a b))))      
           
(define infix-pack-s-l (lambda (s)
    (lambda (a b)
        (if (null? b) a
           (fold-left (lambda (x y) `(,s ,x ,(cadr y))) a b)))))      

(define infix-pack-s-r (lambda (s)
     (lambda (a b)
        (if (null? b) a
           (let* ((b-values (map cadr b))
               (last-elem (last-elem b-values))
               (b-reduced (remove-last-elem b-values))
               (b-final (cons a b-reduced)))
             (fold-right (lambda (x y) `(,s ,x ,y)) last-elem b-final))))
        ))      
        
(define <InfixPrefixExtensionPrefix>
        (new    (*parser (char #\#))
                (*parser (char #\#))
                (*caten 2)
                (*parser (char #\#))
                (*parser (char #\%))
                (*caten 2)
                (*disj 2)
        done))
        
(define <InfixAddSub> 
       (new     (*delayed (lambda() <InfixMulDiv>))
                (*parser (char #\+))
                (*parser (char #\-))
                (*disj 2)
                (*delayed (lambda() <InfixMulDiv>))
                (*caten 2) *star
                (*caten 2)
                (*pack-with infix-pack-l)
        done))
        
(define <InfixMulDiv> 
       (new     (*delayed (lambda() <InfixPow>))
                (*parser (char #\*))
                (*parser (char #\/))
                (*disj 2)
                (*delayed (lambda() <InfixPow>))
                (*caten 2) *star
                (*caten 2)
                (*pack-with infix-pack-l)
        done))
        
(define <PowerSymbol>
       (new     (*parser (word "**"))
                (*parser (char #\^))
                (*disj 2)
        done))
        
(define <InfixPow> 
       (new     (*delayed (lambda() (^<skipped-infix*> <InfixFuncArray>)))
                (*parser <PowerSymbol>)
                (*delayed (lambda() (^<skipped-infix*> <InfixFuncArray>)))
                (*caten 2) *star
                (*caten 2)
                (*pack-with (infix-pack-s-r 'expt))
        done))
        
(define <InfixFuncArray>
       (new     (*delayed (lambda() <InfixLast>))
                (*parser (char #\[))
                (*delayed (lambda() <InfixExpression>))
                (*parser (char #\]))
                (*caten 3) 
                (*pack-with (lambda (a b c) (cons 'arr b)))
                (*parser (char #\())
                (*delayed (lambda() <InfixArgList>))
                (*parser (char #\)))
                (*caten 3) 
                (*pack-with (lambda (a b c) (cons 'func b)))
                (*disj 2)
                *star
                (*caten 2) 
                (*pack-with (lambda (a b)
                     (fold-left (lambda (x y)
                        (let ((y-op (car y))
                              (y (cdr y)))
                        (if (eq? y-op 'arr) 
                           `(vector-ref ,x ,y)
                           `(,x ,@y))))
                        a b)))
        done))
        
(define <InfixArrayGet> 
       (new     (*delayed (lambda() (^<skipped-infix*> <InfixFuncall>)))
                (*parser (char #\[))
                (*delayed (lambda() <InfixExpression>))
                (*parser (char #\]))
                (*caten 3) 
                *star
                (*parser <epsilon>)
                (*disj 2)
                (*caten 2) 
                (*pack-with (infix-pack-s-l 'vector-ref))
        done))
        
 (define <InfixFuncall>
       (new     (*delayed (lambda() <InfixLast>))
                (*parser (char #\())
                (*delayed (lambda() <InfixArgList>))
                (*parser (char #\)))
                (*caten 3)
                *star
                (*parser <epsilon>)
                (*disj 2)
                (*caten 2)
                (*pack-with (lambda (a b)
                    (if (null? b) a
                    (fold-left (lambda (x y) `(,x ,@(cadr y))) a b))))      
        done))
        
(define <InfixArgList>
        (new    (*delayed (lambda() <InfixExpression>))
                (*parser (char #\,))
                (*delayed (lambda() <InfixExpression>))
                (*caten 2) *star
                (*pack (lambda(a)
                            (map cadr a)))
                (*caten 2)
                (*pack-with (lambda(a b)
                                 (cons a b)))
                            
                (*parser (^<skipped-infix*> <epsilon>))
                (*disj 2)
        done))
        
 (define <InfixParen>
        (new    (*parser (char #\())
                (*delayed (lambda () <InfixExpression>))
                (*parser (char #\)))
                (*caten 3)
                (*pack-with (lambda (a b c) b))
        done)) 
        
(define <InfixSexprEscape>
        (new    (*delayed (lambda () <InfixPrefixExtensionPrefix>))
                (*delayed (lambda () <sexpr>))
                (*caten 2)
                (*pack-with (lambda (a b) b))
        done))
        
(define <InfixNeg>
        (new    (*parser (char #\-))
                (*delayed (lambda () <InfixFuncArray>))
                (*caten 2)
                (*pack-with (lambda (a b)
                    `(- ,b)))
        done))
        
(define <InfixLast>
        (new    (*parser (^<skipped-infix*> <InfixSexprEscape>))
                (*parser (^<skipped-infix*> <InfixParen>))
                (*delayed (lambda() (^<skipped-infix*> <InfixNumber>)))
                (*parser (^<skipped-infix*> <InfixNeg>))
                (*delayed (lambda() (^<skipped-infix*> <InfixSymbol>)))
                (*disj 5)
          done))
          
(define <InfixExpression> <InfixAddSub>)
        
(define <InfixSymbol>
        (new    (*parser <SymbolChar>) 
                (*parser (char #\+))
                (*parser (char #\-))
                (*parser (char #\*))
                (*parser (char #\^))
                (*parser (char #\/))
                (*parser (word "**"))
                (*disj 6) *diff *plus
                (*pack (lambda (a)
                    (string->symbol (string-downcase (list->string a)))))
        done))
        
(define <InfixNumber>
	(new   (*parser <Fraction>) 
	       (*parser <Integer>)
	       (*disj 2) 
	       (*parser <InfixSymbol>)
	       (*parser <Natural>)
	       *diff *not-followed-by
	     done))
        
(define <InfixExtension>
        (new    (*parser <InfixPrefixExtensionPrefix>)
                (*parser <InfixExpression>)
                (*caten 2)
                (*pack-with (lambda (a b)
                    b))
        done))
        
(define <sexpr> 
   (let* ((parsers (list <Boolean> <Char> <Number> <Symbol> <String> <ProperList>
                    <ImproperList> <Vector> <Quoted> <QuasiQuoted> <Unquoted>
                    <UnquotedAndSpliced> <InfixExtension>))
          (parsers-skipped (map ^<skipped*> parsers)))
         (apply disj parsers-skipped)
    ))

(define const-pattern-rule
     (lambda(pred)
        (pattern-rule
                (lambda (exp) (if (pred exp) (list exp) #f))
               (lambda(x) (list 'const x)))))


(define _boolean (const-pattern-rule boolean?))
(define _character (const-pattern-rule char?))
(define _number (const-pattern-rule number?))
(define _string (const-pattern-rule string?))
(define _vector (const-pattern-rule vector?))
(define _void (const-pattern-rule (lambda (x) (equal? (void) x))))

(define _quote
         (pattern-rule
              `(quote ,(? 'exp))
              (lambda (exp) `(const ,exp))))

(define _const
     (compose-patterns
          _vector
          _boolean
          _character
          _number
          _string
          _quote
          _void
            ))

(define *reserved-words*
    '(and begin cond define do else if lambda
    let let* letrec or quasiquote unquote
    unquote-splicing quote set!))

(define _var
        (pattern-rule
               (lambda (exp) (if (and (symbol? exp) (not (member exp *reserved-words*))) (list exp) #f))
               (lambda(x) (list 'var x))))

(define _if
   (compose-patterns
     (pattern-rule
          `(if ,(? 'test) ,(? 'then) ,(? 'else))
               (lambda (test then else)
               `(if3 ,(parse test) ,(parse then) ,(parse else))))
     (pattern-rule
          `(if ,(? 'test) ,(? 'then))
               (lambda (test then)
               `(if3 ,(parse test) ,(parse then) ,(parse (void)))))))

(define _or
     (pattern-rule
          `(or . ,(? 'bodies))
               (lambda (bodies)
               (cond ((null? bodies) (parse '#f))
                     ((eq? (length bodies) 1) (parse (car bodies)))
                     (else `(or ,(map parse bodies)))))))

(define _and
     (pattern-rule
          `(and . ,(? 'bodies))
               (lambda (bodies)
               (cond ((null? bodies) (parse '#t))
                     ((eq? (length bodies) 1) (parse (car bodies)))
                     (else (parse `(if ,(car bodies) (and ,@(cdr bodies)) #f)))))))

(define improper-list-last
        (lambda(l)
            (if (pair? l) (improper-list-last (cdr l)) l)))

(define improper-list-remove-last
        (lambda(l)
            (if (pair? l)
                  (cons (car l) (improper-list-remove-last (cdr l)))
                    '())))

(define remove-duplicates
     (lambda (l)
        (cond ((null? l) '())
            ((member (car l) (cdr l)) (remove-duplicates (cdr l)))
            (else (cons (car l) (remove-duplicates (cdr l)))))))

(define is-distinct
    (lambda (x)
        (equal? (length (remove-duplicates x)) (length x))))

(define _lambda
     (pattern-rule
          `(lambda ,(? 'params) . ,(? 'bodies))
               (lambda (params bodies)
                         (if (and (list? params) (not (is-distinct params)))
                            (error 'parse "Repeating lambda params!")
                            (let ((bodies (map parse bodies))
                              (new-params (improper-list-remove-last params))
                              (seq (parse `(begin ,@bodies))))
                                (cond ((list? params) `(lambda-simple ,params ,seq))
                                   ((pair? params) `(lambda-opt ,new-params ,(improper-list-last params) ,seq))
                                   (else `(lambda-var ,params ,seq))))))))

(define _define
      (pattern-rule
          `(define ,(? 'var) . ,(? 'def))
               (lambda (var def)
                  (if (pair? var)
                  (let ((exp (parse `(lambda ,(cdr var) ,@def))))
                    `(def ,(parse (car var)) ,exp))
                    (let ((def (parse `(begin ,@def))))
                      `(def ,(parse var) ,def))))))

(define _application
      (pattern-rule
          `(,(? 'func) . ,(? 'params))
               (lambda (func params)
                  (if (null? func)
                     (error 'done (format "Unknown form: ~s" func))
                     (let ((func (parse func))
                        (params (map parse params)))
                        `(applic ,func ,params))))))

(define _cond
      (pattern-rule
          `(cond . ,(? 'bodies))
               (lambda (bodies)
                    (cond ((null? bodies) (error 'parse "Unknown form: (cond)"))
                          ((eq? (caar bodies) 'else) (parse `(begin ,@(cdar bodies))))
                          ((eq? (length bodies) 1) (parse `(if ,(caar bodies) (begin ,@(cdar bodies)))))
                          (else (parse `(if ,(caar bodies) (begin ,@(cdar bodies)) (cond ,@(cdr bodies)))))))))

(define remove-seq
   (lambda (bodies)
       (cond ((null? bodies) bodies)
             ((and (list? (car bodies)) (equal? (caar bodies) 'seq))
                 (append (car (cdar bodies)) (remove-seq (cdr bodies))))
             (else (cons (car bodies) (remove-seq (cdr bodies)))))))


(define _begin
      (pattern-rule
          `(begin . ,(? 'bodies))
               (lambda (bodies)
                  (let* ((bodies (map parse bodies))
                         (bodies (remove-seq bodies)))
                    (cond ((null? bodies) (parse (void)))
                         ((eq? (length bodies) 1) (car bodies))
                         (else `(seq ,bodies)))))))

(define _let
      (pattern-rule
          `(let ,(? 'params) . ,(? 'bodies))
               (lambda (params bodies)
                      (parse `((lambda ,(map car params) ,@bodies)  ,@(map cadr params))))))

(define _let*
      (pattern-rule
          `(let* ,(? 'params) . ,(? 'bodies))
               (lambda (params bodies)
                      (cond ((null? params)(parse `(let ,params ,@bodies)))
                            ((eq? (length params) 1) (parse `(let ,params ,@bodies)))
                            (else (parse `(let ,(list (car params)) (let* ,(cdr params) ,@bodies))))))))

(define _letrec
      (pattern-rule
          `(letrec ,(? 'params) . ,(? 'bodies))
               (lambda (params bodies)
                    (let ((set-bodies (map (lambda (x y) `(set! ,x ,y))
                                       (map car params) (map cadr params)))
                          (false-params (map (lambda (x y) `(,x #f))
                                           (map car params) (map cadr params)))
                          (let-bodies `((let () ,@bodies))))
                         (parse `(let ,false-params ,@(append set-bodies let-bodies)))))))

(define _set!
     (pattern-rule
          `(set! ,(? 'arg) ,(? 'val))
             (lambda (arg val)
                 `(set ,(parse arg) ,(parse val)))))

(define _quasiquote
     (pattern-rule
             (lambda (exp) (if (eq? (car exp) 'quasiquote) (cdr exp) #f))
             (lambda (exp)
                 (parse (expand-qq exp)))))


(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))



(define parse
 (let ((run
  (compose-patterns
     _const
     _var
     _cond
     _if
     _or
     _and
     _lambda
     _define
     _begin
     _let
     _let*
     _letrec
     _set!
     _quasiquote
     _application
     )))
  (lambda (sexpr)
        (run sexpr (lambda () "Exception in parser")))))

(define eliminate-nested-defines-helper
       (lambda(parsed-expr)
           (cond ((null? parsed-expr) (cons '() '()))
                 ((is-lambda? (car parsed-expr)) (cons '() (list parsed-expr)))
                 ((list? (car parsed-expr))
                    (let ((res1 (eliminate-nested-defines-helper (car parsed-expr)))
                          (res2 (eliminate-nested-defines-helper (cdr parsed-expr))))
                       (cons (append (car res1) (car res2)) (append (cdr res1) (cdr res2)))))
                 ((eq? (car parsed-expr) 'def) (cons (list parsed-expr) '()))
                 ((eq? (car parsed-expr) 'seq) (eliminate-nested-defines-helper (cadr parsed-expr)))
                 (else (cons '() (list parsed-expr))))))

(define is-set?
      (lambda(e) (and (eq? (car e) 'set) (eq? (length e) 3))))

(define is-lambda?
      (lambda(e) (or (eq? e 'lambda-simple) (eq? e 'lambda-var) (eq? e 'lambda-opt))))

(define lambda-get-def
      (lambda(e)
          (car e)))

(define lambda-get-params
      (lambda(e)
          (remove-last-elem (cdr e))))

(define lambda-get-listed-params
      (lambda(e)
          (letrec ((convert-to-list (lambda (x)
                (if (null? x)
                     x
                     (if (list? (car x))
                        (append (car x) (convert-to-list (cdr x)))
                        (append (list (car x)) (convert-to-list (cdr x))))))))
                  (convert-to-list (lambda-get-params e)))))

(define lambda-get-body
      (lambda(e)
          (last e)))

(define eliminate-nested-defines
      (lambda(parsed-expr)
          (cond ((null? parsed-expr) `())
                ((list? (car parsed-expr))
                   (cons (eliminate-nested-defines (car parsed-expr))
                         (eliminate-nested-defines (cdr parsed-expr))))
                ((is-lambda? (car parsed-expr))
                    (let* ((lambda-def (lambda-get-def parsed-expr))
                           (body (eliminate-nested-defines (lambda-get-body parsed-expr)))
                           (params (lambda-get-params parsed-expr))
                           (res (eliminate-nested-defines-helper body))
                           (ds (car res))
                           (es (cdr res)))
                                (if (null? ds)
                                   `(,lambda-def ,@params ,body)
                                   (let* ((vars (map cadadr ds))
                                          (vals (map caddr ds))
                                          (applic-args (map (lambda (x) '(const #f)) vars))
                                          (let-sets (map (lambda (var val) `(set (var ,var) ,val)) vars vals))
                                          (let-body (append let-sets es)))
                                      `(,lambda-def ,@params
                                         (applic (lambda-simple ,vars (seq ,let-body)) ,applic-args))))))
                (else (cons (car parsed-expr) (eliminate-nested-defines (cdr parsed-expr)))))))

(define applic-lambda-nil?
   (lambda(e)
   (and (eq? (car e) 'applic)
        (eq? 'lambda-simple (caadr e))
        (null? (cadadr e))
        (null? (caddr e)))))

(define remove-applic-lambda-nil
      (lambda(parsed-expr)
          (cond ((null? parsed-expr) `())
                ((applic-lambda-nil? parsed-expr)
                   (remove-applic-lambda-nil (car (cddadr parsed-expr))))
                ((list? (car parsed-expr))
                   (cons (remove-applic-lambda-nil (car parsed-expr))
                         (remove-applic-lambda-nil (cdr parsed-expr))))
                (else (cons (car parsed-expr) (remove-applic-lambda-nil (cdr parsed-expr)))))))

(define index
     (lambda(var lst)
        (letrec ((index-helper
           (lambda(var lst counter)
             (cond ((null? lst) -1)
                   ((eq? (car lst) var) counter)
                   (else (index-helper var (cdr lst) (+ counter 1)))))))
            (index-helper var lst 0))))

(define find-var-in-acc
    (lambda(var acc)
      (find-var-in-acc-helper var acc 0)))

(define find-var-in-acc-helper
    (lambda(var acc counter)
       (if (null? acc)
          `(fvar ,var)
          (let ((minor (index var (car acc))))
             (cond ((eq? minor -1) (find-var-in-acc-helper var (cdr acc) (+ counter 1)))
                   (else (if (eq? counter 0)
                      `(pvar ,var ,minor)
                      `(bvar ,var ,(- counter 1) ,minor))))))))

(define pe->lex-pe
      (lambda(parsed-expr)
          (pe->lex-pe-helper parsed-expr '())))

(define pe->lex-pe-helper
      (lambda(parsed-expr acc)
          (cond ((or (not (list? parsed-expr)) (null? parsed-expr)) parsed-expr)
                ((list? (car parsed-expr))
                  (cons (pe->lex-pe-helper (car parsed-expr) acc) (pe->lex-pe-helper (cdr parsed-expr) acc)))
                ((is-lambda? (car parsed-expr))
                   (let* ((lambda-def (lambda-get-def parsed-expr))
                          (params (lambda-get-params parsed-expr))
                          (listed-params (lambda-get-listed-params parsed-expr))
                          (body (lambda-get-body parsed-expr)))
                       `(,lambda-def ,@params ,(pe->lex-pe-helper body (cons listed-params acc)))))
                ((eq? (car parsed-expr) 'var) (find-var-in-acc (cadr parsed-expr) acc))
                (else (cons (car parsed-expr) (pe->lex-pe-helper (cdr parsed-expr) acc))))))

(define last
   (lambda(lst)
      (if (eq? (length lst) 1)
         (car lst)
         (last (cdr lst)))))

(define replace-last
   (lambda(lst var)
      (if (eq? (length lst) 1)
         (list var)
         (cons (car lst) (replace-last (cdr lst) var)))))

(define annotate-last-elem
    (lambda(elem)
       (cond ((not (list? elem)) elem)
             ((eq? (car elem) 'applic) (cons 'tc-applic (cdr elem)))
             ((eq? (car elem) 'if3)
                (let ((condition (cadr elem))
                      (dit (caddr elem))
                      (dif (cadddr elem)))
                    `(if3  ,condition ,(annotate-last-elem dit) ,(annotate-last-elem dif))))
             ((member (car elem) '(or seq))
                (let* ((operator (car elem))
                       (body (cadr elem))
                       (new-last (annotate-last-elem (last body)))
                       (new-body (replace-last body new-last)))
                   `(,operator ,new-body)))
             (else elem))))

(define annotate-tc
      (lambda(parsed-expr)
          (cond ((or (not (list? parsed-expr)) (null? parsed-expr)) parsed-expr)
                ((list? (car parsed-expr))
                  (cons (annotate-tc (car parsed-expr)) (annotate-tc (cdr parsed-expr))))
                ((is-lambda? (car parsed-expr))
                    (let* ((lambda-def (lambda-get-def parsed-expr))
                           (params (lambda-get-params parsed-expr))
                           (body (annotate-tc (lambda-get-body parsed-expr)))
                           (new-body (annotate-last-elem body)))
                        `(,lambda-def ,@params ,new-body)))
                (else (cons (car parsed-expr) (annotate-tc (cdr parsed-expr)))))))

(define box-set
   (lambda(parsed-expr)
          (cond ((or (not (list? parsed-expr)) (null? parsed-expr)) parsed-expr)
                ((list? (car parsed-expr))
                  (cons (box-set (car parsed-expr)) (box-set (cdr parsed-expr))))
                ((is-lambda? (car parsed-expr))
                    (box-set-helper parsed-expr))
                (else (cons (car parsed-expr) (box-set (cdr parsed-expr)))))))

(define fix-order
      (lambda (lst order)
           (fold-right (lambda (mem acc) (if (member mem lst) (cons mem acc) acc)) (list) order)))

(define box-set-helper
   (lambda(parsed-expr)
          (let* ((lambda-def (lambda-get-def parsed-expr))
                 (params (lambda-get-params parsed-expr))
                 (body (box-set (lambda-get-body parsed-expr)))
                 (listed-params (lambda-get-listed-params parsed-expr))
                 (set-vars (remove-duplicates (find-set-vars body '())))
                 (get-vars (remove-duplicates (find-get-vars body (list listed-params) '())))
                 (bound-vars (remove-duplicates (find-bound-vars body (list listed-params) '())))
                 (params-in-vars (map (lambda (x) `(var ,x)) listed-params))
                 (vars-to-fix (fix-order (member-in-three set-vars get-vars bound-vars) params-in-vars)))
              (if (null? vars-to-fix)
                  `(,lambda-def ,@params ,body)
                  (let* ((set-exprs (map (lambda (x) `(set ,x (box ,x))) vars-to-fix))
                         (fixed-body (box-set-vars body vars-to-fix))
                         (new-body (if (eq? (car fixed-body) 'seq)
                            `(seq ,(append set-exprs (cadr fixed-body)))
                            `(seq ,(append set-exprs (list fixed-body))))))
                     `(,lambda-def ,@params ,new-body))))))


(define find-set-vars
   (lambda(parsed-expr acc)
          (cond ((or (not (list? parsed-expr)) (null? parsed-expr)) acc)
                ((list? (car parsed-expr))
                  (append (find-set-vars (car parsed-expr) acc) (find-set-vars (cdr parsed-expr) acc)))
                ((is-lambda? (car parsed-expr))
                   (let* ((lambda-def (lambda-get-def parsed-expr))
                          (listed-params (lambda-get-listed-params parsed-expr))
                          (body (lambda-get-body parsed-expr))
                          (var-listed-params (map (lambda(x) `(var ,x)) listed-params))
                          (res (find-set-vars body acc)))
                       (append (remove-from-list res var-listed-params) acc)))
                ((is-set? parsed-expr)
                    (cons (cadr parsed-expr) (find-set-vars (caddr parsed-expr) acc)))
                (else (find-set-vars (cdr parsed-expr) acc)))))

(define find-get-vars
   (lambda(parsed-expr params-acc acc)
          (cond ((or (not (list? parsed-expr)) (null? parsed-expr)) acc)
                ((list? (car parsed-expr))
                  (append (find-get-vars (car parsed-expr) params-acc acc) (find-get-vars (cdr parsed-expr) params-acc acc)))
                ((is-lambda? (car parsed-expr))
                   (let* ((lambda-def (lambda-get-def parsed-expr))
                          (listed-params (lambda-get-listed-params parsed-expr))
                          (body (lambda-get-body parsed-expr))
                          (var-listed-params (map (lambda(x) `(var ,x)) listed-params))
                          (res (find-get-vars body (cons listed-params params-acc) acc)))
                       (append (remove-from-list res var-listed-params) acc)))
                ((is-set? parsed-expr)
                   (append (find-get-vars (cddr parsed-expr) params-acc acc) acc))
                ((eq? (car parsed-expr) 'var)
                   (let ((tagged-var (find-var-in-acc (cadr parsed-expr) params-acc)))
                       (if (or (eq? (car tagged-var) 'bvar) (eq? (car tagged-var) 'pvar))
                           (cons parsed-expr acc)
                           acc)))
                (else (find-get-vars (cdr parsed-expr) params-acc acc)))))

(define find-bound-vars
   (lambda(parsed-expr params-acc acc)
          (cond ((or (not (list? parsed-expr)) (null? parsed-expr)) acc)
                ((list? (car parsed-expr))
                  (append (find-bound-vars (car parsed-expr) params-acc acc) (find-bound-vars (cdr parsed-expr) params-acc acc)))
                ((is-lambda? (car parsed-expr))
                   (let* ((lambda-def (lambda-get-def parsed-expr))
                          (listed-params (lambda-get-listed-params parsed-expr))
                          (body (lambda-get-body parsed-expr))
                          (var-listed-params (map (lambda(x) `(var ,x)) listed-params))
                          (res (find-bound-vars body (cons listed-params params-acc) acc)))
                       (append (remove-from-list res var-listed-params) acc)))
                ((eq? (car parsed-expr) 'var)
                   (let ((tagged-var (find-var-in-acc (cadr parsed-expr) params-acc)))
                       (if (eq? (car tagged-var) 'bvar)
                           (cons parsed-expr acc)
                           acc)))
                (else (find-bound-vars (cdr parsed-expr) params-acc acc)))))

(define member-in-three
    (lambda (l1 l2 l3)
        (fold-right (lambda (mem acc) (if (and (member mem l2) (member mem l3)) (cons mem acc) acc)) (list) l1)))

(define remove-from-list
    (lambda (lst vars-to-remove)
        (cond ((null? lst) lst)
              ((member (car lst) vars-to-remove) (remove-from-list (cdr lst) vars-to-remove))
              (else (cons (car lst) (remove-from-list (cdr lst) vars-to-remove))))))

(define box-set-vars
    (lambda(parsed-expr vars-to-fix)
         (cond ((or (not (list? parsed-expr)) (null? parsed-expr)) parsed-expr)
                ((list? (car parsed-expr))
                  (cons (box-set-vars (car parsed-expr) vars-to-fix) (box-set-vars (cdr parsed-expr) vars-to-fix)))
                ((and (is-set? parsed-expr) (member (cadr parsed-expr) vars-to-fix))
                  `(box-set ,(cadr parsed-expr) ,@(box-set-vars (cddr parsed-expr) vars-to-fix)))
                ((and (eq? (car parsed-expr) 'var) (member parsed-expr vars-to-fix))
                  `(box-get ,parsed-expr))
                ((is-lambda? (car parsed-expr))
                   (let ((lambda-def (lambda-get-def parsed-expr))
                         (body (lambda-get-body parsed-expr))
                         (params (lambda-get-params parsed-expr))
                         (listed-params (map (lambda(x) `(var ,x)) (lambda-get-listed-params parsed-expr))))
                   `(,lambda-def ,@params ,(box-set-vars body (remove-from-list vars-to-fix listed-params)))))
                (else (cons (car parsed-expr) (box-set-vars (cdr parsed-expr) vars-to-fix))))))

(define file->string
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
                 (list->string (run))))))

(define full-parse
     (lambda(exp)
        (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines (parse exp))))))))

(define nl "\n")

(define const-nil-register "R15")
(define const-void-register "R14")
(define const-true-register "R13")
(define const-false-register "R12")
(define const-table-register "R11")
(define global-table-register "R10")
(define symbol-string-list-register "R9")

(define compile-scheme-file
    (lambda(scheme-path target-path)
          (let* ((txt-string (file->string scheme-path))
                 (expr-list (read-expr-list txt-string))
                 (parsed-expr-list (map full-parse expr-list))
                 (const-table (make-const-table parsed-expr-list))
                 (global-table (make-global-table parsed-expr-list))
                 (prologue (create-prologue const-table global-table))
                 (code-txt-parts (map (lambda(x) (string-append (code-gen x const-table global-table 0) print-r0)) parsed-expr-list))
                 (code (apply string-append code-txt-parts))
                 (final-code (string-append prologue code epilogue)))
                   (delete-file target-path)
                   (let ((output-file (open-output-file target-path)))
                      (display final-code output-file)
                      (close-output-port output-file)))))

(define fraction?
    (lambda(n) (and (not (integer? n)) (rational? n))))

(define newline?
    (lambda(n) (equal? n #\newline)))

(define newline-str "#\\newline")

(print-gensym #f)

(define read-expr-list
    (lambda(txt-string)
        (letrec ((helper (lambda(txt-string acc)
            (if (eq? (string-length txt-string) 0)
                acc
                (let* ((res (test-string <sexpr> txt-string))
                       (rem (cadadr res))
                       (match (cadar res)))
                    (helper rem (cons match acc)))))))
                (reverse (helper txt-string '())))))

(define make-const-table
     (lambda(parsed-expr-list)
         (let ((const-table (box `((0 ,(void) ("T_VOID"))
                                   (1 () ("T_NIL"))
                                   (2 #f ("T_BOOL" "0"))
                                   (4 #t ("T_BOOL" "1"))
                                   (6 0 ("T_INTEGER" "0" "1"))
                                   (9 1 ("T_INTEGER" "1" "1"))
                                   (12 2 ("T_INTEGER" "2" "1"))
                                   (15 "a" ("T_STRING" "1" ,(number->string (char->integer #\a))))
                                   (18 'a ("T_SYMBOL" "16"))))))
             (make-const-table-helper parsed-expr-list const-table)
             (unbox const-table))))

(define member-const-table
     (lambda(val const-table)
         (cond ((null? const-table) #f)
               ((equal? (cadr (car const-table)) val) (caar const-table))
               (else (member-const-table val (cdr const-table))))))

(define const-table-new-address
     (lambda(const-table)
         (let ((last (last-elem const-table)))
             (+ (car last) (length (caddr last))))))

(define const-table-debug
     (lambda(const-table)
         (fold-left (lambda (x y)
             (let ((addr (car y))
                   (repr (caddr y))
                   (counter (box 0)))
                (string-append x (fold-left (lambda(a b)
                   (let ((res (string-append "[" (number->string (+ addr (unbox counter))) "]\t" b nl)))
                      (set-box! counter (+ (unbox counter) 1))
                      (string-append a res))) "" repr)))) "" const-table)))

(define make-const-table-helper
     (lambda(parsed-expr-list const-table)
         (cond ((null? parsed-expr-list) '())
               ((null? (car parsed-expr-list)) (make-const-table-helper (cdr parsed-expr-list) const-table))
               ((list? (car parsed-expr-list))
                     (make-const-table-helper (car parsed-expr-list) const-table)
                     (make-const-table-helper (cdr parsed-expr-list) const-table))
               ((eq? (car parsed-expr-list) 'const)
                   (let ((const-table-unboxed (unbox const-table)))
                      (set-box! const-table (make-const-table-single (cadr parsed-expr-list) const-table-unboxed))))
               (else (make-const-table-helper (cdr parsed-expr-list) const-table)))))

(define make-const-table-single
     (lambda(val const-table)
         (letrec ((new-addr (const-table-new-address const-table))
                  (string-repr (lambda(str)
                     (map (lambda(x) (number->string (char->integer x))) (string->list str)))))
            (cond ((member-const-table val const-table) const-table)
                  ((number? val) (append const-table `((,new-addr ,val
                    ("T_INTEGER" ,(number->string (numerator val)) ,(number->string (denominator val)))))))
                  ((newline? val) (append const-table `((,new-addr ,val
                    ("T_CHAR" "'\\n'")))))
                  ((char? val) (append const-table `((,new-addr ,val ("T_CHAR"
                     ,(string-append (number->string (char->integer val))))))))
                  ((string? val) (append const-table `((,new-addr ,val ("T_STRING"
                      ,(number->string (length (string->list val)))
                      ,@(string-repr val))))))
                  ((symbol? val) (make-const-table-symbol val const-table))
                  ((pair? val) (make-const-table-pair val const-table))
                  ((vector? val) (make-const-table-vector val const-table))
                  (else const-table)))))

(define make-const-table-symbol
     (lambda(val const-table)
          (let* ((str (symbol->string val))
                 (new-table (make-const-table-single str const-table))
                 (new-addr (const-table-new-address new-table))
                 (str-addr (+ (member-const-table str new-table) 1)))
              (append new-table `((,new-addr ,val ("T_SYMBOL" ,(number->string str-addr))))))))

(define make-const-table-pair
     (lambda(val const-table)
         (if (pair? val)
            (let* ((first-table (make-const-table-single (car val) const-table))
                   (second-table (make-const-table-single (cdr val) first-table))
                   (new-addr (const-table-new-address second-table))
                   (car-addr (+ (member-const-table (car val) second-table) 1))
                   (cdr-addr (+ (member-const-table (cdr val) second-table) 1)))
                (append second-table `((,new-addr ,val
                   ("T_PAIR" ,(number->string car-addr) ,(number->string cdr-addr))))))
            (make-const-table-single val const-table))))

(define make-const-table-vector
     (lambda(val const-table)
         (let ((boxed-const-table (box const-table))
               (lst (vector->list val)))
             (map (lambda(x) (set-box! boxed-const-table (make-const-table-single x (unbox boxed-const-table))))
                 lst)
             (let* ((new-const-table (unbox boxed-const-table))
                    (new-addr (const-table-new-address new-const-table))
                    (repr-start `("T_VECTOR" ,(number->string (length lst)))))
                 (append new-const-table `((,new-addr ,val
                     ,(append repr-start (map (lambda(x) (number->string (+ (member-const-table x new-const-table) 1))) lst)))))))))

(define make-const-table-mov-instructions
     (lambda (table)
        (let ((counter (box 0)))
           (fold-left (lambda(x y)
              (let ((repr (caddr y)))
                (string-append x
                  (fold-left (lambda(a b)
                     (let ((res (string-append "MOV(INDD(CONST_TABLE, " (number->string (unbox counter))
                             "), IMM(" b "));" nl)))
                         (set-box! counter (+ (unbox counter) 1))
                         (string-append a res)))
                     "" repr))))
                         "" table))))

(define make-const-table-symbol-str-list
     (lambda (table)
         (let* ((symbols (filter (lambda(x) (equal? "T_SYMBOL" (car (caddr x)))) table))
                (num-symbols (length symbols))
                (symbol-list-size (* num-symbols 2)))
             (string-append
                   "PUSH(IMM(" (number->string symbol-list-size) "));" nl
                   "CALL(MALLOC);" nl
                   "DROP(1);" nl
                   "MOV(SYMBOL_STRING_LIST, IMM(R0));" nl
                   (let ((counter (box 0)))
                      (fold-left (lambda(text sym)
                         (let ((res (string-append
                                 text
                                 "//create_symbol_tableeeeeeeeee" nl
                                 "MOV(INDD(SYMBOL_STRING_LIST, " (number->string (unbox counter)) "), "
                                    (cadr (caddr sym)) ");" nl
                                 "MOV(INDD(SYMBOL_STRING_LIST, " (number->string (+ (unbox counter) 1)) "),"
                                    (if (>= (+ (unbox counter) 2) symbol-list-size)
                                       "0"
                                        (string-append "R9 + " (number->string (+ (unbox counter) 2))))
                                    ");" nl)))
                              (set-box! counter (+ (unbox counter) 2))
                              res)) "" symbols))))))

































(define make-global-table
     (lambda(parsed-expr-list)
       (let ((global-table (box `((0 eq? ,make-eq?)
                                  (1 zero? ,make-zero?)
                                  (2 not ,make-not)
                                  (3 car ,make-car)
                                  (4 cdr ,make-cdr)
                                  (5 cons ,make-cons)
                                  (6 cadr ,make-cadr)
                                  (7 - ,make-minus)
                                  (8 + ,make-plus)
                                  (9 integer? ,make-integer?)
                                  (10 boolean? ,make-boolean?)
                                  (11 char? ,make-char?)
                                  (12 procedure? ,make-procedure?)
                                  (13 pair? ,make-pair?)
                                  (14 symbol? ,make-symbol?)
                                  (15 string? ,make-string?)
                                  (16 list? ,make-list?)
                                  (17 vector? ,make-vector?)
                                  (18 null? ,make-null?)
                                  (19 integer->char ,make-integer->char)
                                  (20 char->integer ,make-char->integer)
                                  (21 box ,make-box)
                                  (22 unbox ,make-unbox)
                                  (23 = ,make-num-eq)
                                  (24 > ,make-gt)
                                  (25 < ,make-lt)
                                  (26 map ,make-map)
                                  (27 list ,make-clist)
                                  (28 length ,make-length)
                                  (29 reverse ,make-reverse)
                                  (30 apply ,make-apply)
                                  (31 append-binary ,make-append-binary)
                                  (32 append ,make-append)
                                  (33 make-string ,make-make-string)
                                  (34 string-length ,make-string-length)
                                  (35 string-ref ,make-string-ref)
                                  (36 string-set! ,make-string-set!)
                                  (37 vector ,make-cvector)
                                  (38 make-vector ,make-make-vector)
                                  (39 vector-length ,make-vector-length)
                                  (40 vector-ref ,make-vector-ref)
                                  (41 vector-set! ,make-vector-set!)
                                  (42 * ,make-mult)
                                  (43 / ,make-div)
                                  (44 denominator ,make-denominator)
                                  (45 numerator ,make-numerator)
                                  (46 remainder ,make-remainder)
                                  (47 number? ,make-number?)
                                  (48 rational? ,make-rational?)
                                  (49 symbol->string ,make-symbol->string)
                                  (50 string->symbol ,make-string->symbol)
                                  (51 set-car! ,make-set-car!)
                                  (52 set-cdr! ,make-set-cdr!)))))
             (make-global-table-helper parsed-expr-list global-table)
             (unbox global-table))))

(define member-global-table member-const-table)

(define global-table-new-address
     (lambda(global-table)
         (length global-table)))

(define make-global-table-mov-instructions
     (lambda(const-table global-table)
        (let ((counter (box 0)))
           (fold-left (lambda(x y)
              (let* ((repr (caddr y))
                     (res (if (equal? repr "T_UNDEFINED")
                              (string-append "MOV(INDD(GLOBAL_TABLE, " (number->string (unbox counter))
                                   "), IMM(" repr "));" nl)
                              (string-append
                                  (repr const-table global-table)
                                  "MOV(INDD(GLOBAL_TABLE, " (number->string (unbox counter))
                                   "), IMM(R0));" nl))))
                     (set-box! counter (+ (unbox counter) 1))
                     (string-append x res)))
                     "" global-table))))


(define make-global-table-helper
    (lambda(parsed-expr-list global-table)
        (cond ((null? parsed-expr-list) '())
              ((null? (car parsed-expr-list)) (make-global-table-helper (cdr parsed-expr-list) global-table))
              ((list? (car parsed-expr-list))
                     (make-global-table-helper (car parsed-expr-list) global-table)
                     (make-global-table-helper (cdr parsed-expr-list) global-table))
              ((eq? (car parsed-expr-list) 'fvar)
                   (let ((global-table-unboxed (unbox global-table)))
                      (set-box! global-table (make-global-table-single (cadr parsed-expr-list) global-table-unboxed))))
              (else (make-global-table-helper (cdr parsed-expr-list) global-table)))))

(define make-global-table-single
    (lambda(var global-table)
        (if (member-global-table var global-table)
            global-table
            (append global-table `((,(global-table-new-address global-table) ,var "T_UNDEFINED"))))))

(define func-prologue
   (string-append
     "PUSH(FP);" nl
     "MOV(FP, SP);" nl
     ))

(define func-epilogue
   (string-append
      "POP(FP);" nl
      "RETURN;" nl
      ))

(define range
  (lambda (n . m)
    (let
      ((n (if (null? m) 0 n)) (m (if (null? m) n (car m))))
      (cond
    ((= n m) (list n))
    (else (cons n (range ((if (< n m) + -) n 1) m)))))))

(define code-gen-error
    (lambda (label-name txt)
        (let ((txt-lst (string->list (string-append "MAYER-EXCEPTION: " txt))))
          (string-append
             label-name ":" nl
             (apply string-append (map (lambda(x) (string-append "PUSH(IMM('" (string x) "')); ")) txt-lst))
             "PUSH(IMM(" (number->string (length txt-lst)) "));" nl
             "CALL(MAKE_SOB_STRING);" nl
             "DROP(" (number->string (+ (length txt-lst) 1)) ");" nl
             "PUSH(IMM(R0));" nl
             "CALL(WRITELN);" nl
             "DROP(1)" nl
             "JUMP(EXIT);" nl
             ))))

(define code-gen-make-closure
    (lambda(label env-name)
        (string-append
            "// Create closure for " label nl
            "PUSH(LABEL(" label "));" nl
            "PUSH(IMM(" env-name "));" nl
            "CALL(MAKE_SOB_CLOSURE);" nl
            "DROP(2);" nl
            )))

(define code-gen-check-lambda-params
    (lambda(num-params)
        (string-append
           "CMP(FPARG(1), IMM(" (number->string num-params) "));" nl
           "JUMP_NE(L_err_lambda_args_count);" nl
           )))

(define make-primitive-from-scheme
    (lambda(label env-name scheme-code const-table global-table)
        (let* ((parsed-code (full-parse scheme-code))
               (lambda-def (lambda-get-def parsed-code))
               (body-code (lambda-get-body parsed-code))
               (params (lambda-get-listed-params parsed-code))
               (num-params (if (eq? (car parsed-code) 'lambda-simple)
                  (length params)
                  #f))
               (code (create-lambda-body lambda-def params body-code const-table global-table 0)))
            (make-primitive-from-code label env-name num-params code const-table global-table))))

(define make-primitive-from-code
    (lambda(label env-name num-params code const-table global-table)
        (let ((make-closure-label (label-gen "L_make_closure")))
            (string-append
               "JUMP(" make-closure-label ");" nl
               label ":" nl
               func-prologue
               (if num-params
                  (code-gen-check-lambda-params num-params)
                  "")
               code
               func-epilogue
               make-closure-label ":" nl
               (code-gen-make-closure label env-name)))))

(define make-eq?
    (lambda(const-table global-table)
       (let ((code (string-append
          "MOV(R1, FPARG(2));" nl
          "MOV(R2, FPARG(3));" nl
          "MOV(R0, IMM(SOB_FALSE));" nl
          "CMP(IND(R1), T_SYMBOL);" nl
          "JUMP_NE(EQ_NOT_SYMBOL);" nl
          "CMP(IND(R2), T_SYMBOL);" nl
          "JUMP_NE(L_eq_exit);" nl
          "MOV(R1, INDD(R1, 1));" nl
          "MOV(R2, INDD(R2, 1));" nl
          "JUMP(EQ_NOT_INTEGER);" nl
          "EQ_NOT_SYMBOL:" nl
          "CMP(IND(R1), T_INTEGER);" nl
          "JUMP_NE(EQ_NOT_INTEGER);" nl
          "CMP(IND(R2), T_INTEGER);" nl
          "JUMP_NE(L_eq_exit);" nl
          "CMP(INDD(R1, 1), INDD(R2, 1));" nl
          "JUMP_NE(L_eq_exit);" nl
          "CMP(INDD(R1, 2), INDD(R2, 2));" nl
          "JUMP_NE(L_eq_exit);" nl
          "JUMP(L_eq_true);" nl
          "EQ_NOT_INTEGER:" nl
          "CMP(IMM(R1), IMM(R2));" nl
          "JUMP_NE(L_eq_exit);" nl
          "L_eq_true:" nl
          "MOV(R0, IMM(SOB_TRUE));" nl
          "L_eq_exit:" nl)))
         (make-primitive-from-code "L_eq" "E_EQ" 2 code const-table global-table))))

(define make-car
    (lambda(const-table global-table)
       (let ((code (string-append
          "MOV(R1, FPARG(2));" nl
          "CMP(INDD(R1, 0), T_PAIR);" nl
          "JUMP_NE(L_err_invalid_param);" nl
          "MOV(R0, INDD(R1, 1));" nl)))
         (make-primitive-from-code "L_car" "E_CAR" 1 code const-table global-table))))

(define make-cdr
    (lambda(const-table global-table)
       (let ((code (string-append
          "MOV(R1, FPARG(2));" nl
          "CMP(INDD(R1, 0), T_PAIR);" nl
          "JUMP_NE(L_err_invalid_param);" nl
          "MOV(R0, INDD(R1, 2));" nl)))
         (make-primitive-from-code "L_cdr" "E_CDR" 1 code const-table global-table))))

(define make-cons
    (lambda(const-table global-table)
       (let ((code (string-append
          "PUSH(FPARG(3));" nl
          "PUSH(FPARG(2));" nl
          "CALL(MAKE_SOB_PAIR);" nl
          "DROP(2);" nl)))
         (make-primitive-from-code "L_cons" "E_CONS" 2 code const-table global-table))))

(define make-zero?
    (lambda(const-table global-table)
        (make-primitive-from-scheme "L_zero" "E_ZERO" '(lambda(x) (and (integer? x) (= x 0))) const-table global-table)))

(define make-null?
    (lambda(const-table global-table)
        (make-primitive-from-scheme "L_pnull" "E_PNULL" '(lambda(x) (eq? x '())) const-table global-table)))

(define make-list?
    (lambda(const-table global-table)
       (let ((code '(lambda(lst)
          (if (pair? lst)
             (list? (cdr lst))
             (null? lst)))
             ))
        (make-primitive-from-scheme "L_plist" "E_PLIST" code const-table global-table))))

(define make-map
    (lambda(const-table global-table)
       (let* ((code '(lambda (f lst)
   (if (null? lst)
       '()
       (cons (f (car lst)) (map f (cdr lst)))))))
	  (make-primitive-from-scheme "L_map" "E_MAP" code const-table global-table))))


(define make-clist
    (lambda(const-table global-table)
        (make-primitive-from-scheme "L_list" "E_LIST" '(lambda v v) const-table global-table)))

(define make-not
    (lambda(const-table global-table)
        (make-primitive-from-scheme "L_not" "E_NOT" '(lambda(x) (eq? x #f)) const-table global-table)))

(define make-plus
    (lambda(const-table global-table)
       (let ((code (string-append
            "MOV(R3, FPARG(1)); // Num of params" nl
            "MOV(R0, IMM(0));" nl
            "MOV(R1, IMM(1));" nl
            "MOV(R2, IMM(FP));" nl
            "SUB(R2, IMM(5));" nl
            "PLUS_LOOP:" nl
            "CMP(R3, IMM(0));" nl
            "JUMP_LE(PLUS_EXIT);" nl
            "MOV(R4, STACK(R2));" nl
            "PUSH(IMM(R4));" nl
            "PUSH(IMM(R1));" nl
            "PUSH(IMM(R0));" nl
            "CALL(SUM);" nl
            "DROP(3);" nl
            "DECR(R2);" nl
            "DECR(R3);" nl
            "JUMP(PLUS_LOOP);" nl
            "PLUS_EXIT:" nl
            "PUSH(IMM(R0));" nl
            "PUSH(IMM(R1));" nl
            "CALL(MAKE_SOB_INTEGER);" nl
            "DROP(2);" nl)))
         (make-primitive-from-code "L_PLUS" "E_PLUS" #f code const-table global-table))))

(define make-int-to-bool
    (lambda(const-table global-table)
        (let ((code (string-append
            "MOV(R1, FPARG(0));" nl
            "MOV(R0, IMM(SOB_TRUE));" nl
            "CMP(R1, IMM(1));" nl
            "JUMP_EQ(INT_TO_BOOL_EXIT);" nl
            "MOV(R0, IMM(SOB_FALSE));" nl
            "INT_TO_BOOL_EXIT:" nl)))
           (make-primitive-from-code "L_int_to_bool" "E_INT_TO_BOOL" #f code const-table global-table))))

(define make-minus
    (lambda(const-table global-table)
       (let ((code (string-append
            "MOV(R3, FPARG(1)); // Num of params" nl
            "CMP(R3, 0);" nl
            "JUMP_EQ(L_err_lambda_args_count);" nl
            "CMP(R3, 1);" nl
            "JUMP_NE(MINUS_CONTINUE);" nl
            "MOV(R0, IMM(0));" nl
            "MOV(R1, IMM(1));" nl
            "PUSH(FPARG(2));" nl
            "PUSH(IMM(R1));" nl
            "PUSH(IMM(R0));" nl
            "CALL(SUBSTRACT);" nl
            "DROP(3);" nl
            "JUMP(MINUS_EXIT);" nl
            "MINUS_CONTINUE:" nl
            "MOV(R2, IMM(FP));" nl
            "SUB(R2, IMM(5));" nl
            "MOV(R4, STACK(R2));" nl
            "MOV(R0, INDD(R4, 1));" nl
            "MOV(R1, INDD(R4, 2));" nl
            "DECR(R3);" nl
            "DECR(R2);" nl
            "MINUS_LOOP:" nl
            "CMP(R3, IMM(0));" nl
            "JUMP_LE(MINUS_EXIT);" nl
            "MOV(R4, STACK(R2));" nl
            "PUSH(IMM(R4));" nl
            "PUSH(IMM(R1));" nl
            "PUSH(IMM(R0));" nl
            "CALL(SUBSTRACT);" nl
            "DROP(3);" nl
            "DECR(R2);" nl
            "DECR(R3);" nl
            "JUMP(MINUS_LOOP);" nl
            "MINUS_EXIT:" nl
            "PUSH(IMM(R0));" nl
            "PUSH(IMM(R1));" nl
            "CALL(MAKE_SOB_INTEGER);" nl
            "DROP(2);" nl)))
         (make-primitive-from-code "L_MINUS" "E_MINUS" #f code const-table global-table))))

(define make-mult
    (lambda(const-table global-table)
       (let ((code (string-append
            "MOV(R3, FPARG(1)); // Num of params" nl
            "MOV(R0, IMM(1));" nl
            "MOV(R1, IMM(1));" nl
            "MOV(R2, IMM(FP));" nl
            "SUB(R2, IMM(5));" nl
            "MULT_LOOP:" nl
            "CMP(R3, IMM(0));" nl
            "JUMP_LE(MULT_EXIT);" nl
            "MOV(R4, STACK(R2));" nl
            "PUSH(IMM(R4));" nl
            "PUSH(IMM(R1));" nl
            "PUSH(IMM(R0));" nl
            "CALL(MULT);" nl
            "DROP(3);" nl
            "DECR(R2);" nl
            "DECR(R3);" nl
            "JUMP(MULT_LOOP);" nl
            "MULT_EXIT:" nl
            "PUSH(IMM(R0));" nl
            "PUSH(IMM(R1));" nl
            "CALL(MAKE_SOB_INTEGER);" nl
            "DROP(2);" nl)))
         (make-primitive-from-code "L_MULT" "E_MULT" #f code const-table global-table))))

(define make-div
    (lambda(const-table global-table)
       (let ((code (string-append
            "MOV(R3, FPARG(1)); // Num of params" nl
            "CMP(R3, IMM(0));" nl
            "JUMP_EQ(L_err_lambda_args_count);" nl
            "CMP(R3, IMM(1));" nl
            "JUMP_NE(DIV_CONTINUE);" nl
            "// Switch denominator and numerator" nl
            "MOV(R0, FPARG(2));" nl
            "PUSH(INDD(R0, 2));" nl
            "PUSH(INDD(R0, 1));" nl
            "CALL(REDUCT);" nl
            "DROP(2);" nl
            "JUMP(DIV_EXIT);" nl
            "DIV_CONTINUE:" nl
            "MOV(R0, FPARG(2));" nl
            "MOV(R1, INDD(R0, 2));" nl
            "MOV(R0, INDD(R0, 1));" nl
            "MOV(R2, IMM(FP));" nl
            "SUB(R2, IMM(6));" nl
            "DECR(R3);" nl
            "DIV_LOOP:" nl
            "MOV(R4, STACK(R2));" nl
            "PUSH(IMM(R4));" nl
            "PUSH(IMM(R1));" nl
            "PUSH(IMM(R0));" nl
            "CALL(DIVI);" nl
            "DROP(3);" nl
            "DECR(R2);" nl
            "DECR(R3);" nl
            "CMP(R3, IMM(0));" nl
            "JUMP_LE(DIV_EXIT);" nl
            "JUMP(DIV_LOOP);" nl
            "DIV_EXIT:" nl
            "PUSH(IMM(R0));" nl
            "PUSH(IMM(R1));" nl
            "CALL(MAKE_SOB_INTEGER);" nl
            "DROP(2);" nl)))
         (make-primitive-from-code "L_DIV" "E_DIV" #f code const-table global-table))))

; = primitive
(define make-num-eq
    (lambda(const-table global-table)
       (let ((code (string-append
            "MOV(R1, FPARG(1)); // Num of params" nl
            "CMP(R1, 0);" nl
            "JUMP_EQ(L_err_lambda_args_count);" nl
            "MOV(R2, IMM(FP));" nl
            "SUB(R2, IMM(5));" nl
            "MOV(R3, STACK(R2));" nl
            "MOV(R0, IMM(SOB_TRUE));" nl
            "NUM_EQ_LOOP:" nl
            "CMP(R1, IMM(0));" nl
            "JUMP_LE(NUM_EQ_EXIT);" nl
            "MOV(R4, STACK(R2));" nl
            "CMP(INDD(R4,0), T_INTEGER);" nl
            "JUMP_NE(L_err_invalid_param);" nl
            "CMP(INDD(R4,1), INDD(R3, 1));" nl
            "JUMP_NE(NUM_EQ_NOT_EQ);" nl
            "CMP(INDD(R4,2), INDD(R3, 2));" nl
            "JUMP_NE(NUM_EQ_NOT_EQ);" nl
            "DECR(R2);" nl
            "DECR(R1);" nl
            "JUMP(NUM_EQ_LOOP);" nl
            "NUM_EQ_NOT_EQ:" nl
            "MOV(R0, IMM(SOB_FALSE));"
            "JUMP(NUM_EQ_EXIT);" nl
            "NUM_EQ_EXIT:" nl)))
         (make-primitive-from-code "L_NUM_EQ" "E_NUM_EQ" #f code const-table global-table))))

(define make-lt
    (lambda(const-table global-table)
       (let ((code (string-append
            "MOV(R5, FPARG(1)); // Num of params" nl
            "CMP(R5, 0);" nl
            "JUMP_EQ(L_err_lambda_args_count);" nl
            "MOV(R2, IMM(FP));" nl
            "SUB(R2, IMM(5));" nl
            "MOV(R3, STACK(R2));" nl
            "DECR(R2);" nl
            "LT_LOOP:" nl
            "CMP(R5, IMM(1));" nl
            "JUMP_LE(LT_TRUE);" nl
            "MOV(R4, STACK(R2));" nl
            "CMP(INDD(R4,0), T_INTEGER);" nl
            "JUMP_NE(L_err_invalid_param);" nl
            "PUSH(IMM(R4));" nl
            "PUSH(INDD(R3, 2));" nl
            "PUSH(INDD(R3, 1));" nl
            "CALL(SUBSTRACT);" nl
            "DROP(3);" nl
            "CMP(R0, 0);" nl
            "JUMP_GE(LT_FALSE);" nl
            "MOV(R3, STACK(R2));" nl
            "DECR(R2);" nl
            "DECR(R5);" nl
            "JUMP(LT_LOOP);" nl
            "LT_TRUE:" nl
            "MOV(R0, IMM(SOB_TRUE));" nl
            "JUMP(LT_EXIT);" nl
            "LT_FALSE:" nl
            "MOV(R0, IMM(SOB_FALSE));" nl
            "LT_EXIT:" nl)))
         (make-primitive-from-code "L_LT" "E_LT" #f code const-table global-table))))

(define make-gt
    (lambda(const-table global-table)
       (let ((code (string-append
            "MOV(R5, FPARG(1)); // Num of params" nl
            "CMP(R5, 0);" nl
            "JUMP_EQ(L_err_lambda_args_count);" nl
            "MOV(R2, IMM(FP));" nl
            "SUB(R2, IMM(5));" nl
            "MOV(R3, STACK(R2));" nl
            "DECR(R2);" nl
            "GT_LOOP:" nl
            "CMP(R5, IMM(1));" nl
            "JUMP_LE(GT_TRUE);" nl
            "MOV(R4, STACK(R2));" nl
            "CMP(INDD(R4,0), T_INTEGER);" nl
            "JUMP_NE(L_err_invalid_param);" nl
            "PUSH(IMM(R4));" nl
            "PUSH(INDD(R3, 2));" nl
            "PUSH(INDD(R3, 1));" nl
            "CALL(SUBSTRACT);" nl
            "DROP(3);" nl
            "CMP(R0, 0);" nl
            "JUMP_LE(GT_FALSE);" nl
            "MOV(R3, STACK(R2));" nl
            "DECR(R2);" nl
            "DECR(R5);" nl
            "JUMP(GT_LOOP);" nl
            "GT_TRUE:" nl
            "MOV(R0, IMM(SOB_TRUE));" nl
            "JUMP(GT_EXIT);" nl
            "GT_FALSE:" nl
            "MOV(R0, IMM(SOB_FALSE));" nl
            "GT_EXIT:" nl)))
         (make-primitive-from-code "L_GT" "E_GT" #f code const-table global-table))))

(define make-denominator
    (lambda(const-table global-table)
        (let ((code (string-append
            "MOV(R0, FPARG(2));" nl
            "CMP(IND(R0), T_INTEGER);" nl
            "JUMP_NE(L_err_invalid_param);" nl
            "PUSH(INDD(R0, 2));" nl
            "PUSH(IMM(1));" nl
            "CALL(MAKE_SOB_INTEGER);" nl
            "DROP(2);" nl)))
          (make-primitive-from-code "L_DENOMINATOR" "E_PRIVATE" 1 code const-table global-table))))

(define make-remainder
    (lambda(const-table global-table)
        (let ((code (string-append
            "MOV(R0, FPARG(2));" nl
            "MOV(R1, FPARG(3));" nl
            "CMP(IND(R0), T_INTEGER);" nl
            "JUMP_NE(L_err_invalid_param);" nl
            "CMP(IND(R1), T_INTEGER);" nl
            "JUMP_NE(L_err_invalid_param);" nl
            "MOV(R2, INDD(R0, 1));" nl
            "REM(R2, INDD(R1, 1));" nl
            "PUSH(R2);" nl
            "PUSH(1);" nl
            "CALL(MAKE_SOB_INTEGER);" nl
            "DROP(2);" nl)))
          (make-primitive-from-code "L_REMAINDER" "E_PRIVATE" 2 code const-table global-table))))

(define make-numerator
    (lambda(const-table global-table)
        (let ((code (string-append
            "MOV(R0, FPARG(2));" nl
            "CMP(IND(R0), T_INTEGER);" nl
            "JUMP_NE(L_err_invalid_param);" nl
            "PUSH(INDD(R0, 1));" nl
            "PUSH(IMM(1));" nl
            "CALL(MAKE_SOB_INTEGER);" nl
            "DROP(2);" nl)))
          (make-primitive-from-code "L_NUMERATOR" "E_PRIVATE" 1 code const-table global-table))))

(define make-cadr
    (lambda(const-table global-table)
       (make-primitive-from-scheme "L_CADR" "E_CADR" '(lambda(l) (car (cdr l))) const-table global-table)))

(define make-append-binary
    (lambda(const-table global-table)
        (let ((code
           '(lambda(l m)
              (if (null? l)
                m
                (cons (car l) (append-binary (cdr l) m))))))
           (make-primitive-from-scheme "L_APPEND_BINARY" "E_PRIVATE" code const-table global-table))))

(define make-append
    (lambda(const-table global-table)
        (let ((code
           '(lambda v
              (cond ((null? v) v)
                    ((null? (cdr v)) (car v))
                    ((= (length v) 2) (append-binary (car v) (cadr v)))
                    (else (let ((rest (cdr (cdr v)))
                                (first (append-binary (car v) (cadr v))))
                       (apply append (cons first rest))))))))
           (make-primitive-from-scheme "L_APPEND" "E_APPEND" code const-table global-table))))

(define make-length
   (lambda(const-table global-table)
       (let ((code '(lambda(lst)
           (letrec ((helper (lambda(ls counter)
              (if (null? ls)
                 counter
                 (helper (cdr ls) (+ counter 1))))))
               (helper lst 0)))))
           (make-primitive-from-scheme "L_LENGTH" "E_LENGTH" code const-table global-table))))

(define make-make-string
   (lambda(const-table global-table)
       (let ((code (string-append
           "MOV(R0, FPARG(1));" nl
           "CMP(R0, 3);" nl
           "JUMP_GE(L_err_lambda_args_count);" nl
           "// Save the char in R1" nl
           "CMP(R0, 2);" nl
           "JUMP_EQ(L_make_string_char_received);" nl
           "MOV(R1, IMM(0));" nl
           "JUMP(L_make_string_continue);" nl
           "L_make_string_char_received:" nl
           "MOV(R0, FPARG(3));" nl
           "MOV(R1, INDD(R0, 1));" nl
           "L_make_string_continue:" nl
           "MOV(R2, FPARG(2));" nl
           "MOV(R2, INDD(R2, 1));" nl
           "MOV(R6, R2);" nl
           "L_make_string_loop:" nl
           "CMP(R2, 0);" nl
           "JUMP_LE(L_make_string_loop_exit);" nl
           "PUSH(IMM(R1));" nl
           "DECR(R2);" nl
           "JUMP(L_make_string_loop);" nl
           "L_make_string_loop_exit:" nl
           "PUSH(IMM(R6));" nl
           "CALL(MAKE_SOB_STRING);" nl
           "DROP(IMM(R6));" nl
           "DROP(1);" nl)))
         (make-primitive-from-code "L_MAKE_STRING" "E_PRIVATE" #f code const-table global-table))))

(define make-string-length
   (lambda(const-table global-table)
       (let ((code (string-append
          "MOV(R0, FPARG(2));" nl
          "PUSH(INDD(R0, 1));" nl
          "PUSH(1);" nl
          "CALL(MAKE_SOB_INTEGER);" nl
          "DROP(2);" nl)))
         (make-primitive-from-code "L_STRING_LENGTH" "E_PRIVATE" 1 code const-table global-table))))

(define make-string-ref
   (lambda(const-table global-table)
       (let ((code (string-append
          "MOV(R0, FPARG(2));" nl
          "CMP(INDD(R0, 0), T_STRING);" nl
          "JUMP_NE(L_err_invalid_param);" nl
          "MOV(R1, FPARG(3));" nl
          "CMP(INDD(R1, 0), T_INTEGER);" nl
          "JUMP_NE(L_err_invalid_param);" nl
          "ADD(R0, 2);" nl
          "ADD(R0, INDD(R1, 1));" nl
          "PUSH(IND(R0));"
          "CALL(MAKE_SOB_CHAR);" nl
          "DROP(1);" nl)))
         (make-primitive-from-code "L_STRING_REF" "E_PRIVATE" 2 code const-table global-table))))

(define make-string-set!
   (lambda(const-table global-table)
       (let ((code (string-append
          "MOV(R0, FPARG(2));" nl
          "CMP(INDD(R0, 0), T_STRING);" nl
          "JUMP_NE(L_err_invalid_param);" nl
          "MOV(R1, FPARG(3));" nl
          "CMP(INDD(R1, 0), T_INTEGER);" nl
          "JUMP_NE(L_err_invalid_param);" nl
          "MOV(R2, FPARG(4));" nl
          "CMP(INDD(R2, 0), T_CHAR);" nl
          "JUMP_NE(L_err_invalid_param);" nl
          "ADD(R0, 2);" nl
          "ADD(R0, INDD(R1, 1));" nl
          "MOV(IND(R0), INDD(R2, 1));" nl
          "MOV(R0, IMM(SOB_VOID));" nl)))
         (make-primitive-from-code "L_STRING_SET" "E_PRIVATE" 3 code const-table global-table))))

(define make-cvector
   (lambda(const-table global-table)
       (let ((code (string-append
           "MOV(R0, FPARG(1));" nl
           "MOV(R1, FP);" nl
           "SUB(R1, 5);" nl
           "L_vector_push_loop:" nl
           "CMP(R0, 0);" nl
           "JUMP_LE(L_vector_push_loop_end);" nl
           "PUSH(STACK(R1));" nl
           "DECR(R1);" nl
           "DECR(R0);" nl
           "JUMP(L_vector_push_loop);" nl
           "L_vector_push_loop_end:" nl
           "PUSH(FPARG(1));" nl
           "CALL(MAKE_SOB_VECTOR);" nl
           "DROP(1);" nl
           "DROP(FPARG(1));" nl
           )))
         (make-primitive-from-code "L_VECTOR" "E_PRIVATE" #f code const-table global-table))))

(define make-make-vector
   (lambda(const-table global-table)
       (let ((code (string-append
           "MOV(R0, FPARG(1));" nl
           "CMP(R0, 3);" nl
           "JUMP_GE(L_err_lambda_args_count);" nl
           "// Save the char in R1" nl
           "CMP(R0, 2);" nl
           "JUMP_EQ(L_make_vector_mem_received);" nl
           "MOV(R1," (number->string (+ 1 (member-const-table 0 const-table))) ");" nl
           "JUMP(L_make_vector_continue);" nl
           "L_make_vector_mem_received:" nl
           "MOV(R1, FPARG(3));" nl
           "L_make_vector_continue:" nl
           "MOV(R2, FPARG(2));" nl
           "MOV(R2, INDD(R2, 1));" nl
           "MOV(R6, R2);" nl
           "L_make_vector_loop:" nl
           "CMP(R2, 0);" nl
           "JUMP_LE(L_make_vector_loop_exit);" nl
           "PUSH(IMM(R1));" nl
           "DECR(R2);" nl
           "JUMP(L_make_vector_loop);" nl
           "L_make_vector_loop_exit:" nl
           "PUSH(IMM(R6));" nl
           "CALL(MAKE_SOB_VECTOR);" nl
           "DROP(IMM(R6));" nl
           "DROP(1);" nl)))
         (make-primitive-from-code "L_MAKE_VECTOR" "E_PRIVATE" #f code const-table global-table))))

(define make-vector-length
   (lambda(const-table global-table)
       (let ((code (string-append
          "MOV(R0, FPARG(2));" nl
          "PUSH(INDD(R0, 1));" nl
          "PUSH(1);" nl
          "CALL(MAKE_SOB_INTEGER);" nl
          "DROP(2);" nl)))
         (make-primitive-from-code "L_VECTOR_LENGTH" "E_PRIVATE" 1 code const-table global-table))))

(define make-vector-ref
   (lambda(const-table global-table)
       (let ((code (string-append
          "MOV(R0, FPARG(2));" nl
          "CMP(INDD(R0, 0), T_VECTOR);" nl
          "JUMP_NE(L_err_invalid_param);" nl
          "MOV(R1, FPARG(3));" nl
          "CMP(INDD(R1, 0), T_INTEGER);" nl
          "JUMP_NE(L_err_invalid_param);" nl
          "ADD(R0, 2);" nl
          "ADD(R0, INDD(R1, 1));" nl
          "MOV(R0, IND(R0));" nl)))
         (make-primitive-from-code "L_VECTOR_REF" "E_PRIVATE" 2 code const-table global-table))))

(define make-vector-set!
   (lambda(const-table global-table)
       (let ((code (string-append
          "MOV(R0, FPARG(2));" nl
          "CMP(INDD(R0, 0), T_VECTOR);" nl
          "JUMP_NE(L_err_invalid_param);" nl
          "MOV(R1, FPARG(3));" nl
          "CMP(INDD(R1, 0), T_INTEGER);" nl
          "JUMP_NE(L_err_invalid_param);" nl
          "ADD(R0, 2);" nl
          "ADD(R0, INDD(R1, 1));" nl
          "MOV(IND(R0), FPARG(4));" nl
          "MOV(R0, IMM(SOB_VOID));" nl)))
         (make-primitive-from-code "L_VECTOR_SET" "E_PRIVATE" 3 code const-table global-table))))

(define make-box
   (lambda(const-table global-table)
      (let ((code
         (string-append
             "PUSH(FPARG(2));" nl
             "CALL(MAKE_SOB_BOX);" nl
             "DROP(1);" nl)))
          (make-primitive-from-code "L_BOX" "E_BOX" 1 code const-table global-table))))

(define make-unbox
   (lambda(const-table global-table)
      (let ((code
         (string-append
             "MOV(R0, FPARG(2));" nl
             "MOV(R0, INDD(R0,1));" nl)))
          (make-primitive-from-code "L_UNBOX" "E_UNBOX" 1 code const-table global-table))))

(define make-reverse
   (lambda(const-table global-table)
       (let ((code '(lambda(lst)
          (letrec ((helper (lambda (ls acc)
            (if (null? ls)
               acc
               (helper (cdr ls) (cons (car ls) acc))))))
          (helper lst '())))))
         (make-primitive-from-scheme "L_REVERSE" "E_PRIVATE" code const-table global-table))))

(define make-apply
     (lambda(const-table global-table)
        (let ((code (string-append
            "// Save num of params in the stack" nl
            (code-gen '(applic (fvar length) ((pvar params 1))) const-table global-table 0)
            "PUSH(IMM(R0));" nl
            "// Calculate the reversed var list" nl
            (code-gen '(applic (fvar reverse) ((pvar params 1))) const-table global-table 0)
            "// Push the vars from the var list (in reversed order)" nl
            "POP(R1);" nl
            "APPLY_PUSH_VARS_LOOP:" nl
            "CMP(R0, SOB_NIL);" nl
            "JUMP_EQ(APPLY_PUSH_VARS_LOOP_END);" nl
            "PUSH(INDD(R0, 1));" nl
            "MOV(R0, INDD(R0, 2));" nl
            "JUMP(APPLY_PUSH_VARS_LOOP);" nl
            "APPLY_PUSH_VARS_LOOP_END:" nl
            "// Push the num of params" nl
            "PUSH(INDD(R1, 1));" nl
            "// Push the closure's env" nl
            "MOV(R0, FPARG(2));" nl
            "PUSH(INDD(R0, 1));" nl
            "CALLA(INDD(R0, 2));" nl
            "DROP(1); // env" nl
            "POP(R1); // num of args" nl
            "DROP(IMM(R1));" nl
            )))
          (make-primitive-from-code "L_apply" "E_APPLY" 2 code const-table global-table))))

(define make-set-car!
     (lambda(const-table global-table)
         (let ((code (string-append
             "MOV(R0, FPARG(2));" nl
             "CMP(IND(R0), T_PAIR);" nl
             "JUMP_NE(L_err_invalid_param);" nl
             "MOV(INDD(R0, 1), FPARG(3));" nl
             "MOV(R0, IMM(SOB_VOID));" nl)))
           (make-primitive-from-code "L_SET_CAR" "E_PRIVATE" 2 code const-table global-table))))

(define make-set-cdr!
     (lambda(const-table global-table)
         (let ((code (string-append
             "MOV(R0, FPARG(2));" nl
             "CMP(IND(R0), T_PAIR);" nl
             "JUMP_NE(L_err_invalid_param);" nl
             "MOV(INDD(R0, 2), FPARG(3));" nl
             "MOV(R0, IMM(SOB_VOID));" nl)))
           (make-primitive-from-code "L_SET_CDR" "E_PRIVATE" 2 code const-table global-table))))

(define make-char->integer
    (lambda(const-table global-table)
       (let ((code (string-append
          "MOV(R1, FPARG(2));" nl
          "PUSH(R1);" nl
          "CALL(IS_SOB_CHAR);" nl
          "DROP(1);" nl
          "CMP(R0,0);" nl
          "JUMP_EQ(L_err_invalid_param);" nl
          "MOV(R0, INDD(R1, 1));" nl
          "PUSH(IMM(R0));"
          "PUSH(1);" nl
          "CALL(MAKE_SOB_INTEGER);" nl
          "DROP(2);" nl)))
         (make-primitive-from-code "L_char_to_integer" "E_CHAR_TO_INTEGER" 1 code const-table global-table))))

(define make-integer->char
    (lambda(const-table global-table)
       (let ((code (string-append
          "MOV(R1, FPARG(2));" nl
          "PUSH(R1);" nl
          "CALL(IS_SOB_INTEGER);" nl
          "DROP(1);" nl
          "CMP(R0,0);" nl
          "JUMP_EQ(L_err_invalid_param);" nl
          "MOV(R0, INDD(R1, 1));" nl
          "PUSH(IMM(R0));"
          "CALL(MAKE_SOB_CHAR);" nl
          "DROP(1);" nl)))
         (make-primitive-from-code "L_integer_to_char" "E_INTEGER_TO_CHAR" 1 code const-table global-table))))

(define make-symbol->string
    (lambda(const-table global-table)
        (let ((code (string-append
             "MOV(R1, FPARG(2));" nl
             "CMP(IND(R1), T_SYMBOL);" nl
             "JUMP_NE(L_err_invalid_param);" nl
             "MOV(R1, INDD(R1, 1));" nl
             "PUSH(INDD(R1, 1));" nl
             "CALL(MALLOC);" nl
             "DROP(1);" nl
             "MOV(INDD(R0, 0), INDD(R1, 0));" nl
             "MOV(INDD(R0, 1), INDD(R1, 1));" nl
             "MOV(R2, IMM(R1));" nl
             "ADD(R2, IMM(2));" nl
             "MOV(R3, IMM(R0));" nl
             "ADD(R0, IMM(2));" nl
             "PUSH(IMM(R2));" nl
             "PUSH(IMM(R0));" nl
             "CALL(STRCPY);" nl
             "DROP(2);" nl
             "MOV(R0, IMM(R3));" nl)))
           (make-primitive-from-code "L_symbol_to_string" "E_SYMBOL_TO_STRING" 1 code const-table global-table))))

(define make-string->symbol
    (lambda(const-table global-table)
        (let ((code (string-append
            "MOV(R1, FPARG(2));" nl
            "CMP(IND(R1), T_STRING);" nl
            "JUMP_NE(L_err_invalid_param);" nl
            "// Search for the string in the symbol string list" nl
            "MOV(R2, IMM(SYMBOL_STRING_LIST));" nl
            "L_FIND_STRING_LOOP:" nl
            "PUSH(INDD(R2, 0));" nl
            "PUSH(R1);" nl
            "CALL(COMPARE_SOB_STRING);" nl
            "DROP(2);" nl
            "CMP(R0, 1);" nl
            "JUMP_EQ(L_STRING_FOUND);" nl
            "CMP(INDD(R2, 1), 0);" nl
            "JUMP_EQ(L_STRING_NOT_FOUND);" nl
            "MOV(R2, INDD(R2, 1));" nl
            "JUMP(L_FIND_STRING_LOOP);" nl
            "L_STRING_NOT_FOUND:" nl
            "PUSH(IMM(2));" nl
            "CALL(MALLOC);" nl
            "DROP(1);" nl
            "MOV(INDD(R2, 1), IMM(R0));" nl
            "MOV(INDD(R0, 0), IMM(R1));" nl
            "MOV(INDD(R0, 1), IMM(0));" nl
            "JUMP(L_STRING_TO_SYMBOL_CONT);" nl
            "L_STRING_FOUND:" nl
            "MOV(R1, INDD(R2, 0));" nl
            "L_STRING_TO_SYMBOL_CONT:" nl
            "// Create symbol" nl
            "PUSH(IMM(2));" nl
            "CALL(MALLOC);" nl
            "DROP(1);" nl
            "MOV(INDD(R0, 0), T_SYMBOL);" nl
            "MOV(INDD(R0, 1), IMM(R1));" nl)))
          (make-primitive-from-code "L_string_to_symbol" "E_STRING_TO_SYMBOL" 1 code const-table global-table))))

(define make-predicate
    (lambda(label env-name func-name const-table global-table)
       (let ((code (string-append
          "PUSH(FPARG(2));" nl
          "CALL(" func-name ");" nl
          "DROP(1);" nl
          "PUSH(IMM(R0));" nl
          "CALL(L_int_to_bool);" nl
          "DROP(1);" nl)))
         (make-primitive-from-code label env-name 1 code const-table global-table))))

(define make-number?
    (lambda(const-table global-table)
       (make-predicate "L_pnumber" "E_PRIVATE" "IS_SOB_INTEGER" const-table global-table)))

(define make-integer?
    (lambda(const-table global-table)
      (let ((code (string-append
          "MOV(R0, FPARG(2));" nl
          "CMP(IND(R0), T_INTEGER);" nl
          "JUMP_NE(L_PINTEGER_FALSE);" nl
          "CMP(INDD(R0, 2), 1);" nl
          "JUMP_NE(L_PINTEGER_FALSE);" nl
          "MOV(R0, IMM(SOB_TRUE));" nl
          "JUMP(L_PINTEGER_EXIT);" nl
          "L_PINTEGER_FALSE:" nl
          "MOV(R0, IMM(SOB_FALSE));" nl
          "L_PINTEGER_EXIT:" nl)))
       (make-primitive-from-code "L_pinteger" "E_PINTEGER" 1 code const-table global-table))))

(define make-rational?
    (lambda(const-table global-table)
        (make-primitive-from-scheme "L_prational" "E_PRIVATE" '(lambda(x) (number? x)) const-table global-table)))

(define make-boolean?
    (lambda(const-table global-table)
       (make-predicate "L_pboolean" "E_PBOOLEAN" "IS_SOB_BOOL" const-table global-table)))

(define make-char?
    (lambda(const-table global-table)
       (make-predicate "L_pchar" "E_PCHAR" "IS_SOB_CHAR" const-table global-table)))

(define make-procedure?
    (lambda(const-table global-table)
       (make-predicate "L_pclosure" "E_PCLOSURE" "IS_SOB_CLOSURE" const-table global-table)))

(define make-pair?
    (lambda(const-table global-table)
       (make-predicate "L_ppair" "E_PPAIR" "IS_SOB_PAIR" const-table global-table)))

(define make-symbol?
    (lambda(const-table global-table)
       (make-predicate "L_psymbol" "E_PSYMBOL" "IS_SOB_SYMBOL" const-table global-table)))

(define make-vector?
    (lambda(const-table global-table)
       (make-predicate "L_pvector" "E_PVECTOR" "IS_SOB_VECTOR" const-table global-table)))

(define make-string?
    (lambda(const-table global-table)
       (make-predicate "L_pstring" "E_PSTRING" "IS_SOB_STRING" const-table global-table)))

(define create-prologue
     (lambda(const-table global-table)
                (let ((const-table-size (number->string (const-table-new-address const-table)))
                      (global-table-size (number->string (global-table-new-address global-table))))
           (string-append
            "#include <stdio.h>" nl
            "#include <stdlib.h>" nl
            "#define DO_SHOW 1" nl
            "#include \"arch/cisc.h\"" nl
            "#include \"arch/debug_macros.h\"" nl
            nl
            "int main()" nl
            "{" nl
            "START_MACHINE;" nl
            nl
            "JUMP(CONTINUE);" nl
            nl
            "#include \"arch/char.lib\"" nl
            "#include \"arch/io.lib\"" nl
            "#include \"arch/math.lib\"" nl
            "#include \"arch/string.lib\"" nl
            "#include \"arch/system.lib\"" nl
            "#include \"arch/scheme.lib\"" nl
            nl
            nl
            "#define CONST_TABLE " const-table-register nl
            "#define GLOBAL_TABLE " global-table-register nl
            "#define SYMBOL_STRING_LIST " symbol-string-list-register nl
            nl
            "#define SOB_NIL " const-nil-register nl
            "#define SOB_VOID " const-void-register nl
            "#define SOB_TRUE " const-true-register nl
            "#define SOB_FALSE " const-false-register nl
            nl
            nl
            "INIT_CONST_TABLE:" nl
            func-prologue
            "PUSH(IMM(" const-table-size "));" nl
            "CALL(MALLOC);" nl
            "DROP(1);" nl
            "MOV(CONST_TABLE, R0);" nl
            (make-const-table-mov-instructions const-table)
            (make-const-table-symbol-str-list const-table)
            func-epilogue
            nl
            nl
            "INIT_GLOBAL_TABLE:" nl
            func-prologue
            "PUSH(IMM(" global-table-size "));" nl
            "CALL(MALLOC);" nl
            "DROP(1);" nl
            "MOV(GLOBAL_TABLE, R0);" nl
            (make-global-table-mov-instructions const-table global-table)
            nl
            "MOV(R0, CONST_TABLE);" nl
            "ADD(R0, " (number->string (member-const-table '() const-table)) ");" nl
            "MOV(" const-nil-register ", R0);" nl
            "MOV(R0, CONST_TABLE);" nl
            "ADD(R0, " (number->string (member-const-table (void) const-table)) ");" nl
            "MOV(" const-void-register ", R0);" nl
            "MOV(R0, CONST_TABLE);" nl
            "ADD(R0, " (number->string (member-const-table #f const-table)) ");" nl
            "MOV(" const-false-register ", R0);" nl
            "MOV(R0, CONST_TABLE);" nl
            "ADD(R0, " (number->string (member-const-table #t const-table)) ");" nl
            "MOV(" const-true-register ", R0);" nl
            func-epilogue
            nl
            nl
            "// Private Primitives" nl
            (make-int-to-bool const-table global-table)
            nl
            nl
            (code-gen-error "L_err_lambda_args_count" "Lambda called with wrong number of args!")
            (code-gen-error "L_err_cannot_apply_non_clos" "Applic called on non closure!")
            (code-gen-error "L_err_define_not_fvar" "Defined called on non fvar!")
            (code-gen-error "L_err_not_in_code_gen" "Code-gen called on unknown expression!")
            (code-gen-error "L_err_invalid_param" "Function received invalid param!")
            nl
            nl
            code-gen-write-sob-if-not-void
            nl
            nl
            "CONTINUE:" nl
            nl
            "CREATE_FAKE_ENV:" nl
            "PUSH(IMM(0));" nl
            "PUSH(IMM(SOB_VOID));" nl
            "PUSH(IMM(SOB_VOID));" nl
            "PUSH(IMM(SOB_VOID));" nl
            nl
            "CALL(INIT_CONST_TABLE);" nl
            "CALL(INIT_GLOBAL_TABLE);" nl
            nl
            nl
            ))))

(define print-r0
    (string-append
        "// Print R0" nl
        "PUSH(IMM(R0));" nl
        "CALL(WRITE_SOB_IF_NOT_VOID);" nl
        "DROP(1);" nl
        ))


(define epilogue
    (string-append
        "EXIT:" nl
        "DROP(4); // Fake env" nl
        "STOP_MACHINE;" nl
        nl
        "return 0;" nl
        "}" nl))

(define label-gen
    (lambda(str)
        (string-append str "_" (symbol->string (gensym)))))

(define code-gen
    (lambda(parsed-expr const-table global-table major)
        (cond ((null? parsed-expr) "")
              ((list? (car parsed-expr)) (string-append (code-gen (car parsed-expr) const-table global-table major)
                                                        (code-gen (cdr parsed-expr) const-table global-table major)))
              ((eq? (car parsed-expr) 'const) (code-gen-const parsed-expr const-table))
              ((eq? (car parsed-expr) 'if3) (code-gen-if parsed-expr const-table global-table major))
              ((eq? (car parsed-expr) 'seq) (code-gen-seq parsed-expr const-table global-table major))
              ((eq? (car parsed-expr) 'or) (code-gen-or parsed-expr const-table global-table major))
              ((is-lambda? (car parsed-expr)) (code-gen-lambda parsed-expr const-table global-table major))
              ((eq? (car parsed-expr) 'applic) (code-gen-applic parsed-expr const-table global-table major))
              ((eq? (car parsed-expr) 'tc-applic) (code-gen-tc-applic parsed-expr const-table global-table major))
              ((eq? (car parsed-expr) 'fvar) (code-gen-fvar parsed-expr const-table global-table major))
              ((eq? (car parsed-expr) 'bvar) (code-gen-bvar parsed-expr const-table global-table major))
              ((eq? (car parsed-expr) 'pvar) (code-gen-pvar parsed-expr const-table global-table major))
              ((eq? (car parsed-expr) 'def) (code-gen-define parsed-expr const-table global-table major))
              ((eq? (car parsed-expr) 'set) (code-gen-set parsed-expr const-table global-table major))
              ((eq? (car parsed-expr) 'box) (code-gen-box parsed-expr const-table global-table major))
              ((eq? (car parsed-expr) 'box-get) (code-gen-box-get parsed-expr const-table global-table major))
              ((eq? (car parsed-expr) 'box-set) (code-gen-box-set parsed-expr const-table global-table major))
              (else "JUMP(L_err_not_in_code_gen)\n"))))

(define code-gen-const
    (lambda(expr const-table)
        (let* ((val (cadr expr))
               (rel-addr (member-const-table val const-table)))
            (string-append
               "// " (format "~s" expr) nl
               "MOV(R0, CONST_TABLE);" nl
               "ADD(R0, " (number->string rel-addr) ");" nl))))

(define code-gen-if
    (lambda(expr const-table global-table major)
        (let ((test (cadr expr))
              (dit (caddr expr))
              (dif (cadddr expr))
              (else-label (label-gen "L_if3_else"))
              (exit-label (label-gen "L_if3_exit")))
            (string-append
              "// if3" nl
              (code-gen test const-table global-table major)
              "CMP(R0, IMM(SOB_FALSE));" nl
              "JUMP_EQ(" else-label ");" nl
              (code-gen dit const-table global-table major)
              "JUMP(" exit-label ");" nl
              else-label ":" nl
              (code-gen dif const-table global-table major)
              nl
              exit-label ":" nl
              ))))

(define code-gen-seq
    (lambda(expr const-table global-table major)
       (let ((expr-list (cadr expr)))
           (string-append
             "// seq" nl
             (apply string-append (map (lambda(exp) (code-gen exp const-table global-table major)) expr-list))))))

(define code-gen-or
    (lambda(expr const-table global-table major)
       (let ((expr-list (cadr expr))
             (exit-label (label-gen "L_or_exit")))
           (string-append
             "// or" nl
             (apply string-append
               (map (lambda(exp)
                (string-append
                   (code-gen exp const-table global-table major)
                   "CMP(R0, IMM(SOB_FALSE));" nl
                   "JUMP_NE(" exit-label ");" nl
                   )) expr-list))
             nl
             exit-label ":" nl
             ))))

(define code-gen-lambda
    (lambda(expr const-table global-table major)
        (let* ((lambda-def (lambda-get-def expr))
               (body (lambda-get-body expr))
               (params (lambda-get-listed-params expr))
               (new-major (+ major 1))
               (body-label (label-gen "L_clos_body"))
               (copy-env-label1 (label-gen "L_clos_copy_env_begin"))
               (copy-env-label2 (label-gen "L_clos_copy_env_exit"))
               (copy-params-label1 (label-gen "L_clos_copy_params_begin"))
               (copy-params-label2 (label-gen "L_clos_copy_params_exit"))
               (params-not-empty-label (label-gen "L_clos_params_not_empty"))
               (exit-label (label-gen "L_clos_exit")))
            (string-append
                "// lambda" nl
                "// Allocate env list" nl
                "MOV(R1, FPARG(0));" nl
                "PUSH(IMM(" (number->string new-major) "));" nl
                "CALL(MALLOC);" nl
                "DROP(1);" nl
                "MOV(R2, R0);" nl
                nl
                "// Copy old env" nl
                "XOR(R3, R3);" nl
                "MOV(R4, 1);" nl
                copy-env-label1 ":" nl
                "CMP(R3, IMM(" (number->string major) "));" nl
                "JUMP_GE(" copy-env-label2 ");" nl
                "MOV(R5, R2);" nl
                "ADD(R5, R4);" nl
                "MOV(R6, R1);" nl
                "ADD(R6, R3);" nl
                "MOV(IND(R5), IND(R6));" nl
                "INCR(R3);" nl
                "INCR(R4);" nl
                "JUMP(" copy-env-label1 ");" nl
                copy-env-label2 ":" nl
                nl
                "// Allocate current env" nl
                "MOV(R3, FPARG(1)); // Number of last lambda params" nl
                "PUSH(IMM(R3));" nl
                "CALL(MALLOC);" nl
                "DROP(1);" nl
                "MOV(IND(R2), R0);" nl
                "CMP(R3, IMM(0));" nl
                "JUMP_NE(" params-not-empty-label ");" nl
                "MOV(IND(R2), IMM(E_EMPTY));" nl
                params-not-empty-label ":" nl
                nl
                "// Copy last lambda params" nl
                "XOR(R4, R4);" nl
                "MOV(R5, 1);" nl
                copy-params-label1 ":" nl
                "CMP(R4, IMM(R3));" nl
                "JUMP_GE(" copy-params-label2 ");" nl
                "MOV(R6, IND(R2));" nl
                "ADD(R6, R4);" nl
                "MOV(R7, IMM(FP));" nl
                "SUB(R7, IMM(4));" nl
                "SUB(R7, IMM(R5));" nl
                "MOV(IND(R6), STACK(R7));" nl
                "INCR(R4);" nl
                "INCR(R5);" nl
                "JUMP(" copy-params-label1 ");" nl
                copy-params-label2 ":" nl
                nl
                "// Allocate closure object" nl
                "PUSH(IMM(3));" nl
                "CALL(MALLOC);" nl
                "DROP(1);" nl
                "MOV(INDD(R0, 0), T_CLOSURE);" nl
                "MOV(INDD(R0, 1), IMM(R2)); // env" nl
                "MOV(INDD(R0, 2), LABEL(" body-label "));" nl
                "JUMP(" exit-label ");" nl
                nl
                body-label ":" nl
                func-prologue
                (create-lambda-body lambda-def params body const-table global-table major)
                func-epilogue
                nl
                exit-label ":" nl
                ))))

(define create-lambda-body
    (lambda(lambda-def params body const-table global-table major)
       (cond ((eq? lambda-def 'lambda-simple) (create-lambda-simple-body params body const-table global-table major))
             ((or (eq? lambda-def 'lambda-var) (eq? lambda-def 'lambda-opt))
               (create-lambda-opt-body params body const-table global-table major)))))

(define create-lambda-simple-body
    (lambda(params body const-table global-table major)
       (string-append
           "// Lambda-simple body" nl
           (code-gen-check-lambda-params (length params))
           "// Actual body" nl
           (code-gen body const-table global-table (+ major 1))
           )))

(define create-lambda-opt-body
    (lambda(params body const-table global-table major)
       (let ((num-params (- (length params) 1))
             (var-list-loop (label-gen "L_var_list_loop"))
             (var-list-loop-end (label-gen "L_var_list_loop_end"))
             (stack-loop (label-gen "L_stack_loop"))
             (stack-fix-end (label-gen "L_stack_fix_end"))
             (fix-stack-empty-var-list (label-gen "L_fix_stack_empty_var_list"))
             (fix-stack-empty-loop (label-gen "L_fix_stack_empty_loop"))
             (stack-fix-empty-end (label-gen "L_stack_fix_empty_end")))
         (string-append
            "// Lambda-opt/var body" nl
            "// Init R2 with the length of the var list" nl
            "MOV(R2, FPARG(1));" nl
            "SUB(R2, IMM(" (number->string num-params) "));" nl
            "// Save the var list length in R6 for later" nl
            "MOV(R6, IMM(R2));" nl
            "// Create var list" nl
            "MOV(R0, SOB_NIL); // Result of var list in R0" nl
            "MOV(R3, IMM(FP));" nl
            "SUB(R3, IMM(4));" nl
            "SUB(R3, FPARG(1)); // Save increasing stack pointer in R3" nl
            var-list-loop ":" nl
            "CMP(R2, 0);" nl
            "JUMP_LE(" var-list-loop-end ");" nl
            "PUSH(IMM(R0));" nl
            "PUSH(STACK(R3));" nl
            "CALL(MAKE_SOB_PAIR);" nl
            "DROP(2);" nl
            "INCR(R3);" nl
            "DECR(R2);" nl
            "JUMP(" var-list-loop ");" nl
            var-list-loop-end ":" nl
            "// Fix the stack" nl
            "CMP(R6, 0);" nl
            "JUMP_EQ(" fix-stack-empty-var-list ");" nl
            "MOV(R1, IMM(FP));" nl
            "SUB(R1, IMM(3));" nl
            "SUB(R1, FPARG(1)); // R1 = bottom" nl
            "MOV(R2, IMM(FP));" nl
            "SUB(R2, IMM(" (number->string (+ 4 num-params)) ")); // R2 = bottom of non-optional params" nl
            stack-loop ":" nl
            "CMP(R2, IMM(FP));" nl
            "JUMP_GE(" stack-fix-end ");" nl
            "MOV(STACK(R1), STACK(R2));" nl
            "INCR(R1);" nl
            "INCR(R2);" nl
            "JUMP(" stack-loop ");" nl
            fix-stack-empty-var-list ":" nl
            "// Init R3 with the loop limit (position of first optional var in original stack)" nl
            "MOV(R3, IMM(FP));" nl
            "SUB(R3, IMM(" (number->string (+ 4 num-params)) "));" nl
            "MOV(R1, IMM(FP));" nl
            "INCR(R1);" nl
            "MOV(R2, IMM(FP));" nl
            fix-stack-empty-loop ":" nl
            "CMP(R2, IMM(R3));" nl
            "JUMP_LT(" stack-fix-empty-end ");" nl
            "MOV(STACK(R1), STACK(R2));" nl
            "DECR(R1);" nl
            "DECR(R2);" nl
            "JUMP(" fix-stack-empty-loop ");" nl
            stack-fix-empty-end ":" nl
            "// Fix R1 to point to the new FP since the code below relies on that" nl
            "MOV(R1, IMM(FP));" nl
            "INCR(R1);" nl
            stack-fix-end ":" nl
            "// Fix FP and SP" nl
            "MOV(FP, IMM(R1));" nl
            "MOV(SP, IMM(FP));" nl
            "// Fix the number of params" nl
            "SUB(R1, IMM(4));" nl
            "MOV(STACK(R1), IMM(" (number->string (+ num-params 1)) "));" nl
            "// Write the var list" nl
            "SUB(R1, IMM(" (number->string (+ num-params 1)) "));" nl
            "MOV(STACK(R1), IMM(R0)); // Put the var list" nl
            "// Actual body" nl
            (code-gen body const-table global-table (+ major 1))))))

(define code-gen-write-sob-if-not-void
    (string-append
        "WRITE_SOB_IF_NOT_VOID:" nl
        func-prologue
        "CMP(FPARG(0), IMM(SOB_VOID));" nl
        "JUMP_EQ(WRITE_SOB_IF_NOT_VOID_END);" nl
        "PUSH(FPARG(0));" nl
        "CALL(WRITE_SOB);" nl
        "DROP(1);" nl
        "PUSH(IMM('\\n'));" nl
        "CALL(PUTCHAR);" nl
        "DROP(1);" nl
        "WRITE_SOB_IF_NOT_VOID_END:" nl
        func-epilogue))

(define code-gen-applic
    (lambda(expr const-table global-table major)
        (let ((proc (cadr expr))
              (args (caddr expr)))
            (string-append
              "// applic" nl
              (fold-right (lambda(arg code)
                  (string-append
                     code
                     (code-gen arg const-table global-table major)
                     "PUSH(IMM(R0));" nl
                     )) "" args)
              "PUSH(IMM(" (number->string (length args)) ")); // Num of params" nl
              (code-gen proc const-table global-table major)
              "CMP(INDD(R0, 0), IMM(T_CLOSURE));" nl
              "JUMP_NE(L_err_cannot_apply_non_clos);" nl
              "PUSH(INDD(R0, 1));" nl
              "CALLA(INDD(R0, 2));" nl
              "DROP(1); // env" nl
              "POP(R1); // num of args" nl
              "DROP(IMM(R1));" nl
              ))))

(define code-gen-tc-applic
    (lambda(expr const-table global-table major)
        (let ((proc (cadr expr))
              (args (caddr expr))
              (fix-stack-loop (label-gen "L_fix_stack_loop"))
              (fix-stack-loop-end (label-gen "L_fix_stack_loop_end")))
            (string-append
              "// tc-applic" nl
              (fold-right (lambda(arg code)
                  (string-append
                     code
                     (code-gen arg const-table global-table major)
                     "PUSH(IMM(R0));" nl
                     )) "" args)
              "PUSH(IMM(" (number->string (length args)) ")); // Num of params" nl
              (code-gen proc const-table global-table major)
              "CMP(INDD(R0, 0), IMM(T_CLOSURE));" nl
              "JUMP_NE(L_err_cannot_apply_non_clos);" nl
              "PUSH(INDD(R0, 1)); // env" nl
              "PUSH(FPARG(-1)); // ret" nl
              "// Save old_fp" nl
              "MOV(R1, FP);" nl
              "DECR(R1);" nl
              "MOV(R1, STACK(R1));" nl
              "// Fix the stack" nl
              "MOV(R2, IMM(FP));" nl
              "SUB(R2, 4);" nl
              "SUB(R2, STACK(R2)); // R2 = bottom" nl
              "MOV(R3, IMM(FP));" nl
              fix-stack-loop ":" nl
              "CMP(R3, IMM(SP));" nl
              "JUMP_GE(" fix-stack-loop-end ");" nl
              "MOV(STACK(R2), STACK(R3));" nl
              "INCR(R2);" nl
              "INCR(R3);" nl
              "JUMP(" fix-stack-loop ");" nl
              fix-stack-loop-end ":" nl
              "MOV(SP, IMM(R2));" nl
              "MOV(FP, R1);" nl
              "JUMPA(INDD(R0, 2));" nl
              ))))

(define code-gen-fvar
    (lambda(expr const-table global-table major)
        (let* ((var-name (cadr expr))
               (var-addr (member-global-table var-name global-table)))
             (string-append
                "// " (format "~s" expr) nl
                "MOV(R0, INDD(GLOBAL_TABLE," (number->string var-addr) "));" nl
                ))))

(define code-gen-pvar
    (lambda(expr const-table global-table major)
        (let ((var-name (cadr expr))
              (minor (caddr expr)))
            (string-append
               "// " (format "~s" expr) nl
               "MOV(R0, FPARG(" (number->string (+ 2 minor)) "));" nl))))

(define code-gen-bvar
    (lambda(expr const-table global-table major)
        (let ((var-name (cadr expr))
              (major (caddr expr))
              (minor (cadddr expr)))
             (string-append
               "// " (format "~s" expr) nl
               "MOV(R0, FPARG(0));" nl
               "MOV(R0, INDD(R0, " (number->string major) "));" nl
               "MOV(R0, INDD(R0, " (number->string minor) "));" nl))))

(define code-gen-define
    (lambda(expr const-table global-table major)
        (let* ((var (cadr (cadr expr)))
               (val (caddr expr))
               (address (member-global-table var global-table)))
             (if address
               (string-append
                 "// define " (format "~s" var) nl
                 "MOV(R1, GLOBAL_TABLE);" nl
                 "ADD(R1, " (number->string address) ");" nl
                 "PUSH(R1); // Save pointer to fvar" nl
                 (code-gen val const-table global-table major)
                 "POP(R1); // Restore pointer to fvar" nl
                 "MOV(IND(R1), R0);" nl
                 "MOV(R0, SOB_VOID);" nl)
               (string-append "JUMP(L_err_define_not_fvar);" nl))
              )))

(define code-gen-set
    (lambda(expr const-table global-table major)
       (let* ((var (cadr expr))
              (val (caddr expr)))
        (string-append
           "// " (format "~s" expr) nl
           (code-gen val const-table global-table major)
           (cond ((eq? (car var) 'pvar)
               (string-append
                   "MOV(R1, FP);" nl
                   "SUB(R1, " (number->string (+ 5 (caddr var))) ");" nl
                   "MOV(STACK(R1), R0);" nl))
                 ((eq? (car var) 'bvar)
                   (let ((major (caddr var))
                         (minor (cadddr var)))
                     "MOV(R1, FPARG(0));" nl
                     "MOV(R1, INDD(R1, " (number->string major) "));" nl
                     "MOV(INDD(R1, " (number->string minor) "), IMM(R0));" nl))
                 (else (string-append
                   "MOV(R1, IMM(GLOBAL_TABLE));" nl
                   "ADD(R1, " (number->string (member-global-table (cadr var) global-table)) ");" nl
                   "MOV(IND(R1), IMM(R0));" nl)))
           "MOV(R0, IMM(SOB_VOID));" nl))))

(define code-gen-box
    (lambda(expr const-table global-table major)
        (let* ((var (cadr expr)))
           (string-append
             "// " (format "~s" expr) nl
             (code-gen var const-table global-table major)
             "PUSH(IMM(R0));" nl
             "CALL(MAKE_SOB_BOX);" nl
             "DROP(1);" nl))))

(define code-gen-box-set
    (lambda(expr const-table global-table major)
        (let* ((var (cadr expr))
               (val (caddr expr)))
            (string-append
               "// box-set" nl
               (code-gen var const-table global-table major)
               "PUSH(IMM(R0)); // Save the box pointer" nl
               (code-gen val const-table global-table major)
               "POP(R1);" nl
               "MOV(INDD(R1, 1), IMM(R0));" nl
               ))))

(define code-gen-box-get
    (lambda(expr const-table global-table major)
        (let* ((var (cadr expr)))
           (string-append
              "// " (format "~s" expr) nl
              (code-gen var const-table global-table major)
              "MOV(R0, INDD(R0,1));" nl
              ))))