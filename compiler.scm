(load "./pc.scm")

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

(define <comment>
  (disj <line-comment>
	<sexpr-comment>))

(define <skip>
  (disj <comment>
	<whitespace>))

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
	        (*parser <HexChar>) *star
		    (*caten 2)
		    (*pack-with (lambda (a b)
		       (integer->char (string->number (list->string b) 16))))
	     done))

(define <Char>
	(new    (*parser <CharPrefix>)
            (*parser <NamedChar>)
            (*parser <HexUnicodeChar>)
	        (*parser <VisibleSimpleChar>)
            (*disj 3)
            (*caten 2)
            (*parser <end-of-input>)
            (*caten 2)
            (*pack-with (lambda (a b) a))   
            (*pack-with
                  (lambda (a b)
                    b
                  ))
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
                ;(*parser <end-of-input>)
                ;(*caten 2)
                ;(*pack-with (lambda (a b) a))
	     done))
	     
(define <StringVisibleChar> (range #\space #\xffff))
	     
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
		    (*parser <HexChar>) *plus
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
	     
(define <Symbol>
        (new    (*parser <SymbolChar>) *plus
                (*pack (lambda (a)
                    (string->symbol (list->string a))))
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
                (*parser (char #\)))
                (*caten 3)
                (*pack-with (lambda (a b c)
                    (vector)))
                (*parser (char #\#))
                (*parser (char #\())
                (*delayed (lambda () <sexpr>))
                (*parser (char #\space))
                (*delayed (lambda () <sexpr>))
                (*caten 2) *star
                (*pack(lambda (a)
                    (map cadr a)))
                (*caten 2)
                (*pack-with (lambda (a b)
                    (list a b)))
                (*parser (char #\)))
                (*caten 4)
                (*pack-with (lambda (a b c d)
                    (apply vector (append (list (car c)) (cadr c)))))
                (*disj 2)
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

(define infix-pack-l (lambda (s)
     (lambda (a b)
        (if (null? b) a
           (let* ((b-values (map cadr b)))
             (fold-left (lambda (x y) `(,s ,x ,y)) a b-values))))
        ))      

(define infix-pack-r (lambda (s)
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
        
(define <InfixSub> 
       (new     (*delayed (lambda() <InfixAdd>))
                (*parser (char #\-))
                (*delayed (lambda() <InfixAdd>))
                (*caten 2) *star
                (*caten 2)
                (*pack-with (infix-pack-l '-))
        done))
        
(define <InfixAdd> 
       (new     (*delayed (lambda() <InfixDiv>))
                (*parser (char #\+))
                (*delayed (lambda() <InfixDiv>))
                (*caten 2) *star
                (*caten 2)
                (*pack-with (infix-pack-l '+))
        done))
        
(define <InfixDiv> 
       (new     (*delayed (lambda() <InfixMul>))
                (*parser (char #\/))
                (*delayed (lambda() <InfixMul>))
                (*caten 2) *star
                (*caten 2)
                (*pack-with (infix-pack-l '/))
        done))
        
(define <InfixMul> 
       (new     (*delayed (lambda() <InfixPow>))
                (*parser (char #\*))
                (*delayed (lambda() <InfixPow>))
                (*caten 2) *star
                (*caten 2)
                (*pack-with (infix-pack-l '*))
        done))
        
(define <PowerSymbol>
       (new     (*parser (word "**"))
                (*parser (char #\^))
                (*disj 2)
        done))
        
(define <InfixPow> 
       (new     (*delayed (lambda() (^<skipped*> <InfixArrayGet>)))
                (*parser <PowerSymbol>)
                (*delayed (lambda() (^<skipped*> <InfixArrayGet>)))
                (*caten 2) *star
                (*caten 2)
                (*pack-with (infix-pack-r 'expt))
        done))
        
(define <InfixArrayGet> 
       (new     (*delayed (lambda() (^<skipped*> <InfixFuncall>)))
                (*parser (char #\[))
                (*delayed (lambda() <InfixExpression>))
                (*parser (char #\]))
                (*caten 3) 
                *star
                (*parser <epsilon>)
                (*disj 2)
                (*caten 2) 
                (*pack-with (infix-pack-l 'vector-ref))
        done))
        
 (define <InfixFuncall>
       (new     (*delayed (lambda() <InfixLast>))
                (*parser (char #\())
                (*delayed (lambda() <InfixArgList>))
                (*parser (char #\)))
                (*caten 3)
                (*pack-with (lambda(a b c) 
                                b))
                (*parser <epsilon>)
                (*disj 2)
                (*caten 2)
                (*pack-with (lambda(a b)
                                (if (null? b) a     
                                `(,a ,@b))))
                                   
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
                            
                (*parser <epsilon>)
                (*disj 2)
        done))
        
 (define <InfixParen>
        (new    (*parser (char #\())
                (*delayed (lambda() <InfixExpression>))
                (*parser (char #\)))
                (*caten 3)
                (*pack-with (lambda (a b c) b))
        done)) 
        
(define <InfixSexprEscape>
        (new    (*delayed (lambda() <InfixPrefixExtensionPrefix>))
                (*delayed (lambda () <sexpr>))
                (*caten 2)
        done))
        
(define <InfixNeg>
        (new    (*parser (char #\-))
                (*delayed (lambda () <InfixArrayGet>))
                (*caten 2)
                (*pack-with (lambda (a b)
                    `(- ,b)))
        done))
        
(define <InfixLast>
        (new    (*parser (^<skipped*> <InfixSexprEscape>))
                (*parser (^<skipped*> <InfixParen>))
                (*parser (^<skipped*> <InfixNeg>))
                (*delayed (lambda() (^<skipped*> <Number>)))
                (*delayed (lambda() (^<skipped*> <InfixSymbol>)))
                (*disj 5)
          done))
          
(define <InfixExpression> <InfixSub>)
        
(define <InfixSymbol>
        (new    (*parser <SymbolChar>)
                (*parser (char #\+))
                (*parser (char #\-))
                (*parser (char #\*))
                (*parser (char #\^))
                (*parser (char #\/))
                (*disj 5)
                *diff *plus
                (*parser (word "**"))
                *diff
                (*pack (lambda (a)
                    (string->symbol (list->string a))))
        done))
        
(define <InfixExtension>
        (new    (*parser <InfixPrefixExtensionPrefix>)
                (*parser <InfixExpression>)
                (*caten 2)
                (*pack-with (lambda (a b)
                    b))
        done))
        
(define <sexpr> (^<skipped*> 
	        (disj <Boolean> <Char> <Number> <Symbol> <String> <ProperList>
	              <ImproperList> <Vector> <Quoted> <QuasiQuoted> <Unquoted>
	              <UnquotedAndSpliced> <InfixExtension>)))
