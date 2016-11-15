(load "./pc.scm")

(define <Boolean>
	(new    (*parser (char #\#))
	        (*parser (char #\t))
	        (*caten 2)
	        (*parser (char #\#))
	        (*parser (char #\f))
	        (*caten 2)
		(*disj 2)
		(*pack-with 
		  (lambda (a b)
		    (if (eq? b #\t)
		       #t
		       #f)
                    ))
        done))
	     
(define <CharPrefix>
      (new     (*parser (char #\#))
               (*parser (char #\\))
               (*caten 2)
       done))
       
(define <VisibleSimpleChar> (range #\! #\xff))


(define <NamedChar>
	(new    (*parser (word "lambda"))
	        (*parser (word "newline"))
            (*parser (word "nul"))
            (*parser (word "page")) 
	        (*parser (word "return"))
	        (*parser (word "space"))
	        (*parser (word "tab"))
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
	(new    (*parser (char #\x))
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
	     done))
	     
(define <StringVisibleChar> (range #\space #\xffff))
	     
(define <StringMetaChar>
	(new    (*parser (char #\\)) 
	        (*parser (char #\\))  ;ask Mayer after
		    (*parser (char #\"))
		    (*parser (char #\t))
		    (*parser (char #\f))
		    (*parser (char #\n))
            (*parser (char #\r))
		    (*disj 6)
		    (*caten 2)
		    (*pack-with (lambda (a b)
		         (cond ((equal? b #\\) #\\)
		               ((equal? b #\") #\")
		               ((equal? b #\t) #\tab)
		               ((equal? b #\f) #\xc)
		               ((equal? b #\n) #\xa)
		               ((equal? b #\r) #\xd))))
	     done))

(define <StringHexChar>
	(new    (*parser (char #\\))
            (*parser (char #\x))
            (*caten 2)
		    (*parser <HexChar>) *star
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
                (*disj 15)
        done))	    
	     
(define <Symbol>
        (new    (*parser <SymbolChar>) *plus
                (*pack (lambda (a)
                    (string->symbol (list->string a))))
        done))
	     
(define <ProperList>
        (new    (*parser (char #\())
                (*delayed (lambda () <sexpr>))
                (*parser (char #\space))
                (*delayed (lambda () <sexpr>))
                (*caten 2) *star
                (*parser (char #\)))
                (*caten 4)
                (*pack-with (lambda (a b c d)
                    (append (list b) (map cadr c))))
        done))

(define <ImproperList>
        (new    (*parser (char #\())
                (*delayed (lambda () <sexpr>))
                (*parser (char #\space))
                (*caten 2) *plus
                (*parser (char #\.))
                (*parser (char #\space))
                (*delayed (lambda () <sexpr>))
                (*parser (char #\)))
                (*caten 6)
                (*pack-with (lambda (a b c d e f)
                    `(,@(map car b) . ,e))
                        )
        done))
        
(define <Vector>
        (new    (*parser (char #\#))
                (*parser (char #\())
                (*delayed (lambda () <sexpr>))
                (*parser (char #\)))
                (*caten 4)
                (*pack-with (lambda (a b c d)
                    (apply vector c)))
        done))
        
(define <Quoted>
        (new    (*parser (char #\'))
                (*delayed (lambda()  <sexpr>))
                (*caten 2)
        done))
        
(define <QuasiQuoted>
        (new    (*parser (char #\`))
                (*delayed (lambda()  <sexpr>))
                (*caten 2)
        done))
        
(define <Unquoted>
        (new    (*parser (char #\,))
                (*delayed (lambda()  <sexpr>))
                (*caten 2)
        done))        
        
(define <UnquotedAndSpliced>
        (new    (*parser (char #\,))
                (*parser (char #\@))
                (*delayed (lambda()  <sexpr>))
                (*caten 3)
        done))
        
(define <InfixExpression>
        (new    (*delayed (lambda() (*parser <InfixAdd>)))
                (*delayed (lambda() (*parser <InfixNeg>)))
                (*delayed (lambda() (*parser <InfixSub>)))
                (*delayed (lambda() (*parser <InfixMul>)))
                (*delayed (lambda() (*parser <InfixDiv>)))
                (*delayed (lambda() (*parser <InfixPow>)))
                (*delayed (lambda() (*parser <InfixArrayGet>)))
                (*delayed (lambda() (*parser <InfixFuncall>)))
                (*delayed (lambda() (*parser <InfixParen>)))
                (*delayed (lambda() (*parser <InfixSexprEscape>)))
                (*delayed (lambda() (*parser <InfixSymbol>)))
                (*delayed (lambda() (*parser <InfixNumber>)))
                (*disj 12)
        done))
        
(define <InfixPrefixExtensionPrefix>
        (new    (*parser (char #\#))
                (*parser (char #\#))
                (*caten 2)
                (*parser (char #\#))
                (*parser (char #\%))
                (*caten 2)
                (*disj 2)
        done))
        
(define <InfixSymbol>
        (new    (*parser <Symbol>)
                (*parser (char #\+))
                (*parser (char #\-))
                (*parser (char #\*))
                (*parser (char #\^))
                (*parser (char #\/))
                (*parser (char #\*))
                (*parser (char #\*))
                (*caten 2)
                (*disj 6)
                *diff
        done))
        
(define <InfixAdd>
        (new    (*parser <InfixExpression>)
                (*parser (char #\+))
                (*parser <InfixExpression>)
                (*caten 3)
        done))
        
(define <InfixNeg>
        (new    (*parser (char #\-))
                (*parser <InfixExpression>)
                (*caten 2)
        done))
      
(define <InfixSub>
        (new    (*parser <InfixExpression>)
                (*parser (char #\-))
                (*parser <InfixExpression>)
                (*caten 3)
        done))
        
(define <InfixMul>
        (new    (*parser <InfixExpression>)
                (*parser (char #\*))
                (*parser <InfixExpression>)
                (*caten 3)
        done))
        
(define <InfixDiv>
        (new    (*parser <InfixExpression>)
                (*parser (char #\/))
                (*parser <InfixExpression>)
                (*caten 3)
        done))
        
(define <PowerSymbol>
        (new    (*parser (char #\^))
                (*parser (char #\*))
                (*parser (char #\*))
                (*caten 2)
                (*disj 2)
        done))
        
(define <InfixPow>
        (new    (*parser <InfixExpression>)
                (*parser <PowerSymbol>)
                (*parser <InfixExpression>)
                (*caten 3)
        done))
        
(define <InfixArrayGet>
        (new    (*parser <InfixExpression>)
                (*parser (char #\[))
                (*parser <InfixExpression>)
                (*parser (char #\]))
                (*caten 4)
        done))
        
(define <InfixArgList>
        (new    (*parser <InfixExpression>)
                (*parser (char #\,))
                (*parser <InfixExpression>)
                (*caten 2)
                *star
                (*caten 2)
                (*parser <epsilon>)
                (*disj 2)
        done))
        
(define <InfixFuncall>
        (new    (*parser <InfixExpression>)
                (*parser (char #\())
                (*parser <InfixArgList>)
                (*parser (char #\)))
                (*caten 4)
        done))
        
(define <InfixParen>
        (new    (*parser (char #\())
                (*parser <InfixExpression>)
                (*parser (char #\)))
                (*caten 3)
        done))
                
(define <InfixSexprEscape>
        (new    (*parser <InfixPrefixExtensionPrefix>)
                (*delayed (lambda () <sexpr>))
                (*caten 2)
        done))
        
(define <InfixExtension>
        (new    (*parser <InfixPrefixExtensionPrefix>)
                (*parser <InfixExpression>)
                (*caten 2)
        done))
        
(define <sexpr>
	(new    (*parser <Boolean>)
	        (*parser <Number>)
	        (*parser <Char>)
		    (*parser <String>)
		    (*parser <Symbol>)
		    (*parser <ProperList>)
		    (*parser <ImproperList>)
		    (*parser <Vector>)
		    (*parser <Quoted>)
		    (*parser <QuasiQuoted>)
		    (*parser <Unquoted>)
		    (*parser <UnquotedAndSpliced>)
		    (*parser <InfixExtension>)
		    (*disj 13)
	     done))
