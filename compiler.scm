(load "./pc.scm")
		
(define <CharPrefix>
      (new     (*parser (char #\#))
               (*parser (char #\\))
               (*caten 2)
       done))
       
(define <HexChar>
	(new    (*parser (range #\0 #\9))
	        (*parser (range #\a #\f))
		(*disj 2) 
	     done))
     

(define <VisibleSimpleChar> (range #\! #\xff))


(define <NamedChar>
	(new    (*parser (char #\x03BB))
	        (*parser (char #\x12))
                (*parser (char #\nul))
               ; (*parser (char #\page)) 
	        (*parser (char #\return))
	        (*parser (char #\space))
	        (*parser (char #\tab))
		(*disj 6) ;add one after page is completed
	     done))
	     
(define <HexUnicodeChar>
	(new    (*parser (char #\x))
	        (*parser <HexChar>) *star
		(*caten 2) 
	     done))
	     
(define <Natural>
	(new    (*parser (range #\0 #\9)) *plus
         done))
	     
(define <Integer>
	(new    (*parser (char #\+))
                (*parser (char #\-))
		(*disj 2)
		(*parser (<Natural>))
		(*parser (*caten 2))
		(*parser (<Natural>))
		(*disj 2)
	     done))
	     
(define <Fraction>
	(new    (*parser (<Integer>))
		(*parser (<Natural>))
		(*disj 2)
	     done))

(define <Number>
	(new    (*parser <Integer>)
	        (*parser <Fraction>)
		(*disj 2) 
	     done))
	     
(define <StringVisibleChar> 
            (range #\space #\xff)
	     done))
	     
	     
(define <StringMetaChar>
	(new    (*parser (char #\\))  ;ask Mayer after
		(*parser (char #\"))
		(*parser (char #\t))
		(*parser (char #\f))
		(*parser (char #\n))
		(*parser (char #\r))
		(*disj 6)
	     done))

(define <StringHexChar>
	(new    (*parser (char #\))
                (*parser (char #\x))
                (*caten 2)
		(*parser (<HexChar>) *star
		(*parser (char #\;))
                (*caten 3)
	     done))
	     
(define <StringChar>
        (new    (*parser <StringVisibleChar>)
                (*parser <StringHexChar)
                (*parser <StringMetaChar>)
                (*disj 3)
             done))
	     
(define <String>
	(new    (*parser (char #\"))
		(*parser (<StringChar>)) *star
		(*parser (char #\"))
		(*caten 3)
	     done))	
	     
(define <Symbol>
        (new    (*parser <SymbolChar>) *plus
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

(define <ProperList>
        (new    (*parser (char #\())
                (*parser <sexpr>) *star
                (*parser (char #\)))
                (*caten 3)
        done))

(define <ImproperList>
        (new    (*parser (char #\())
                (*parser <sexpr>) *plus
                (*parser (char #\.)
                (*parser (<sexpr>))
                (*parser (char #\)))
                (*caten 5)
        done))
        
(define <Vector>
        (new    (*parser (char #\#))
                (*parser (char #\())
                (*parser (<sexpr>)) *star
                (*parser (char #\)))
                (*caten 4)
        done))

(define <Quoted>
        (new    (*parser (char #\'))
                (*parser (<sexpr>))
                (*caten 2)
        done))
        
(define <QuasiQuoted>
        (new    (*parser (char #\`))
                (*parser (<sexpr>))
                (*caten 2)
        done))
        
(define <Unquoted>
        (new    (*parser (char #\,))
                (*parser (<sexpr>))
                (*caten 2)
        done))        
        
(define <UnquotedAndSpliced>
        (new    (*parser (char #\,))
                (*parser (char #\@))
                (*parser (<sexpr>))
                (*caten 3)
        done))
        
(define <InfixExtension>
        (new    (*parser (<InfixPrefixExtensionPrefix>))
                (*parser (<InfixExpression>))
                (*caten 2)
        done))
        
(define <InfixPrefixExtensionPrefix
        (new    (*parser (char #\#))
                (*parser (char #\#))
                (*caten 2)
                (*parser (char #\#))
                (*parser (char #\%))
                (*caten 2)
                (*disj 2)
        done))
        
(define <InfixPrefixExtensionPrefix
        (new    (*parser (<InfixAdd>))
                (*parser (<InfixNeg>))
                (*parser (<InfixSub>))
                (*parser (<InfixMul>))
                (*parser (<InfixDiv>))
                (*parser (<InfixPow>))
                (*parser (<InfixArrayGet>))
                (*parser (<InfixFuncall>))
                (*parser (<InfixParen>))
                (*parser (<InfixSexprEscape>))
                (*parser (<InfixSymbol>))
                (*parser (<InfixNumber>))
                (*disj 12)
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
        (new    (*parser <InfixExpression)
                (*parser (char #\+))
                (*parser <InfixExpression)
                (*caten 3)
        done))
        
(define <InfixNeg>
        (new    (*parser (char #\-))
                (*parser <InfixExpression)
                (*caten 2)
        done))
      
(define <InfixSub>
        (new    (*parser <InfixExpression)
                (*parser (char #\-))
                (*parser <InfixExpression)
                (*caten 3)
        done))
        
(define <InfixMul>
        (new    (*parser <InfixExpression)
                (*parser (char #\*))
                (*parser <InfixExpression)
                (*caten 3)
        done))
        
(define <InfixDiv>
        (new    (*parser <InfixExpression)
                (*parser (char #\/))
                (*parser <InfixExpression)
                (*caten 3)
        done))
        
(define <InfixPow>
        (new    (*parser <InfixExpression)
                (*parser <PowerSymbol>)
                (*parser <InfixExpression)
                (*caten 3)
        done))
        
(define <PowerSymbol>
        (new    (*parser (char #\^))
                (*parser (char #\*))
                (*parser (char #\*))
                (*caten 2)
                (*disj 2)
        done))
        
(define <InfixArrayGet>
        (new    (*parser <InfixExpression>)
                (*parser (char #\[))
                (*parser <InfixExpression>)
                (*parser (char #\]))
                (*caten 4)
        done))
        
(define <InfixFuncall>
        (new    (*parser <InfixExpression>)
                (*parser (char #\())
                (*parser <InfixArgList>)
                (*parser (char #\)))
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
        
(define <InfixParen>
        (new    (*parser (char #\())
                (*parser <InfixExpression>)
                (*parser (char #\)))
                (*caten 3)
        done))
                
(define <InfixSexprEscape>
        (new    (*parser <InfixPrefixExtensionPrefix>)
                (*parser <sexpr>)
                (*caten 2)
        done))
        
(define <Boolean>
	(new    (*parser (char #\#))
	        (*parser (char #\t))
	        (*caten 2)
	        (*parser (char #\#))
	        (*parser (char #\f))
	        (*caten 2)
		(*disj 2)
	     done))

(define <Char>
	(new    (*parser <CharPrefix>)
	        (*parser <VisibleSimpleChar>)
                (*parser <NamedChar>)
                (*parser <HexUnicodeChar>)
                (*disj 3)
                (*caten 2)
	     done))
        
(define <sexpr>
	(new    (*parser <Boolean>)
	        (*parser <Char>)
	        (*parser <Number>)
		(*parser <String>)
		(*parser <Symbol>)
		(*parser <ProperList>)
		(*parser <ImproperList>)
		(*parser <Vector>)
		(*parser <Quoted>)
		(*parser <QuasiQuoted>)
		(*parser <Unquoted>)
		(*parser <UnquoteAndSpliced>)
		(*parser <InfixExtension>)
		(*disj 13)
	     done))