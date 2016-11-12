(load "./pc.scm")

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

(define <Boolean>
	(new    (*parser (char #\#t))
	        (*parser (char #\#f)
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
		
(define <CharPrefix>
         (new   (*char #\#\)
          done))

(define <VisibleSimpleChar>
         (new   (*char #\#\)
          done))
          
          eli simhayev is gay

