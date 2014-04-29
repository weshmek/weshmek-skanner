#lang racket

(require (for-syntax racket/syntax))
(require (for-syntax racket/base))
(require (for-syntax racket/list))


(define-syntax (regular-expression-helper stx)
  (syntax-case stx (&& || * Epsilon)
    [(_ Epsilon) #'(lambda (result lst)
                     (list (list result lst)))]
    [(_ (exp *)) #'(lambda (result lst)
                     (cons (list result lst)
                           (letrec ([f (lambda (result lst)
                                         (let ([set (filter (lambda (x) (not (empty? x))) ((regular-expression-helper exp) result lst))])
                                           (if (empty? set) empty (append set (apply append (map (curry apply f) set))))))])
                             (f result lst))))]
    [(_ (|| fst ...)) #'(lambda (result lst)
                          (append ((regular-expression-helper fst) result lst) ...))]
    
    [(_ (&& only)) #'(lambda (result lst)
                       ((regular-expression-helper only) result lst))]
    [(_  (&& fst rst ...)) #'(lambda (result lst)
                               (let ([set (filter (lambda (x) (not (empty? x))) ((regular-expression-helper fst) result lst))])
                                 (if (empty? set) empty (apply append (map (curry apply (regular-expression-helper (&& rst ...))) set)) )))]
    
    [(_ (character)) #'(regular-expression-helper character)]
    [(_ character) #'(lambda (result lst)
                       (if (empty? lst) empty (if (equal? (first lst) character) (list (list (cons character result) (rest lst))) empty)))]))

(define-syntax (regular-expression stx) 
  (syntax-case stx ()
    [(_ rgx ...)
     #'(lambda (lst)
         (let ([ret (foldr (lambda (x y) (if (>= (length (first x)) (length (first y))) x y)) (list empty lst) 
                           ((regular-expression-helper rgx ...) empty lst))])
           (list (reverse (first ret)) (second ret))))]))
  


(define the-regex (regular-expression (&& (#\t) (#\h) (#\e))))

(the-regex (list #\t #\h #\e))
(the-regex (list #\t #\h #\a #\n))
(the-regex (list #\t #\h #\e #\r #\e))

(define bool-regex (regular-expression (|| (&& (#\t) (#\r) (#\u) (#\e)) (&& (#\f) (#\a) (#\l) (#\s) (#\e)))))

(bool-regex (list #\t #\r #\u #\e #\d #\a #\t))

(bool-regex (list #\f #\a #\l #\s #\e))
(bool-regex (list #\f #\a #\l #\s))

(define ones-regex (regular-expression ((#\1) *)))

(ones-regex '())
(ones-regex (list #\1 #\1 #\1 #\2))
(ones-regex (list #\2))


(define int-lit (regular-expression (|| (#\0) (&& (|| (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) 
                                                  ((|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) *)))))


(int-lit (list #\0))
(int-lit (list #\0 #\1 #\2))
(int-lit (list #\3 #\1 #\4 #\1 #\5 #\9))


(define sign-lit (regular-expression (|| Epsilon #\+ #\-)))

(sign-lit (list #\+))
(sign-lit (list #\-))
(sign-lit (list #\9))
;;TODO : ADD "Illegal Following Chars" To the Regexes so we can identify errors appropriately - 
;;For example, "012" above matches 0 and gives back 12 as the remainder. 
;Numerical characters should not immediately follow a number. Alphanumeric characters should not follow an identifier, etc...
;;Possibly such anomalies would be picked up by the parser


(define-syntax (scanner stx) 
  (syntax-case stx (scanner)
    [(scanner scanner-name error-symbol
      [regex output-symbol] ...)
     #'(define (scanner-name lst)
         (cond
           [(empty? lst) empty]
           [else (let ([next-token (foldr (lambda (x y) (if (and (> (length (first (first x))) 0) 
                                                                 (>= (length (first (first x))) (length (first (first y))))) 
                                                            
                                                            x y)) 
                                          (list  (list empty lst) error-symbol) 
                                          (append (list (list ((regular-expression regex) lst) output-symbol)) ...))])
                   (if (equal? (second next-token) error-symbol) (cons (list (first (first next-token)) error-symbol) 
                                                                       (cons (second (first next-token)) empty)) 
                       (cons (list (first (first next-token)) (second next-token)) (scanner-name (second (first next-token))))))]))]))
    


(scanner boolean-scanner 'error
         [(|| (&& (#\t) (#\r) (#\u) (#\e)) (&& (#\f) (#\a) (#\l) (#\s) (#\e))) 'boolean]
         [((|| (#\newline) (#\space) (#\tab) (#\return)) *) 'space])

(boolean-scanner (string->list "true 

\tfalse"))

(boolean-scanner (string->list "true 3.14159"))

(define-syntax (whitespace-no-newline stx)
    #'(|| #\space #\tab))

(define-syntax (whitespace stx)
  #'(|| (#\space) (#\newline) (#\tab) (#\return)))
(define-syntax (no-star-slash stx)
  #'(|| 
     (#\a) (#\b) (#\c) (#\d) (#\e) (#\f) (#\g)(#\h) #\i (#\j) (#\k) (#\l) (#\m) (#\n) (#\o) (#\p) (#\q) (#\r) (#\s) (#\t) (#\u) (#\v) (#\w) (#\x) (#\y) 
     (#\z)))

(scanner comment-scanner 'error
         [(|| (#\space) (#\newline) (#\tab) (#\return)) 'whitespace]
         [(&& (#\/) (#\/) ((|| (|| (#\a) (#\b) (#\c) (#\d) (#\e) (#\f) (#\g)(#\h) (#\i) (#\j) (#\k) (#\l) (#\m) (#\n) (#\o) (#\p) (#\q) (#\r) (#\s) 
                                   (#\t) (#\u) (#\v) 
                                   (#\w) (#\x) (#\y) (#\z)) (|| (#\space) (#\tab))) *) (|| (#\newline) (#\return))) 'single-line-comment]
         [(&& (#\/) (#\*) ((|| (|| (#\a) (#\b) (#\c) (#\d) (#\e) (#\f) (#\g)(#\h) (#\i) (#\j) (#\k) (#\l) (#\m) (#\n) (#\o) (#\p) (#\q) 
                                   (#\r) (#\s) (#\t) (#\u) (#\v) (#\w) (#\x) (#\y) (#\z)) (|| (#\space) (#\newline) (#\tab) (#\return))) *) 
              (#\*) (#\/)) 'block-comment])

(comment-scanner (string->list "//this is a line comment

/*this is
a block comment*/"))

;(scanner whitespace-scanner 'error
;         [(whitespace-no-newline) 'whitespace-no-newline]
;         [(whitespace) 'whitespace])


(define-syntax (string-list stx)
  (syntax-case stx ()
    [(_ string) (datum->syntax stx (string->list (syntax->list #'string)))]))
;((|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) *)





(scanner java-scanner 'error 
         ;;First, Parentheses
         [#\{ 'ocurl]
         [#\} 'ccurl]
         [#\( 'oparen]
         [#\) 'cparen]
         [#\[ 'obrac]
         [#\] 'cbrac]
         
         
         ;;Operators:
         
         [(#\=) 'eq]
         [(&& #\= #\=) 'eqeq]
         [#\+ 'plus]
         [(&& #\+ #\=) 'pluseq]
         [(#\<) 'lt]
         [(&& #\< #\=) 'le]
         [#\- 'minus]
         [(&& #\- #\=) 'minuseq]
         
         [#\> 'gt]
         [(&& #\> #\=) 'ge]
         [#\* 'star]
         [(&& #\* #\=) 'stareq]
         
         [#\! 'bang]
         [(&& #\! #\=) 'bangeq]
         
         [(#\/) 'slash]
         [(&& #\/ #\=) 'slasheq]
         [(#\~) 'tilde]
         [(&& #\& #\&) 'andand]
         [(&& #\&) 'and]
         [(&& #\& #\=) 'andeq]
         [(#\?) 'question-mark]
         [(&& #\| #\|) 'barbar]
         [#\| 'bar]
         [(&& #\| #\=) 'bareq]
         [#\: 'colon]
         [(&& #\+ #\+) 'plusplus]
         [#\^ 'hat]
         [(&& #\^ #\=) 'hateq]
         [(&& #\- #\-) 'minusminus]
         [(#\%)'pct]
         [(&& #\% #\=) 'pcteq]
         [(&& #\< #\<) 'ltlt]
         [(&& #\< #\< #\=) 'ltlteq]
         [(&& #\> #\>) 'gtgt]
         [(&& #\> #\> #\=) 'gtgteq]
         [(&& #\> #\> #\>) 'gtgtgt]
         [(&& #\> #\> #\> #\=) 'gtgtgteq]
         ;;Separators:
         [(#\;) 'semi]
         [(#\.) 'dot]
         [#\, 'comma]
         ;;Now, the keywords
         [(&& #\a #\b #\s #\t #\r #\a #\c #\t) 'abstract]
         [(&& #\b #\o #\o #\l #\e #\a #\n) 'boolean]
         [(&& #\b #\r #\e #\a #\k) 'break]
         [(&& #\c #\a #\s #\e) 'case]
         [(&& #\c #\a #\t #\c #\h) 'catch]
         [(&& #\c #\l #\a #\s #\s) 'class]
         [(&& #\c #\o #\n #\s #\t) 'const]
         [(&& #\d #\e #\f #\a #\u #\l #\t) 'default]
         [(&& #\d #\o) 'do]
         [(&& #\e #\x #\t #\e #\n #\d #\s) 'extends]
         [(&& #\f #\i #\n #\a #\l) 'final]
         [(&& #\f #\i #\n #\a #\l #\l #\y) 'finally]
         
         [(&& #\g #\o #\t #\o) 'goto]
         
         [(&& #\i #\m #\p #\l #\e #\m #\e #\n #\t #\s) 'implements]
         [(&& #\i #\m #\p #\o #\r #\t) 'import]
         [(&& #\i #\n #\s #\t #\a #\n #\c #\e #\o #\f) 'instanceof]
         
         [(&& #\i #\n #\t #\e #\r #\f #\a #\c #\e) 'interface]
         
         [(&& #\n #\a #\t #\i #\v #\e) 'native]
         [(&& #\n #\e #\w) 'new]
         
         [(&& #\p #\a #\c #\k #\a #\g #\e) 'package]
         
         [(&& #\p #\r #\i #\v #\a #\t #\e) 'private]
         [(&& #\p #\r #\o #\t #\e #\c #\t #\e #\d) 'protected]
         [(&& #\p #\u #\b #\l #\i #\c) 'public]
         
         [(&& #\s #\t #\a #\t #\i #\c) 'static]
         
         [(&& #\s #\u #\p #\e #\r) 'super]
         
         [(&& #\s #\w #\i #\t #\c #\h) 'switch]
         [(&& #\t #\h #\i #\s) 'this]
         
         [(&& #\t #\h #\r #\o #\w) 'throw]
         [(&& #\t #\h #\r #\o #\w #\s) 'throws]
         
         [(&& #\t #\r #\y) 'try]
         [(&& #\v #\o #\i #\d) 'void]
         [(&& #\v #\o #\l #\a #\t #\i #\l #\e) 'volatile]
         
         [(&& #\r #\e #\t #\u #\r #\n) 'return]
         ;[(string-list "goto") 'goto]
         [(&& #\w #\h #\i #\l #\e) 'while]
         [(&& #\f #\o #\r) 'for]
         [(&& #\i #\f) 'iff]
         [(&& #\e #\l #\s #\e) 'else]
         [(&& #\i #\n #\t) 'int]
         [(&& #\c #\h #\a #\r) 'char]
         [(&& #\b #\y #\t #\e) 'byte]
         [(&& #\s #\h #\o #\r #\t) 'short]
         [(&& #\f #\l #\o #\a #\t) 'float]
         [(&& #\d #\o #\u #\b #\l #\e) 'double]
         
         
         ;;Identifier:
         [(&& (|| #\$ #\_ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                  #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z) 
              ((|| (|| #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z 
                       #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z) 
                   (|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) #\_ #\$) *))  'identifier] 
         
         ;;Literals:
         
         [(&& #\" 
              
              ((|| #\nul  #\u0001  #\u0002  #\u0003  #\u0004  #\u0005  #\u0006  #\u0007  #\backspace  #\tab  #\newline  #\vtab  #\page  #\return  
                        #\u000E  #\u000F  #\u0010  #\u0011  #\u0012  #\u0013  #\u0014  #\u0015  #\u0016  #\u0017  #\u0018  #\u0019  #\u001A  #\u001B  
                        #\u001C  #\u001D  #\u001E  #\u001F  #\space  #\!  #\#  #\$  #\%  #\&  #\'  #\(  #\)  #\*  #\+  #\,  #\-  #\.  #\/  #\0  #\1  #\2  
                        #\3  #\4  #\5  #\6  #\7  #\8  #\9  #\:  #\;  #\<  #\=  #\>  #\?  #\@  #\A  #\B  #\C  #\D  #\E  #\F  #\G  #\H  #\I  #\J  #\K  #\L  
                        #\M  #\N  #\O  #\P  #\Q  #\R  #\S  #\T  #\U  #\V  #\W  #\X  #\Y  #\Z  #\[  #\]  #\^  #\_  #\`  #\a  #\b  #\c  #\d  #\e  #\f  
                        #\g  #\h  #\i  #\j  #\k  #\l  #\m  #\n  #\o  #\p  #\q  #\r  #\s  #\t  #\u  #\v  #\w  #\x  #\y  #\z  #\{  #\|  #\}  #\~ #\rubout
                        (&& #\\ (|| #\b #\t #\n #\f #\r #\" #\' #\\))
                        (&& #\\ (|| #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
                        (&& #\\ (|| #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) (|| #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
                        (&& #\\ (|| #\0 #\1 #\2 #\3) (|| #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) (|| #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))) *)
                        

              
              #\")
          'string-lit]
         
         [(&& #\'
              
              ((|| #\nul  #\u0001  #\u0002  #\u0003  #\u0004  #\u0005  #\u0006  #\u0007  #\backspace  #\tab  #\newline  #\vtab  #\page  #\return  
                        #\u000E  #\u000F  #\u0010  #\u0011  #\u0012  #\u0013  #\u0014  #\u0015  #\u0016  #\u0017  #\u0018  #\u0019  #\u001A  #\u001B  
                        #\u001C  #\u001D  #\u001E  #\u001F  #\space  #\!  #\#  #\$  #\%  #\&  #\"  #\(  #\)  #\*  #\+  #\,  #\-  #\.  #\/  #\0  #\1  #\2  
                        #\3  #\4  #\5  #\6  #\7  #\8  #\9  #\:  #\;  #\<  #\=  #\>  #\?  #\@  #\A  #\B  #\C  #\D  #\E  #\F  #\G  #\H  #\I  #\J  #\K  #\L  
                        #\M  #\N  #\O  #\P  #\Q  #\R  #\S  #\T  #\U  #\V  #\W  #\X  #\Y  #\Z  #\[  #\]  #\^  #\_  #\`  #\a  #\b  #\c  #\d  #\e  #\f  
                        #\g  #\h  #\i  #\j  #\k  #\l  #\m  #\n  #\o  #\p  #\q  #\r  #\s  #\t  #\u  #\v  #\w  #\x  #\y  #\z  #\{  #\|  #\}  #\~ #\rubout
                        (&& #\\ (|| #\b #\t #\n #\f #\r #\" #\' #\\))
                        (&& #\\ (|| #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
                        (&& #\\ (|| #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) (|| #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
                        (&& #\\ (|| #\0 #\1 #\2 #\3) (|| #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) (|| #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))) *)
                        

              
              #\')
          'char-lit]
              
         
         [(&& #\n #\u #\l #\l) 'null]
         [(&& #\t #\r #\u #\e) 'true]
         [(&& #\f #\a #\l #\s #\e) 'false]
         [(&& (|| (#\0) (&& (|| (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) ((|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) *))) (|| Epsilon #\l #\L)) 'decimal-lit]
         
         [(&& #\0 (|| #\x #\X) ((|| #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f) *)) 'hex-lit]
         
         [(&& #\0 ((|| #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) *)) 'octal-lit]
         
         [(&& #\/ #\/ ((|| #\nul  #\u0001  #\u0002  #\u0003  #\u0004  
                           #\u0005  #\u0006  #\u0007  #\backspace  #\tab 
                           #\vtab  #\page #\u000E  #\u000F  #\u0010  
                           #\u0011  #\u0012  #\u0013  #\u0014  #\u0015  
                           #\u0016  #\u0017  #\u0018  #\u0019  #\u001A  
                           #\u001B  #\u001C  #\u001D  #\u001E  #\u001F  
                           #\space  #\!  #\"  #\#  #\$  #\%  #\&  #\'  
                           #\(  #\)  #\*  #\+  #\,  #\-  #\.  #\/  #\0  
                           #\1  #\2  #\3  #\4  #\5  #\6  #\7  #\8  #\9  
                           #\:  #\;  #\<  #\=  #\>  #\?  #\@  #\A  #\B  
                           #\C  #\D  #\E  #\F  #\G  #\H  #\I  #\J  #\K  
                           #\L  #\M  #\N  #\O  #\P  #\Q  #\R  #\S  #\T  
                           #\U  #\V  #\W  #\X  #\Y  #\Z  #\[  #\\  #\]  
                           #\^  #\_  #\`  #\a  #\b  #\c  #\d  #\e  #\f  
                           #\g  #\h  #\i  #\j  #\k  #\l  #\m  #\n  #\o  
                           #\p  #\q  #\r  #\s  #\t  #\u  #\v  #\w  #\x  
                           #\y  #\z  #\{  #\|  #\}  #\~ #\rubout) *))
          'line-comment]

         
         [(&& #\/ 
              
               ((&& #\* (|| #\nul  #\u0001  #\u0002  #\u0003  #\u0004  
                            #\u0005  #\u0006  #\u0007  #\backspace  
                            #\tab  #\newline  #\vtab  #\page  #\return  
                            #\u000E  #\u000F  #\u0010  #\u0011  #\u0012  
                            #\u0013  #\u0014  #\u0015  #\u0016  #\u0017  
                            #\u0018  #\u0019  #\u001A  #\u001B  #\u001C  
                            #\u001D  #\u001E  #\u001F  #\space  #\!  #\"
                            #\#  #\$  #\%  #\&  #\'  #\(  #\)  #\+  #\,
                            #\-  #\.   #\0  #\1  #\2  #\3  #\4  #\5  #\6
                            #\7  #\8  #\9  #\:  #\;  #\<  #\=  #\>  #\? 
                            #\@  #\A  #\B  #\C  #\D  #\E  #\F  #\G  #\H
                            #\I  #\J  #\K  #\L  #\M  #\N  #\O  #\P  #\Q
                            #\R  #\S  #\T  #\U  #\V  #\W  #\X  #\Y  #\Z 
                            #\[  #\\  #\]  #\^  #\_  #\`  #\a  #\b  #\c
                            #\d  #\e  #\f  #\g  #\h  #\i  #\j  #\k  #\l
                            #\m  #\n  #\o  #\p  #\q  #\r  #\s  #\t  #\u
                            #\v  #\w  #\x  #\y  #\z  #\{  #\|  #\}  #\~ 
                            #\rubout) (#\/ *)) *)
               ((&& #\* 
                    ((&& (|| #\nul  #\u0001  #\u0002  #\u0003  #\u0004  
                             #\u0005  #\u0006  #\u0007  #\backspace  
                             #\tab  #\newline  #\vtab  #\page  #\return  
                             #\u000E  #\u000F  #\u0010  #\u0011  #\u0012  
                             #\u0013  #\u0014  #\u0015  #\u0016  #\u0017  
                             #\u0018  #\u0019  #\u001A  #\u001B  #\u001C  
                             #\u001D  #\u001E  #\u001F  #\space  #\!  #\"
                             #\#  #\$  #\%  #\&  #\'  #\(  #\)  #\+  #\,
                             #\-  #\.   #\0  #\1  #\2  #\3  #\4  #\5  #\6
                             #\7  #\8  #\9  #\:  #\;  #\<  #\=  #\>  #\? 
                             #\@  #\A  #\B  #\C  #\D  #\E  #\F  #\G  #\H
                             #\I  #\J  #\K  #\L  #\M  #\N  #\O  #\P  #\Q
                             #\R  #\S  #\T  #\U  #\V  #\W  #\X  #\Y  #\Z 
                             #\[  #\\  #\]  #\^  #\_  #\`  #\a  #\b  #\c
                             #\d  #\e  #\f  #\g  #\h  #\i  #\j  #\k  #\l
                             #\m  #\n  #\o  #\p  #\q  #\r  #\s  #\t  #\u
                             #\v  #\w  #\x  #\y  #\z  #\{  #\|  #\}  #\~ 
                             #\rubout)
                         (#\/ *)) *)) *)
              #\*
              #\/) 'block-comment]
         
         

         
         ;;Floating-point literals are a doozy
         [(|| 
           (&& 
            ;;Digits:
            (|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) 
            ((|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) *) 
            ;;EponentPart(Optional)
            (&& (|| Epsilon (|| #\E #\e) 
                    ;;Sign(Optional)
                    (|| Epsilon #\+ #\-) 
                    ;;Digits:
                    (|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) 
                    ((|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) *)) 
                ;;FloatTypeSuffix:
                (|| #\f #\F #\d #\D))) 
           
           (&& 
            ;;Digits:
            
            (|| #\0 (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) 
            ((|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) *) 
            ;;ExponentPart:
            (&& (|| #\E #\e) 
                ;;Sign(Optional):
                (|| Epsilon #\+ #\-) 
                ;;Digits:
                (|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) 
                ((|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) *) 
                ;;FloatTypeSuffix(Optional:)
                (|| Epsilon #\f #\F #\d #\D)))  
           ;;Digits:
           (&& #\. 
               (|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) 
               ((|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) *) 
               ;;ExponentPart(Optional)
               (|| Epsilon (&& (|| #\E #\e) 
                               ;;Sign (Optional)
                               (|| Epsilon #\+ #\-) 
                               ;;Digits:
                               (|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) 
                               ((|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) *))) 
               ;;FloatTypeSuffix(Optional)
               (|| Epsilon #\f #\F #\d #\D)) 
           (&& 
            ;;Digits:
            (|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) 
            ((|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) *) 
            
            ;;Decimal
            #\.
            ;;Digits(Optional):
            ((|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) *) 
            ;;ExponentPart(Optional)
            (|| Epsilon (&& (|| #\E #\e) 
                            ;;Sign(Optional)
                            (|| Epsilon #\+ #\-) 
                            ;;Digits:
                            (|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) 
                            ((|| (#\0) (#\1) (#\2) (#\3) (#\4) (#\5) (#\6) (#\7) (#\8) (#\9)) *))) 
            ;;FloatTypeSuffix(Optional)
            (|| Epsilon #\f #\F #\d #\D))) 
          
          'float-literal]
         
         ;;Whitespace:
         [((|| #\space #\tab #\newline #\return) *) 'whitespace]
         
         
         )
;;line terminators: (|| #\newline #\return (&& #\return #\newline))
(java-scanner (string->list "for (int;) { return ; }"))
(java-scanner (string->list "for (int = 0; ;) { 200 = 32l + 1L;}"))

(java-scanner (string->list "for (int i = 0; i <= 7; i++) { j1identifier[i] = i;}"))
(java-scanner (string->list "double pi = 3.14159"))

(java-scanner (string->list "1e1f 2.f .3f 0f 2.14f 6.022137e+23f 1e1 2. .3 0.0 3.14 1e-9d 1e137"))

(java-scanner (string->list "/*this is a block comment 
 /* block comments do not nest */ 
 //this is a line comment
/*this is another block comment */
/* this comment /* // /* ends here: */
/*/
/*this comment is incomplete"))

(java-scanner (string->list "\"this is a string\"
't''h''e''s''e'a're char literals"))


;;TODO : See if we can't error out obvious cases like unclosed block comments
