#lang racket

;;Wesley Chalmers: This scanner works. It works better than the scanner we spent 30 hours working on. Good job. Sincerely, Wesley Chalmers

;;The non-macro version may have advantages. Possibly, procedures aren't created unless they might be needed. 
;;We can use things like string->list and such to avoid writing the specific syntax
(struct regex-and (regexes))

(struct regex-or (regexes))

(struct regex-star (regex))

(define not-empty? (compose not empty?))
(define filter-empty (curry filter not-empty?)) 



(define (regex-helper rgx)
  (define epsilon (lambda (result lst) (list (list result lst))))
  (match rgx
    ['epsilon epsilon]
    [(regex-and regexes)
     ;(printf "regex-and ~a~n" regexes)
     (cond
       [(empty? regexes) epsilon]
       [else
        (lambda (result lst)
          (let ([set (filter-empty ((regex-helper (first regexes)) result lst))])
            (if (empty? set) empty (apply append (map (curry apply (regex-helper (regex-and (rest regexes)))) set)))))])]
    [(regex-or regexes)
     (lambda (result lst)
       (foldr (lambda (f y) (append (f result lst) y)) empty (map regex-helper regexes)))]
    [(regex-star regex) 
                          (lambda (result lst)
                            (cons (list result lst) (letrec ([g (regex-helper regex)]
                                                             [f (lambda (result lst)
                                                                  (let ([set (filter-empty (g result lst))])
                                                                    (if (empty? set) empty (apply append set (map (curry apply f) set)))))])                                                  
                                                      (f result lst))))]
    [character 
     ;(printf "regex-character ~a~n" character)
     (lambda (result lst)
       ;(printf "result ~a~nlst ~a~n" result lst)
       (if (empty? lst) empty (if (equal? (first lst) character) (list (list (cons character result) (rest lst))) empty)))]))


(define (regex rgx)
  (lambda (lst)
    (let* ([set ((regex-helper rgx) empty lst)]
           [ret (foldr (lambda (x y) (if (>= (length (first x)) (length (first y))) x y)) (list empty lst) 
                       set)])
      ;(printf "set: ~a~n" set)
      (list (reverse (first ret)) (second ret)))))


(define the-regex (regex (regex-and '(#\t #\h #\e))))

(the-regex (string->list "the"))
(the-regex (string->list "then"))
(the-regex (string->list "than"))


(define (non-macro-scanner error-symbol regex-list)
  (define regex-fns (map (lambda (pr) (list (regex (first pr)) (second pr))) regex-list))
  (define (helper lst)
    (cond
      [(empty? lst) empty]
      [else (let ([next-token (foldr (lambda (x y) (if (and (> (length (first (first x))) 0)
                                                            (>= (length (first (first x))) (length (first (first y)))))
                                                       x y))
                                     (list (list empty lst) error-symbol)
                                     (map (lambda (pr) (list ((first pr) lst) (second pr))) regex-fns))])
              (if (equal? (second next-token) error-symbol) 
                  (cons (list (first (first next-token)) error-symbol)
                        (cons (second (first next-token)) empty))
                  
                  (cons (list (first (first next-token)) (second next-token))  (helper (second (first next-token))))))]))
  helper)


(define boolean-scanner (non-macro-scanner 'error (list (list (regex-or (list (regex-and (string->list "true")) (regex-and (string->list "false")))) 'boolean) (list #\space 'space))))

(boolean-scanner (string->list "true"))
(boolean-scanner (string->list "false"))
(boolean-scanner (string->list "true false"))
(boolean-scanner (string->list "123"))


(define ones-regex (regex (regex-star #\1)))

(ones-regex (string->list "1"))
(ones-regex (string->list "11112"))
(ones-regex (string->list "2"))



(define all-ascii (regex-or (map integer->char (range 0 128))))
(define integer-type-suffix (regex-or (list 'epsilon #\L #\l)))
(define digits (regex-or (string->list "0123456789")))

(define octal-digits (regex-or (string->list "01234567")))
(define exponent-part
  (regex-and
   (list 
    (regex-or '(#\e #\E))
    (regex-or '(epsilon #\+ #\-))
    digits
    (regex-star digits))))

(define float-type-suffix
  (regex-or (list #\f #\F #\d #\D)))
(define (regex-opt rgx)
  (regex-or (list 'epsilon rgx)))

(define escape-sequences
  (regex-and (list #\\
                   (regex-or 
                    (list
                     #\b #\t #\n #\f #\r #\" #\' #\\
                     (regex-and
                      (list #\\ 
                            (regex-or
                             (list
                              octal-digits
                              (regex-and
                               (list octal-digits octal-digits))
                              (regex-and
                               (list (regex-or '(#\0 #\1 #\2 #\3)) octal-digits octal-digits)))))))))))

(define no-star-slash (regex-or (filter (lambda (c) (and (not (equal? c #\/)) (not (equal? c #\*)))) (map integer->char (range 0 128)))))
(define no-newline (regex-or (filter (lambda (c) (and (not (equal? c #\newline)) (not (equal? c #\return)))) (map integer->char (range 0 128)))))


(define-syntax-rule (operators  [char-list ... symb] ...)
  
  (list
   (list (regex-and (list char-list ...)) symb) ...))


(operators
 [#\= 'eq]
 [#\= #\= 'eqeq])

(define java-scanner (non-macro-scanner 'error 
                                        (append
                                         
                                         
                                         ;;separators
                                         '((#\( oparen) (#\) cparen) (#\{ ocurl) (#\} ccurl) (#\[ obrac) (#\] cbrac) (#\; semi) (#\, comma) (#\. dot))
                                         
                                         
                                         ;;Operators
                                         
                                         
                                         
                                         (operators
                                          (#\= 'eq)
                                          (#\= #\= 'eqeq) 
                                          (#\+ 'plus)
                                          (#\+ #\= 'pluseq)
                                          (#\> 'gt)
                                          (#\> #\= 'ge)
                                          (#\- 'minus)
                                          (#\- #\= 'minuseq)
                                          (#\< 'lt)
                                          (#\< #\= 'le)
                                          (#\* 'star)
                                          (#\* #\= 'stareq)
                                          (#\! 'bang)
                                          (#\! #\= 'bangeq)
                                          (#\/ 'slash)
                                          (#\/ #\= 'slasheq)
                                          (#\~ 'tilde)
                                          (#\& #\& 'ampamp)
                                          (#\& 'amp)
                                          (#\& #\= 'ampeq)
                                          (#\? 'question)
                                          (#\| #\| 'barbar)
                                          (#\| 'bar)
                                          (#\| #\= 'bareq)
                                          (#\: 'colon)
                                          (#\+ #\+ 'plusplus)
                                          (#\^ 'carot)
                                          (#\^ #\= 'caroteq)
                                          (#\- #\- 'minusminus)
                                          (#\% 'pct)
                                          (#\% #\= 'pcteq)
                                          (#\< #\< 'shl)
                                          (#\< #\< #\= 'shleq)
                                          (#\> #\> 'shr)
                                          (#\> #\> #\= 'shreq)
                                          (#\> #\> #\> 'ushr)
                                          (#\> #\> #\> #\= 'ushreq))
                                         
                                         (list
                                          (list (regex-or (list (regex-and (string->list "true")) (regex-and (string->list "false"))))
                                                'bool-lit)
                                          
                                          (list (regex-and (string->list "null"))
                                                'null-lit))
                                         ;;keywords
                                         (map (lambda (sym) (list (regex-and (string->list (symbol->string sym))) sym)) 
                                              '(abstract boolean break byte case catch char class const continue 
                                                         default do double else extends final finally float for goto
                                                         if implements import instanceof int interface long native new package
                                                         private public protected return short static strictfp super switch synchronized
                                                         this throw throws transient try void volatile while))
                                         
                                         ;;Identifiers
                                         
                                         (list (list (regex-and (list (regex-or (list #\$ #\_ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                                                                                      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                                                                                      #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)) 
                                                                      (regex-star (regex-or (list (regex-or (list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M 
                                                                                                                  #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z 
                                                                                                                  #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m 
                                                                                                                  #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)) 
                                                                                                  (regex-or (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) #\_ #\$))))) 
                                                     'identifier))
                                         
                                         
                                         
                                         
                                         ;;Comment
                                         (list
                                          (list
                                           (regex-and (list #\/ #\/ (regex-star no-newline) (regex-or (list #\newline #\return)))) 'comment)
                                          
                                          (list
                                           
                                           (regex-and (list #\/ 
                                                            
                                                            (regex-star (regex-and (list #\* (regex-and (list no-star-slash (regex-star #\/))))))
                                                            (regex-star (regex-and (list #\* 
                                                                                         (regex-star (regex-and (list no-star-slash
                                                                                                                      (regex-star #\/ )))))))
                                                            
                                                            #\*
                                                            #\/))
                                           'comment))
                                         
                                         ;;Literals
                                         (list 
                                          ;;Decimal
                                          (list (let ([non-zero-digit (regex-or (map integer->char (range 49 59)))]
                                                      [digit (regex-or (map integer->char (range 48 59)))])
                                                  (regex-and (list (regex-or (list #\0 (regex-and (list non-zero-digit (regex-star digit))))) integer-type-suffix)))
                                                'decimal-lit)
                                          ;;Hex
                                          (list (let ([hex-digit (regex-or (string->list "0123456789ABCDEFabcdef"))])
                                                  (regex-and (list #\0 (regex-or '(#\x #\X)) (regex-star hex-digit) integer-type-suffix)))
                                                'hex-lit)
                                          
                                          ;;Octal
                                          (list 
                                           (regex-and (list #\0 (regex-star (regex-or (string->list "01234567"))) integer-type-suffix))
                                           'octal-lit)
                                          
                                          ;;Float
                                          (list
                                           (regex-or 
                                            (list
                                             (regex-and 
                                              (list digits (regex-star digits) #\. (regex-star digits) (regex-opt exponent-part) (regex-opt float-type-suffix)))
                                             
                                             (regex-and
                                              (list #\. digits (regex-star digits) (regex-opt exponent-part) (regex-opt float-type-suffix)))
                                             
                                             (regex-and
                                              (list digits (regex-star digits) exponent-part (regex-opt float-type-suffix)))
                                             
                                             (regex-and 
                                              (list digits (regex-star digits) (regex-opt exponent-part) float-type-suffix))))
                                           'float-lit
                                           ))
                                         
                                         
                                         
                                         
                                         (list 
                                          (list
                                           (regex-and 
                                            (list #\" (regex-star (regex-or (append (list (regex-or (filter (lambda (c) (or (not (equal? c #\")) (not (equal? c #\\)))) 
                                                                                                            (regex-or-regexes all-ascii)))) (list escape-sequences))) ) #\"))
                                           'string-lit)
                                          (list 
                                           (regex-and
                                            (list #\' (regex-or (append (list (regex-or (filter (lambda (c) (or (not (equal? c #\')) (not (equal? c #\\)))) (regex-or-regexes all-ascii)))) 
                                                                        (list escape-sequences)))  #\'))
                                           'char-lit))
                                         
                                         
                                         ;;whitespace
                                         (list (list (regex-star (regex-or '(#\space #\newline #\tab #\return))) 'whitespace)))))



(java-scanner (string->list "boolean final finally catch throw"))


(java-scanner (string->list "31415926  0 0123 123 0x12A 0X1aB 0137 0137l 2552L"))

(java-scanner (string->list "true false 31415926 false"))
(java-scanner (string->list "1.1"))
(java-scanner (string->list "1.1 1e1f 2.f .3f 0f 3.14f 6.022137e+23f 1e1 2. .3 0.0 3.14 1e-9d 1e137"))
(java-scanner (string->list "1.d"))

(java-scanner (string->list "\"this is a string literal\\n\""))
(java-scanner '(#\' #\a #\'))

(java-scanner (string->list ";.,(){}[]"))
(java-scanner (string->list "id id2 id_3"))

(java-scanner (string->list "/*this is a block comment 
 /* block comments do not nest*/ 
 //this is a line comment
/*this is another block comment */
/* this comment /* // /* ends here: */
/*/
/*this comment is incomplete"))


(java-scanner (string->list "for (int i = 0; i <= 7; i++)
{
5 >>> x;
}"))

(java-scanner (string->list "\"this
is another string\\tliteral\""))


(java-scanner (string->list "x =  2"))

(java-scanner (string->list "1id2"))

(define hex-digits (regex-or (string->list "0123456789ABCDEFabcdef")))


(define hex-quad
  (regex-and (list hex-digits hex-digits hex-digits hex-digits)))

(define universal-character-name
  (regex-or 
   (list
    (regex-and (list #\u #\\ hex-quad))
    (regex-and (list #\U #\\ hex-quad hex-quad)))))

(define identifier-nondigit
  (regex-or (cons universal-character-name (string->list "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))))

(define c-digit
  (regex-or (string->list "0123456789")))

(define nonzero-digits
  (regex-or (string->list "123456789")))

(define octal-digit
  (regex-or (string->list "01234567")))

(define unsigned-suffix (regex-or (list #\u #\U)))

(define long-suffix (regex-or (list #\l #\L)))

(define long-long-suffix (regex-or (list (regex-and (list #\l #\l)) (regex-and (list #\L #\L)))))



(define integer-suffix 
  (regex-or
   (list
    (regex-and (list unsigned-suffix (regex-opt long-suffix)))
    (regex-and (list unsigned-suffix long-long-suffix))
    (regex-and (list long-suffix (regex-opt unsigned-suffix)))
    (regex-and (list long-long-suffix (regex-opt unsigned-suffix))))))


;;INCOMPLETE
;;Scanner was derived from C Specification Committee Draft, May 6, 2005. Found it online for no charge.
(define c-scanner (non-macro-scanner 'error
                                     (append
                                      (map (lambda (sym) (list (regex-and (string->list (symbol->string sym))) sym))
                                           '(auto break case char const continue default do double else enum extern
                                                  float for goto if inline int long register restrict return short
                                                  signed sizeof static struct switch typedef union unsigned void
                                                  volatile while _Bool _Complex _Imaginary))
                                      
                                      
                                      (list (list (regex-and (list identifier-nondigit (regex-star (regex-or (list c-digit identifier-nondigit))))) 'identifier))
                                      (list (list (regex-star (regex-or (list #\space #\newline #\tab #\return))) 'whitespace) )
                                      
                                      (list 
                                       (list (regex-and (list nonzero-digits (regex-star c-digit) (regex-opt integer-suffix))) 'decimal-constant)
                                       (list (regex-and (list #\0 (regex-star octal-digit) (regex-opt integer-suffix))) 'octal-constant)
                                       (list (regex-and (list #\0 (regex-or (list #\x #\X)) hex-digits (regex-opt integer-suffix))) 'hexadecimal-constant)
                                       (list (regex-and (list (regex-star hex-digits) (regex-opt integer-suffix))) 'hexadecimal-constant))
                                      
                                      (operators
                                       (#\[ 'obrac)
                                       (#\] 'cbrac)
                                       (#\( 'oparen)
                                       (#\) 'cparen)
                                       (#\{ 'ocurl)
                                       (#\} 'ccurl)
                                       (#\. #\. #\. 'ellipsis)
                                       (#\- #\> 'arrow)
                                       (#\+ #\+ 'plusplus)
                                       (#\- #\- 'minusminus)
                                       (#\& 'amp)
                                       (#\* 'star)
                                       (#\+ 'plus)
                                       (#\- 'minus)
                                       (#\~ 'tilde)
                                       (#\! 'bang)
                                       (#\/ 'slash)
                                       (#\% 'pct)
                                       (#\< #\< 'shl)
                                       (#\> #\> 'shr)
                                       (#\< 'lt)
                                       (#\> 'gt)
                                       (#\< #\= 'le)
                                       (#\> #\= 'ge)
                                       (#\= #\= 'eqeq)
                                       (#\! #\= 'bangeq)
                                       (#\^ 'carot)
                                       (#\| 'bar)
                                       (#\& #\& 'ampamp)
                                       (#\| #\| 'barbar)
                                       (#\? 'question)
                                       (#\: 'colon)
                                       (#\; 'semi)
                                       (#\. 'dot)
                                       (#\= 'eq)
                                       (#\* #\= 'stareq)
                                       (#\/ #\= 'slasheq)
                                       (#\% #\= 'pcteq)
                                       (#\+ #\= 'pluseq)
                                       (#\- #\= 'minuseq)
                                       (#\< #\< #\= 'shleq)
                                       (#\> #\> #\= 'shreq)
                                       (#\& #\= 'ampeq)
                                       (#\^ #\= 'caroteq)
                                       (#\| #\= 'bareq)
                                       (#\, 'comma)
                                       (#\# 'pound)
                                       (#\# #\# 'poundpound)
                                       (#\< #\: 'lcolon)
                                       (#\> #\: 'rcolon)
                                       (#\< #\% 'lpct)
                                       (#\> #\% 'rpct)
                                       (#\% #\: 'pctcolon)
                                       (#\% #\: #\% #\: 'pctcolonpctcolon))
                                      )))


(c-scanner (string->list "if int else _Bool"))

(c-scanner (string->list "if = > >> >>= _Imaginary"))

(c-scanner (string->list "int id = x five"))


(c-scanner (string->list "int id = x > y ? x : y"))

(c-scanner (string->list "int main(void)
{
int i;
int Array[10];
for (i = 0; i < sizeof(Array) / sizeof(Array[0]); i++)
{
Array[i] = i + 0x9a;
}
return -1;
}"))


(java-scanner (string->list (read)))
