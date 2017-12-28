; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2017                                *
; *  Author: Ulrich Kremer                    *
; *********************************************
;; -----------------------------------------------------
;; ENVIRONMENT
;; contains "ltv", "vtl",and "reduce" definitions
(load "include.ss")

;; contains a test document consisting of three paragraphs. 
(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
;(load "test-dictionary.ss")

(load "dictionary.ss") ;; the real thing with 45,000 words


;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***
;;paragraph
(define encode-p
  (lambda (p encoder)
    (cond ((null? p) '())
         (else
          (cons (encoder (car p))
                  (encode-p (cdr p) encoder))))))

(define spell-checker-helper
	(lambda (w d)
		(cond
			((null? d) #f) 
                        ((equal? (car d) w) #t)	
			(else (spell-checker-helper w (cdr d)))
			)))

(define the-and
  (lambda (a b)
    (and a b)))

(define checkp ;n starts at 0
  (lambda (p n)
    (cond
     ((> n 26) -1)
     ((eq?(reduce the-and
              (map spell-checker (encode-p p (encode-n n))) #t) #t) (encode-n n))
    (else
     (checkp p (+ 1 n))))))

(define alpha
  '(a b c d e f g h i j k l m n o p q r s t u v w x y z))


(define alphalist
  (lambda (p alpha)
    (cond
      ((null? p) '())
      ((null? alpha) '())
    (else
      (cons (counter (flatten p) (car alpha))
           (alphalist p (cdr alpha)))))))

(define counter
  (lambda (w alpha)
    (cond
      ((null? w) 0)
      ((equal? (car w) alpha)(+ 1 (counter (cdr w) alpha)))
      (else
       (counter (cdr w) alpha)))))

(define flatten
  (lambda (l)
  (cond ((null? l) '()) ((list? (car l))
        (append (flatten (car l)) (flatten (cdr l))))
        (else (cons (car l) (flatten (cdr l))))))
  )

(define maxChar 
  (lambda (max alpha index maxindex)
    (cond
      ((null? alpha) (vtl maxindex))
       ((>(car alpha) max)(maxChar (car alpha)(cdr alpha)(+ 1 index) index))
       (else
        (maxChar max (cdr alpha)(+ 1 index) maxindex)))))

(define ChangeMax
  (lambda (index list)
    (cond
      ((null? list) '())
      (else
      (cons (if (equal? index 0) 0 (car list)) (ChangeMax (- index 1) (cdr list)))))))
;(define ChangeMax
 ;(lambda (index list)
  ; (cond
   ; ((null? list) '())
    ;(else

(define check
  (lambda (p list runs)
    (cond
     ((> runs 26) -1) ;(p invalid paragraph)
     ((eq?(reduce the-and
      (map spell-checker (encode-p p (encode-n (modulo(- 26 (- (ltv (maxChar 0 list 0 0)) (ltv 'e))) 26)))) #t) #t) (encode-n (modulo(- 26 (- (ltv (maxChar 0 list 0 0)) (ltv 'e))) 26)))
    (else
     (check p (ChangeMax (ltv (maxChar 0 list 0 0)) list) (+ 1 runs))))))



;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION

;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
(define spell-checker
  (lambda (w)
   ;'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
   (spell-checker-helper w dictionary)
	)
  )
;; -----------------------------------------------------
;; ENCODING FUNCTIONS

;;generate an Caesar Cipher single word encoders
;;INPUT:a number "n"
;;OUTPUT:a function, whose input is a word, and output is the encoded word
(define encode-n
  (lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
    (lambda (w);;"w" is the word to be encoded
     ;'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
      (map (lambda (x) (vtl (modulo (+ (ltv x) n) 26)))
                w))))
;;encode a document
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
(define encode-d;;this encoder is supposed to be the output of "encode-n"
  (lambda (d encoder)
    ;'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
    (cond ((null? d) '())
          (else
          (cons (encode-p (car d) encoder)
               (encode-d (cdr d) encoder))))
))


;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-A
  (lambda (p)
    (checkp p 0) ;; *** FUNCTION BODY IS MISSING *** 
    ))

;;generate a decoder using frequency analysis
;;INPUT:same as above
;;OUTPUT:same as above
(define Gen-Decoder-B
  (lambda (p)
    (check p (alphalist p alpha) 0)
    ;'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
    ))

;; -----------------------------------------------------
;; CODE-BREAKER FUNCTION

;;a codebreaker
;;INPUT: an encoded document(of course by a Caesar's Cipher), a decoder(generated by functions above)
;;OUTPUT: a decoded document
(define Code-Breaker
  (lambda (d decoder)
     ;'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
     (encode-d d decoder)               
     ))

;; -----------------------------------------------------
;; EXAMPLE APPLICATIONS OF FUNCTIONS
;(spell-checker '(h e l l o))
;(define add5 (encode-n 5))
;(define enc-doc
 ; (encode-d document add5))
;(define decoderSP1 (Gen-Decoder-A (car enc-doc)))
;(define decoderFA1 (Gen-Decoder-B (car enc-doc)))
;(Code-Breaker enc-doc decoderFA1)
;(Code-Breaker enc-doc decoderSP1)
;alpha

;(alphalist '((t h i s)(i s)(s a u c e)) alpha)
;(maxChar 0 (alphalist '((t h i s)(i s)(s a u c e)) alpha) 0 0)
;(- (ltv (maxChar 0 (alphalist '((t h i s)(i s)(s a u c e)) alpha) 0 0)) (ltv 'e))
;(ChangeMax (ltv 's) '(1 0 1 0 1 0 0 1 2 0 0 0 0 0 0 0 0 0 3 1 1 0 0 0 0 0))
;(spell-checker '(t h i s))
;(map spell-checker (encode-p '((t h i s)(i s)(s a u c e)) (encode-n (- (ltv (maxChar 0 (alphalist '((t h i s)(i s)(s a u c e)) alpha) 0 0)) (ltv 'e)))))
;(check '((t h i s)(i s)(t e s t)) '(1 0 1 0 1 0 0 1 2 0 0 0 0 0 0 0 0 0 3 1 1 0 0 0 0 0));(alphalist '((t h i s)(i s)(t e s t)) alpha))
;(encode-p '((h e l l o)(t e s t)) (encode-n 5))
;(encode-p '((t h i s)(i s)(t e s t)) (encode-n 5)) 
;(checkp '((m j q q t) (y j x y)) 0)
;(encode-p '((m j q q t) (y j x y)) (checkp '((m j q q t) (y j x y)) 0))
;(encode-p '((y m n x) (n x) (y j x y)) (check '((t h i s)(i s)(t e s t)) '(1 0 1 0 1 0 0 1 2 0 0 0 0 0 0 0 0 0 3 1 1 0 0 0 0 0)))
;(reduce the-and
              ;(map spell-checker (encode-p '((m j q q t) (y j x y)) (encode-n 21))) #t)
;((Gen-Decoder-B '((h e l l o)(t h i s)(i s)(a)(t e s t))) '(h e l l o))
