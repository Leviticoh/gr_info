#!/usr/bin/env -S guile --no-auto-compile -s
!#

(use-modules (web client) (ice-9 textual-ports) (ice-9 pretty-print))

(define (text-dl uri) (call-with-values (lambda () (http-get uri)) (lambda (a b) b)))

(define json-whitespace (char-set #\space #\tab #\newline #\return))

(define (json-unicode testo)
  (integer->char (+ (* (car testo)))))

(define (_json-string_ accum testo)
  (cond
    ((char=? (car testo) #\") accum)
    
    ((char=? (car testo) #\\) (cond ((char=? (car (cdr testo)) #\")
				     (_json-string_ (cons #\"         accum) (cddr testo)))
				    ((char=? (car (cdr testo)) #\\)
				     (_json-string_ (cons #\\         accum) (cddr testo)))
				    ((char=? (car (cdr testo)) #\/)
				     (_json-string_ (cons #\/         accum) (cddr testo)))
				    ((char=? (car (cdr testo)) #\b)
				     (_json-string_ (cons #\backspace accum) (cddr testo)))
				    ((char=? (car (cdr testo)) #\f)
				     (_json-string_ (cons #\ff        accum) (cddr testo)))
				    ((char=? (car (cdr testo)) #\n)
				     (_json-string_ (cons #\newline   accum) (cddr testo)))
				    ((char=? (car (cdr testo)) #\r)
				     (_json-string_ (cons #\return    accum) (cddr testo)))
				    ((char=? (car (cdr testo)) #\t)
				     (_json-string_ (cons #\tab       accum) (cddr testo)))
				    ((char=? (car (cdr testo)) #\u)
				     (_json-string_ (cons (json-unicode (cddr testo)) accum)
						    (cddddr (cddr testo))))
				    (else 
				      (_json-string_ (cons (car testo) accum) (cdr testo)))))
    (else (_json-string_ (cons (car testo) accum) (cdr testo)))))

(define (json-string testo)
  (list->string (reverse (_json-string_ '() (cdr (string->list testo))))))
#!
(define (json-value testo)
  (cond
    ((char=? (string-ref testo 0) #\")
     (json-string testo))
    ((char=? (string-ref testo 0) #\{)
     (json-object testo))
    ((char-set-contains? (char-set-adjoin char-set:digit #\-) (string-ref testo 0))
     (json-number testo))
    ((char-set-contains? (char-set #\t #\f #\n) (string-ref testo 0))
     (json-atom testo))))
!#
(define (_json-trim-name_ testo)
  (cond

    ((char=? (string-ref testo 0) #\")
     (string-trim (string-drop testo 1) (char-set-adjoin json-whitespace #\:)))

    ((char=? (string-ref testo 0) #\\)
     (_json-trim-name_ (string-drop testo 2)))

    (else
      (_json-trim-name_ (string-drop testo 1)))))



(define (json-trim-name testo)
  (_json-trim-name_ (string-drop testo 1)))

#!

(define (json-element testo)
  (cons
    (json-string testo)
    (json-value (json-trim-name testo))))

!#

(define (json-element testo)
  (define coda (json-trim-name testo))
  (cons
    (json-string testo)
    (cond ((char=? (string-ref coda 0) #\{) (json-object coda))
	  (else (string-trim-right (substring coda 0 (- (string-length coda)
				     (string-length (json-trim-element testo))))
				   (char-set-adjoin json-whitespace #\,))))))

(define (_json-trim-element_ escap paren testo)
  (cond

    ((and (= escap 0)
	  (= paren 0)
	  (or (char=? (string-ref testo 0) #\,)
	      (char=? (string-ref testo 0) #\})))
     (string-trim testo (char-set-adjoin json-whitespace #\,)))

    ((and (= escap 0)
	  (char=? (string-ref testo 0) #\{))
     (_json-trim-element_ 0 (+ paren 1) (string-trim
					  testo
					  (char-set-delete char-set:full #\} #\"))))

    ((and (= escap 0)
	  (> paren 0)
	  (char=? (string-ref testo 0) #\}))
     (_json-trim-element_ 0 (+ paren -1) (string-trim
					   (string-drop testo 1)
					   json-whitespace)))

    ((and (= escap 0)
	  (char=? (string-ref testo 0) #\"))
     (_json-trim-element_ 1 paren (string-trim
				    (string-drop testo 1)
				    (char-set-delete char-set:full #\" #\\))))
    
    ((and (= escap 1)
	  (char=? (string-ref testo 0) #\"))
     (_json-trim-element_ 0 paren (string-drop testo 1)))

    ((and (= escap 1)
	  (char=? (string-ref testo 0) #\\))
     (_json-trim-element_ 2 paren (string-drop testo 1)))

    ((and (= escap 2))
     (_json-trim-element_ 1 paren (string-drop testo 1)))

    (else
      (_json-trim-element_ escap paren (string-drop testo 1)))))



(define (json-trim-element testo)
  (_json-trim-element_ 0 0 testo))



(define (_json-object_ testo)
  (cond
    ((string-null? testo) '())
    ((char=? (string-ref testo 0) #\}) '())
    (else (cons (json-element testo) (_json-object_ (json-trim-element testo))))))



(define (json-object testo)
  (_json-object_ (string-trim testo (char-set-adjoin json-whitespace #\{))))



(define json-parse json-object)

(pretty-print (json-parse (text-dl "https://gensokyoradio.net/api/station/playing/")))
