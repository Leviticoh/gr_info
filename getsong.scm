#!/usr/bin/env -S guile -s
!#

(use-modules (web client) (ice-9 textual-ports) (ice-9 pretty-print) (rnrs bytevectors))

(define (text-dl uri) (utf8->string (call-with-values (lambda () (http-get uri #:decode-body? #f)) (lambda (a b) b))))

(define json-whitespace (char-set #\space #\tab #\newline #\return))

(define (json-unicode testo)
  (string->number (string (car testo) (cadr testo) (caddr testo) (cadddr testo)) 16))

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

(define (json-unicode-resolve-surrogates accum testo)
  (cond
    ((null? testo)
     accum)

    ((char? (car testo))
     (json-unicode-resolve-surrogates (cons (car testo) accum) (cdr testo)))

    ((>= (car testo) #xD800)
     (json-unicode-resolve-surrogates (cons (integer->char (+ #x10000
							      (- (car testo)
								 #xD800)
							      (- (cadr testo)
								 #xDC00)))
					    accum) (cddr testo)))

    (else
      (json-unicode-resolve-surrogates (cons (integer->char (car testo))
					     accum) (cdr testo)))))

(define (json-string testo)
  (list->string (json-unicode-resolve-surrogates '() (_json-string_ '() (cdr (string->list testo))))))

(define (_json-array_ testo)
  (cond
    ((char=? (string-ref testo 0)
	     #\])
     '())

    (else
      (cons (json-value testo)
	    (_json-array_ (json-trim-value testo))))))

(define (json-array testo)
  (_json-array_ (string-trim testo (char-set-adjoin json-whitespace #\[))))

(define (json-atom testo)
  (cond
    ((string=? "true" (string-take testo 4)) #t)
    ((string=? "false" (string-take testo 5)) #f)
    (else '())))

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

(define (json-value testo)
  (cond
    ((char=? (string-ref testo 0) #\")
     (json-string testo))
    ((char=? (string-ref testo 0) #\{)
     (json-object testo))
    ((char=? (string-ref testo 0) #\[)
     (json-array testo))
    ((char-set-contains? (char-set #\t #\f #\n) (string-ref testo 0))
     (json-atom testo))
    (else
      (string-trim-right (substring testo 0 (- (string-length testo)
					       (string-length (json-trim-value testo))))
			 (char-set-adjoin json-whitespace #\,)))))

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


(define (json-element testo)
  (cons
    (json-string testo)
    (json-value (json-trim-name testo))))



(define (_json-trim-string_ testo)
  (cond
    ((char=? (string-ref testo 0)
	     #\")
     (string-drop testo 1))

    ((char=? (string-ref testo 0)
	     #\\)
     (_json-trim-string_ (string-drop testo 2)))

    (else
      (_json-trim-string_ (string-trim testo (char-set-delete char-set:full #\\ #\"))))))

(define (json-trim-string testo)
  (_json-trim-string_ (string-drop testo 1)))




(define (_json-trim-object_ testo)
  (cond
    ((char=? (string-ref testo 0)
	     #\})
     (string-drop testo 1))

    (else
      (_json-trim-object_ (json-trim-element testo)))))

(define (json-trim-object testo)
  (_json-trim-object_ (string-trim (string-drop testo 1) json-whitespace)))


(define (_json-trim-array_ testo)
  (cond
    ((char=? (string-ref testo 0)
	     #\])
     (string-drop testo 1))

    (else
      (_json-trim-array_ (json-trim-value testo)))))

(define (json-trim-array testo)
  (_json-trim-array_ (string-trim (string-drop testo 1) json-whitespace)))


(define (json-trim-gen testo)
  (string-trim (string-trim testo
			    (char-set-delete char-set:full
					     #\,
					     #\]
					     #\}))
	       (char-set-adjoin json-whitespace
				#\,)))


(define (json-trim-value testo)
  (cond
    ((char=? (string-ref testo 0)
	     #\")
     (string-trim (json-trim-string testo)
		  (char-set-adjoin json-whitespace #\,)))

    ((char=? (string-ref testo 0)
	     #\{)
     (string-trim (json-trim-object testo)
		  (char-set-adjoin json-whitespace #\,)))

    ((char=? (string-ref testo 0)
	     #\[)
     (string-trim (json-trim-array testo)
		  (char-set-adjoin json-whitespace #\,)))

    (else
      (string-trim (json-trim-gen testo)
		   (char-set-adjoin json-whitespace #\,)))))

(define (json-trim-element testo)
  (json-trim-value (string-trim (json-trim-string (string-trim testo json-whitespace))
		(char-set-adjoin json-whitespace #\:))))



(define (_json-object_ testo)
  (cond
    ((string-null? testo) '())
    ((char=? (string-ref testo 0) #\}) '())
    (else (cons (json-element testo) (_json-object_ (json-trim-element testo))))))



(define (json-object testo)
  (_json-object_ (string-trim testo (char-set-adjoin json-whitespace #\{))))



(define json-parse json-value)


(pretty-print (json-parse (text-dl "https://gensokyoradio.net/api/station/playing/")))
