#lang typed/racket/shallow

;; Test a variety of accessors for failures

(module untyped racket
  (define pair0 (cons "A" "B"))
  (define mpair0 (mcons "A" "B"))
  (define list0 (list "A" "B" "C"))
  (define vector0 (vector "A" "B"))
  (define box0 (box "A"))
  (define hash0 (make-hash (list (cons 'A "A"))))
  (define set0 (set "A"))
  (define (function0 x) "not symbol")
  (struct mystruct (ref))
  (define mystruct0 (mystruct "not symbol"))
  (define class0
    (class object%
      (super-new)
      (field (field0 "not symbol"))
      (define/public (method0 x) "not symbol")
      (define/public (method1 x) "not symbol")
      (define/public (method2 x) this)
      (define/public (method3 x) "not symbol")))
  (define object0 (new class0))
  (provide pair0 mpair0 list0 vector0 box0 hash0 set0 function0
           (struct-out mystruct) mystruct0 class0 object0))

(module shallow typed/racket/shallow
  (: function1 (-> String String))
  (define (function1 x) "B")
  (define-type Class1
   (Class
     (field (field0 String))
     (method0 (-> String String))
     (method1 (-> String String))
     (method2 (-> String (Instance Class1)))
     (method3 (-> String String))))
  (: class1 Class1)
  (define class1
    (class object%
      (super-new)
      (field (field0 "not symbol"))
      (define/public (method0 x) "not symbol")
      (define/public (method1 x) "not symbol")
      (define/public (method2 x) this)
      (define/public (method3 x) "not symbol")))
  (define object1 (new class1))
  (provide function1 class1 object1))

(define-type Class0
  (Class (field (field0 Symbol))
         (method0 (-> Symbol Symbol))
         (method1 (-> Symbol Symbol))
         (method2 (-> Symbol (Instance Class0)))
         (method3 (-> Symbol Symbol))))

(define-type Class1
  (Class (field (field0 Symbol))
         (method0 (-> Symbol Symbol))
         (method1 (-> Symbol Symbol))
         (method2 (-> Symbol (Instance Class1)))
         (method3 (-> Symbol Symbol))))

(require typed/rackunit typed/racket/random)
(require/typed 'untyped
  (pair0 (Pairof Symbol Symbol))
  (mpair0 (MPairof Symbol Symbol))
  (list0 (Listof Symbol))
  (vector0 (Vectorof Symbol))
  (box0 (Boxof Symbol))
  (hash0 (HashTable Symbol Symbol))
  (set0 (Setof Symbol))
  (function0 (-> Symbol Symbol))
  (#:struct mystruct ((ref : Symbol)))
  (mystruct0 mystruct)
  (class0 Class0)
  (object0 (Instance Class0)))

(require/typed 'shallow
  (function1 (-> Symbol Symbol))
  (class1 Class1)
  (object1 (Instance Class1)))

;; -----------------------------------------------------------------------------
;; pair

(check-exn exn:fail:contract?
  (lambda ()
    (car pair0)))

(check-exn exn:fail:contract?
  (lambda ()
    (cdr pair0)))

;; -----------------------------------------------------------------------------
;; mpair

(check-exn exn:fail:contract?
  (lambda ()
    (mcar mpair0)))

(check-exn exn:fail:contract?
  (lambda ()
    (mcdr mpair0)))

;; -----------------------------------------------------------------------------
;; list

(check-exn exn:fail:contract?
  (lambda ()
    (car list0)))

(check-exn exn:fail:contract?
  (lambda ()
    (cadr list0)))

(check-exn exn:fail:contract?
  (lambda ()
    (third list0)))

;; -----------------------------------------------------------------------------
;; vector

(check-exn exn:fail:contract?
  (lambda ()
    (vector-ref vector0 0)))

; no type: vector*-ref

;; -----------------------------------------------------------------------------
;; box

(check-exn exn:fail:contract?
  (lambda ()
    (unbox box0)))

; no type: unbox*

;; -----------------------------------------------------------------------------
;; hash

(check-exn exn:fail:contract?
  (lambda ()
    (hash-ref hash0 'A)))

(check-exn exn:fail:contract?
  (lambda ()
    (hash-ref! hash0 'A (lambda () 'B))))

;; -----------------------------------------------------------------------------
;; sequence

(check-exn exn:fail:contract?
  (lambda ()
    (sequence-ref list0 0)))

(check-exn exn:fail:contract?
  (lambda ()
    (sequence-ref vector0 0)))

(check-exn exn:fail:contract?
  (lambda ()
    (random-ref list0)))

(check-exn exn:fail:contract?
  (lambda ()
    (random-ref vector0)))

; no type: (sequence-ref hash0 'A)
; no type: (random-ref hash0)

;; -----------------------------------------------------------------------------
;; no type: stream-first stream-ref

;; -----------------------------------------------------------------------------
;; no type: dict-ref

;; -----------------------------------------------------------------------------
;; set

(check-exn exn:fail:contract?
  (lambda ()
    (set-first list0)))

(check-exn exn:fail:contract?
  (lambda ()
    (set-first set0)))

; no type: (set-first hash0)

;; -----------------------------------------------------------------------------
;; procedure

(check-exn exn:fail:contract?
  (lambda ()
    (function0 'A)))

(check-exn exn:fail:contract?
  (lambda ()
    (apply function0 (list 'A))))

(check-exn exn:fail:contract?
  (lambda ()
    (function1 'A)))

(check-exn exn:fail:contract?
  (lambda ()
    (apply function1 (list 'A))))

; no type: keyword-apply

;; -----------------------------------------------------------------------------
;; struct

(check-exn exn:fail:contract?
  (lambda ()
    (mystruct-ref mystruct0)))

;; -----------------------------------------------------------------------------
;; class / object

;; --- bad output

(check-exn exn:fail:contract?
  (lambda ()
    (symbol->string (send object0 method0 'A))))

(check-exn exn:fail:contract?
  (lambda ()
    (symbol->string (send* object0 (method0 'A) (method1 'A)))))

(check-exn exn:fail:contract?
  (lambda ()
    (symbol->string (send+ object0 (method2 'A) (method3 'A)))))

(check-exn exn:fail:contract?
  (lambda ()
    (get-field field0 object0)))

;; --- bad input

(check-exn exn:fail:contract?
  (lambda ()
    (send object1 method0 'A)))

(check-exn exn:fail:contract?
  (lambda ()
    (send* object1 (method0 'A) (method1 'A))))

(check-exn exn:fail:contract?
  (lambda ()
    (send+ object1 (method2 'A) (method3 'A))))

(check-exn exn:fail:contract?
  (lambda ()
    (get-field field0 object1)))

; no type: send/apply send/keyword-apply dynamic-send with-method(expansion) dynamic-get-field class-field-accessor send-generic

