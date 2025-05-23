#lang racket/base

;; Static contracts for class constructs.
;; Currently supports object/c and class/c.

(require "../../utils/utils.rkt"
         "../structures.rkt" "../constraints.rkt"
         "simple.rkt"
         racket/list racket/match
         (contract-req)
         racket/syntax
         typed-racket/utils/opaque-object
         (for-template racket/base racket/class racket/contract
                       typed-racket/utils/opaque-object)
         (for-syntax racket/base syntax/parse))

(struct member-spec (modifier id sc) #:transparent)

(define field-modifiers '(field init init-field inherit-field))
(define method-modifiers '(method inherit super inner override augment augride))

;; Bottom-out members
(define (f-any _) #'any/c)

(struct object-combinator combinator (opaque?)
  #:transparent
  #:property prop:combinator-name "object/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (object-combinator (member-seq-sc-map f (combinator-args v))
                          (object-combinator-opaque? v)))
     (define (sc-traverse v f)
       (member-seq-sc-map f (combinator-args v))
       (void))
     (define (sc->contract v f)
       (object/sc->contract v f))
     (define (sc->constraints v f)
       (merge-restricts* 'impersonator (map f (member-seq->list (combinator-args v)))))])

(struct class-combinator combinator (opaque absents)
  #:transparent
  #:property prop:combinator-name "class/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match-define (class-combinator args opaque absents) v)
       (class-combinator (member-seq-sc-map f args) opaque absents))
     (define (sc-traverse v f)
       (match-define (class-combinator args opaque absents) v)
       (member-seq-sc-map f args)
       (void))
     (define (sc->contract v f)
       (class/sc->contract v f))
     (define (sc->constraints v f)
       (merge-restricts* 'impersonator (map f (member-seq->list (combinator-args v)))))])

(struct instanceof-combinator combinator ()
  #:transparent
  #:property prop:combinator-name "instanceof/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match-define (instanceof-combinator (list class)) v)
       (instanceof-combinator (list (f class 'covariant))))
     (define (sc-traverse v f)
       (match-define (instanceof-combinator (list class)) v)
       (f class 'covariant)
       (void))
     (define (sc->contract v f)
       (instance/sc->contract v f))
     (define (sc->constraints v f)
       (match-define (instanceof-combinator (list class)) v)
       (f class))])


(define member-seq->list
  (match-lambda
    [(member-seq vals) 
     (filter-map member-spec-sc vals)]))

(struct member-seq (vals)
   #:transparent
   #:property prop:sequence member-seq->list)

(define (member-seq-sc-map f seq)
  (match-define (member-seq vals) seq)
  (member-seq (for/list ([v (in-list vals)])
                (match-define (member-spec mod id sc) v)
                (member-spec mod id (and sc (f sc 'invariant))))))

;; TODO make this the correct subset
(define object-member-spec? member-spec?)

(define (object/sc opaque? specs)
  (object-combinator (member-seq specs) opaque?))
(define (class/sc opaque? specs absents)
  (class-combinator (member-seq specs) opaque? absents))
(define (instanceof/sc class)
  (instanceof-combinator (list class)))

(define ((member-spec->form f) v)
  (match-define (member-spec modifier id sc) v)
  (with-syntax ([id/ctc (if sc
                            #`(#,id #,(f sc))
                            id)])
    (case modifier
      [(method) #'id/ctc]
      [(inner) #'(inner id/ctc)]
      [(init) #'(init id/ctc)]
      [(field) #'(field id/ctc)])))

(define (spec->id/ctc f modifier vals)
  (for/lists (_1 _2)
             ([spec vals]
              #:when (eq? modifier (member-spec-modifier spec)))
    (values (member-spec-id spec)
            (f (member-spec-sc spec)))))

(define (object/sc->contract v f)
  (match-define (object-combinator (member-seq vals) opaque?) v)
  #`(#,(if opaque? #'object/c-opaque #'object/c) #,@(map (member-spec->form f) vals)))

(define (class/sc->contract v f)
  (match-define (class-combinator (member-seq vals) opaque absents) v)
  (define-values (override-names override-ctcs) (spec->id/ctc f 'override vals))
  (define-values (pubment-names pubment-ctcs) (spec->id/ctc f 'pubment vals))
  (define/with-syntax (override-temp ...) (generate-temporaries override-ctcs))
  (define/with-syntax (pubment-temp ...) (generate-temporaries pubment-ctcs))
  (define/with-syntax (override-name ...) override-names)
  (define/with-syntax (pubment-name ...) pubment-names)
  (define/with-syntax (override-ctc ...) override-ctcs)
  (define/with-syntax (pubment-ctc ...) pubment-ctcs)
  (define vals-rest
    (filter (λ (spec) (not (memq (member-spec-modifier spec) '(override pubment)))) vals))
  #`(let ([override-temp override-ctc] ...
          [pubment-temp pubment-ctc] ...)
      (class/c #,@(if opaque
                      '(#:opaque #:ignore-local-member-names)
                      null)
               #,@(map (member-spec->form f) vals-rest)
               [override-name override-temp] ...
               (override [override-name override-temp] ...)
               (super [override-name override-temp] ...)
               (inherit [override-name override-temp] ...)
               [pubment-name pubment-temp] ...
               (augment [pubment-name pubment-temp] ...)
               (inherit [pubment-name pubment-temp] ...)
               (absent #,@absents))))

(define (instance/sc->contract v f)
  (match-define (instanceof-combinator (list class)) v)
  #`(instanceof/c #,(f class)))

(define (make-class-shape/sc init* field* public* augment*)
  (define-values [pubment* override*] (partition (lambda (nm) (memq nm augment*)) public*))
  (with-syntax ((ctc-stx
                  #`(class/c
                      (init . #,init*)
                      (field . #,field*)
                      (inner . #,augment*)
                      (override . #,override*)
                      . #,pubment*)))
    (flat/sc
      #'(let ((check-cls-shape (contract-first-order ctc-stx)))
          (λ (cls)
            (and (class? cls) (check-cls-shape cls)))))))

(define (make-object-shape/sc field-name* method-name*)
  (with-syntax ((ctc-stx #`(object/c (field . #,field-name*) . #,method-name*)))
    (flat/sc
      #'(let ((check-obj-shape (contract-first-order ctc-stx)))
          (λ (this)
            (and (object? this) (check-obj-shape this)))))))

(provide/cond-contract
 [struct member-spec ([modifier symbol?] [id symbol?] [sc static-contract?])]
 [object/sc (boolean? (listof object-member-spec?) . -> . static-contract?)]
 [class/sc (boolean? (listof member-spec?) (listof symbol?) . -> . static-contract?)]
 [instanceof/sc (static-contract? . -> . static-contract?)]
 [make-class-shape/sc ((listof symbol?) (listof symbol?) (listof symbol?) (listof symbol?) . -> . static-contract?)]
 [make-object-shape/sc ((listof symbol?) (listof symbol?) . -> . static-contract?)])

