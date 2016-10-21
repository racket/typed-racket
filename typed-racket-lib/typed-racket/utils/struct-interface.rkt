#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/list)
         racket/unsafe/ops)

(provide def-struct-interface)

(define-syntax (def-struct-interface stx)
  (syntax-parse stx
    [(_ #:name name:id
        #:pred pred:id
        #:prop-name prop-name:id
        #:prop-verifier verify:id
        #:fields (accessor:id ...))
     (define n (length (syntax->list #'(accessor ...))))
     (with-syntax ([(idx ...) (range n)])
       (quasisyntax/loc stx
         (begin
           (define (verify vec)
             (unless (and (vector? vec)
                          (immutable? vec)
                          (= #,n (vector-length vec)))
               (error 'name
                      "invalid struct interface for ~a, expected vector of ~a fields, given ~a"
                      'name
                      #,n
                      vec))
             vec)
           (define-values (prop-name pred get-struct-interface)
             (make-struct-type-property 'name))
           (define (accessor struct)
             (unsafe-vector-ref (get-struct-interface struct) idx))
           ...)))]))


#| delete this line to uncomment example w/ tests

(def-struct-interface
  #:name Vehicle
  #:pred Vehicle?
  #:prop-name prop:Vehicle
  #:prop-verifier verify-prop:Vehicle
  #:fields [num-of-wheels motorized? requires-license?])


(struct Bike (brand model) #:transparent
  #:property prop:Vehicle (verify-prop:Vehicle
                           (vector-immutable
                            2
                            #f
                            #f)))

(struct Car (brand model) #:transparent
  #:property prop:Vehicle (verify-prop:Vehicle
                           (vector-immutable
                            4
                            #t
                            #t)))

(define b (Bike 'Schwinn 'Moab))
(define c (Car 'Toyota 'Camry))

(assert (= 2 (num-of-wheels b)))
(assert (not (motorized? b)))
(assert (not (requires-license? b)))

(assert (= 4 (num-of-wheels c)))
(assert (motorized? c))
(assert (requires-license? c))

;; |#

