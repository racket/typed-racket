#lang racket
(require typed-racket/utils/opaque-object rackunit
         (for-syntax (only-in syntax/srcloc build-source-location-list)))

;; --------------------------------------------------------------------------------------------------

;; object/c-opaque names should be lists:
;; - with 'object/c-opaque as the first element
;; - and 1 element for each member of the contract (field spec, method, ...)
(define-syntax (test-object/c-opaque-name-shape stx)
  (syntax-case stx ()
   [(_ . ctc-spec*)
    #`(begin
        #,@(for/list ([ctc-spec (in-list (syntax-e #'ctc-spec*))])
            #`(let ([nm (contract-name (object/c-opaque #,@ctc-spec))])
                (with-check-info* (list (make-check-location '#,(build-source-location-list ctc-spec)))
                  (lambda ()
                    (check-equal? (car nm) 'object/c-opaque)
                    (check-equal? (length nm) (+ 1 #,(length (syntax-e ctc-spec)))))))))]))

(test-object/c-opaque-name-shape
  []
  [(field [i integer?] [j string?])]
  [(field [k (<=/c 0)])
   (m (->m (-> integer? integer?) zero?))]
  [(m1 (->m (-> integer? integer?) zero?))
   (m2 (->m boolean?))
   (m3 (->m natural-number/c any/c))]
  [(field [a real?] [b (-> string?)])
   (m1 (->m (-> integer? integer?) zero?))
   (m2 (->m boolean?))
   (m3 (->m natural-number/c any/c))]
)

;; --------------------------------------------------------------------------------------------------

(define-syntax (test-object/c-vs-opaque-name stx)
  (syntax-case stx ()
   [(_ . ctc-spec*)
    #`(begin
      #,@(for/list ([ctc-spec (in-list (syntax-e #'ctc-spec*))])
           #`(check-not-equal?
               (contract-name (object/c #,@ctc-spec))
               (contract-name (object/c-opaque #,@ctc-spec)))))]))

(test-object/c-vs-opaque-name
  []
  [(field [i integer?])
   (f (->m string? boolean?))]
  [(hd (->m any/c))
   (tl (->m object?))]
)
