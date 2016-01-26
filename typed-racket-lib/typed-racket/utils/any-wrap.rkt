#lang racket/base

(require racket/match racket/contract/combinator
         racket/class racket/unit
         racket/fixnum racket/flonum racket/extflonum
         racket/set
         racket/undefined
         (only-in racket/async-channel async-channel?)
         (only-in racket/future future? fsemaphore?)
         (only-in racket/pretty pretty-print-style-table?)
         (only-in racket/udp udp?)
         (only-in (combine-in racket/private/promise)
                  promise?
                  prop:force promise-forcer))

(define (base-val? e)
  (or (number? e) (string? e) (char? e) (symbol? e)
      (null? e) (eq? undefined e) (path? e) (eof-object? e)
      (regexp? e) (pregexp? e) (byte-regexp? e) (byte-pregexp? e)
      (keyword? e) (bytes? e) (boolean? e) (void? e)
      (bytes-converter? e)
      (impersonator-property? e)
      (inspector? e)
      (logger? e)
      (module-path? e) (resolved-module-path? e)
      (pretty-print-style-table? e)
      (pseudo-random-generator? e)
      (semaphore? e) (fsemaphore? e)
      (thread-group? e)
      (udp? e)
      ;; Base values because you can only store flonums/fixnums in these
      ;; and not any higher-order values. This isn't sound if we ever
      ;; introduce bounded polymorphism for Flvector/Fxvector.
      (flvector? e) (fxvector? e) (extflvector? e) (extflonum? e)))

(define (unsafe-val? e)
  (or ;; TODO: async-channel and special-comment should be safe
      (async-channel? e)
      (special-comment? e)
      ;; --
      (class? e)
      (compiled-expression? e)
      (compiled-module-expression? e)
      (continuation-mark-key? e) ;; Stricter than necessary if key holds a base value
      (continuation-mark-set? e)
      (continuation-prompt-tag? e)
      (custodian-box? e)
      (custodian? e)
      (ephemeron? e)
      (future? e)
      (internal-definition-context? e)
      (mpair? e)
      (namespace-anchor? e)
      (namespace? e)
      (parameterization? e)
      (security-guard? e)
      (struct-type-property? e)
      (syntax? e)
      (thread-cell? e)
      (unit? e)
      (variable-reference? e)
      (weak-box? e)))

(define (late-neg-projection b)
  (define (fail neg-party v)
    (raise-blame-error 
     (blame-swap b) #:missing-party neg-party
     v 
     "Attempted to use a higher-order value passed as `Any` in untyped code: ~v" v))
  
  (define (wrap-struct neg-party s)
    (define blame+neg-party (cons b neg-party))
    (define (extract-functions struct-type)
      (define-values (sym init auto ref set! imms par skip?)
        (struct-type-info struct-type))
      (define-values (fun/chap-list _)
        (for/fold ([res null]
                   [imms imms])
          ([n (in-range (+ init auto))])
          (if (and (pair? imms) (= (car imms) n))
              ;; field is immutable
              (values
               (list* (make-struct-field-accessor ref n)
                      (lambda (s v) (with-contract-continuation-mark
                                     blame+neg-party
                                     (any-wrap/traverse v neg-party)))
                      res)
               (cdr imms))
              ;; field is mutable
              (values
               (list* (make-struct-field-accessor ref n)
                      (lambda (s v) (with-contract-continuation-mark
                                     blame+neg-party
                                     (any-wrap/traverse v neg-party)))
                      (make-struct-field-mutator set! n)
                      (lambda (s v) (fail neg-party s))
                      res)
               imms))))
      (cond
        [par (append fun/chap-list (extract-functions par))]
        [else fun/chap-list]))
    (define-values (type skipped?) (struct-info s))
    ;; It's ok to just ignore skipped? -- see https://github.com/racket/typed-racket/issues/203
    (apply chaperone-struct s (extract-functions type)))
 
  (define (any-wrap/traverse v neg-party)
    (define blame+neg-party (cons b neg-party))
    (match v
      [(? base-val?)
       v]
      [(? unsafe-val?)
       (fail neg-party v)]
      [(cons x y) (cons (any-wrap/traverse x neg-party) (any-wrap/traverse y neg-party))]
      [(? vector? (? immutable?))
       ;; fixme -- should have an immutable for/vector
       (vector->immutable-vector
        (for/vector #:length (vector-length v)
          ([i (in-vector v)]) (any-wrap/traverse i neg-party)))]
      [(? box? (? immutable?)) (box-immutable (any-wrap/traverse (unbox v) neg-party))]
      [(? box?) (chaperone-box v
                               (lambda (v e)
                                 (with-contract-continuation-mark
                                  blame+neg-party
                                  (any-wrap/traverse e neg-party)))
                               (lambda (v e) (fail neg-party v)))]
      ;; fixme -- handling keys properly makes it not a chaperone
      ;; [(? hasheq? (? immutable?))
      ;;  (for/hasheq ([(k v) (in-hash v)]) (values k v))]
      ;; [(? hasheqv? (? immutable?))
      ;;  (for/hasheqv ([(k v) (in-hash v)]) (values k v))]
      
      [(? (λ (e) 
            (and (hash? e) (immutable? e) 
                 (not (hash-eqv? e)) (not (hash-eq? e)))))
       (for/hash ([(k v) (in-hash v)]) (values (any-wrap/traverse k neg-party)
                                               (any-wrap/traverse v neg-party)))]
      [(? vector?) (chaperone-vector v
                                     (lambda (v i e)
                                       (with-contract-continuation-mark
                                        blame+neg-party
                                        (any-wrap/traverse e neg-party)))
                                     (lambda (v i e) (fail neg-party v)))]
      [(? hash?) (chaperone-hash v
                                 (lambda (h k)
                                   (values k (lambda (h k v)
                                               (with-contract-continuation-mark
                                                blame+neg-party
                                                (any-wrap/traverse v neg-party))))) ;; ref
                                 (lambda (h k n)
                                   (if (immutable? v) 
                                       (values k n)
                                       (fail neg-party v))) ;; set
                                 (lambda (h v) v) ;; remove
                                 (lambda (h k)
                                   (with-contract-continuation-mark
                                    blame+neg-party
                                    (any-wrap/traverse k neg-party))))] ;; key
      [(? evt?) (chaperone-evt v (lambda (e) (values e (λ (v)
                                                         (with-contract-continuation-mark
                                                          blame+neg-party
                                                          (any-wrap/traverse v neg-party))))))]
      [(? set?)
       (for/set ([i (in-set v)]) (any-wrap/traverse i neg-party))]
      ;; could do something with generic sets here if they had
      ;; chaperones, or if i could tell if they were immutable.
      [(? struct?) (wrap-struct neg-party v)]
      [(? procedure?)
       (chaperone-procedure v (lambda args (fail neg-party v)))]
      [(? promise?)
       (chaperone-struct
        v
        promise-forcer
        (λ (_ proc) 
          (chaperone-procedure
           proc
           (λ (promise)
             (values (λ (val) (with-contract-continuation-mark
                               blame+neg-party
                               (any-wrap/traverse val neg-party)))
                     promise)))))]
      [(? channel?)
       ;;bg; Should be able to take `Any` from the channel, but can't put anything in
       (chaperone-channel v
                          (lambda (e) (with-contract-continuation-mark
                                       blame+neg-party
                                       (values v (any-wrap/traverse v neg-party))))
                          (lambda (e) (fail neg-party v)))]
      [_ (chaperone-struct v)]))
  any-wrap/traverse)

(define any-wrap/c
  (make-chaperone-contract
   #:name 'Any
   #:first-order (lambda (x) #t)
   #:late-neg-projection late-neg-projection))

;; Contract for "safe" struct predicate procedures.
;; We can trust that these obey the type (-> Any Boolean).
(define (struct-predicate-procedure?/c x)
  (and (struct-predicate-procedure? x)
       (not (impersonator? x))))

(provide any-wrap/c struct-predicate-procedure?/c)
