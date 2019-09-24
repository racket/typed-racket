#lang racket/base

(require racket/match racket/contract/combinator
         racket/class racket/unit
         racket/fixnum racket/flonum racket/extflonum
         racket/set
         racket/undefined
         (only-in racket/async-channel async-channel?)
         (only-in ffi/unsafe cpointer-predicate-procedure?)
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
      ;; Base since they can only store strings
      ;; Bounded polymorphism would again make these not flat,
      ;; but would be unsound since the external OS can change them
      (environment-variables? e)
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

;; late-neg-projection :
;; (-> #:on-opaque (-> Val Blame Neg-Party (U Val Error))
;;     (-> Blame
;;         (-> Val Neg-Party Val)))
(define ((late-neg-projection #:on-opaque on-opaque) b)
  (define (fail neg-party v)
    (raise-blame-error
     (blame-swap b) #:missing-party neg-party
     v
     "Attempted to use a higher-order value passed as `Any` in untyped code: ~v" v))
  
  (define (wrap-struct neg-party s seen [inspector (current-inspector)])
    (define blame+neg-party (cons b neg-party))
    (define (extract-functions struct-type)
      (define-values (sym init auto ref set! imms par skip?)
        (parameterize ([current-inspector inspector])
          (struct-type-info struct-type)))
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
                                     (any-wrap/traverse v neg-party seen)))
                      res)
               (cdr imms))
              ;; field is mutable
              (values
               (list* (make-struct-field-accessor ref n)
                      (lambda (s v) (with-contract-continuation-mark
                                     blame+neg-party
                                     (any-wrap/traverse v neg-party seen)))
                      (make-struct-field-mutator set! n)
                      (lambda (s v) (fail neg-party s))
                      res)
               imms))))
      (cond
        [par (append fun/chap-list (extract-functions par))]
        [else fun/chap-list]))
    (define-values (type skipped?)
      (parameterize ([current-inspector inspector])
        (struct-info s)))
    ;; It's ok to just ignore skipped? -- see https://github.com/racket/typed-racket/issues/203
    (apply chaperone-struct s (extract-functions type)))

  (define (any-wrap/traverse v neg-party [seen (seteq)])
    (define blame+neg-party (cons b neg-party))
    (define seen/v (set-add seen v))
    (match v
      [(? (λ (v) (set-member? seen v)))
       ;; TODO handle cycles with a correct deep copy (github issue #823)
       (raise-argument-error 'any-wrap/c "acyclic value" 0 v neg-party)]
      [(? base-val?)
       v]
      [(? unsafe-val?)
       (fail neg-party v)]
      [(cons x y) (cons (any-wrap/traverse x neg-party seen/v) (any-wrap/traverse y neg-party seen/v))]
      [(? vector? (? immutable?))
       ;; fixme -- should have an immutable for/vector
       (vector->immutable-vector
        (for/vector #:length (vector-length v)
          ([i (in-vector v)]) (any-wrap/traverse i neg-party seen/v)))]
      [(? box? (? immutable?)) (box-immutable (any-wrap/traverse (unbox v) neg-party seen/v))]
      [(? box?) (chaperone-box v
                               (lambda (v e)
                                 (with-contract-continuation-mark
                                  blame+neg-party
                                  (any-wrap/traverse e neg-party seen/v)))
                               (lambda (v e) (fail neg-party v)))]
      ;; fixme -- handling keys properly makes it not a chaperone
      ;; [(? hasheq? (? immutable?))
      ;;  (for/hasheq ([(k v) (in-hash v)]) (values k v))]
      ;; [(? hasheqv? (? immutable?))
      ;;  (for/hasheqv ([(k v) (in-hash v)]) (values k v))]

      [(? (λ (e)
            (and (hash? e) (immutable? e)
                 (not (hash-eqv? e)) (not (hash-eq? e)))))
       (for/hash ([(k v) (in-hash v)]) (values (any-wrap/traverse k neg-party seen/v)
                                               (any-wrap/traverse v neg-party seen/v)))]
      [(? vector?) (chaperone-vector v
                                     (lambda (v i e)
                                       (with-contract-continuation-mark
                                        blame+neg-party
                                        (any-wrap/traverse e neg-party seen/v)))
                                     (lambda (v i e) (fail neg-party v)))]
      [(? hash?) (chaperone-hash v
                                 (lambda (h k)
                                   (values k (lambda (h k v)
                                               (with-contract-continuation-mark
                                                blame+neg-party
                                                (any-wrap/traverse v neg-party seen/v))))) ;; ref
                                 (lambda (h k n)
                                   (if (immutable? v)
                                       (values k n)
                                       (fail neg-party v))) ;; set
                                 (lambda (h v) v) ;; remove
                                 (lambda (h k)
                                   (with-contract-continuation-mark
                                    blame+neg-party
                                    (any-wrap/traverse k neg-party seen/v))))] ;; key
      [(? set?) (chaperone-hash-set
                 v
                 (λ (s e) e) ; inject
                 (λ (s e) (if (immutable? v)
                              e
                              (fail neg-party v))) ; add
                 (λ (s e) e) ; remove
                 (λ (s e) (with-contract-continuation-mark
                           blame+neg-party
                           (any-wrap/traverse e neg-party seen/v))))] ; extract
      ;; could do something with generic sets here if they had
      ;; chaperones, or if i could tell if they were immutable.
      [(? struct?)
       (wrap-struct neg-party v seen/v)]
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
                               (any-wrap/traverse val neg-party seen/v)))
                     promise)))))]
      [(? channel?)
       ;;bg; Should be able to take `Any` from the channel, but can't put anything in
       (chaperone-channel v
                          (lambda (e) (with-contract-continuation-mark
                                       blame+neg-party
                                       (values e (lambda (inner-val) (any-wrap/traverse inner-val neg-party seen/v)))))
                          (lambda (_e _new-val) (fail neg-party v)))]
      [(? evt?)
       ;; must come after cases for write-able values that can be used as events
       (chaperone-evt v (lambda (e) (values e (λ (v)
                                                (with-contract-continuation-mark
                                                 blame+neg-party
                                                 (any-wrap/traverse v neg-party seen/v))))))]
      [_
       (on-opaque v b neg-party)]))
  any-wrap/traverse)

;; on-opaque-error : Val Blame Neg-Party -> Error
;; To be passed as the #:on-opaque argument to make any-wrap/c raise
;; an error on opaque values.
(define (on-opaque-error v blame neg-party)
  (raise-any-wrap/c-opaque-error v blame neg-party))

;; on-opaque-display-warning : Val Blame Neg-Party -> Val
;; To be passed as the #:on-opaque argument to make any-wrap/c display
;; a warning, but keep going with possible unsoundness.
(define (on-opaque-display-warning v blame neg-party)
  ;; this can lead to unsoundness, see https://github.com/racket/typed-racket/issues/379.
  ;; an error here would make this sound, but it breaks the math library as of 2016-07-08,
  ;; see https://github.com/racket/typed-racket/pull/385#issuecomment-231354377.
  (display-any-wrap/c-opaque-warning v blame neg-party)
  (chaperone-struct v))

;; make-any-wrap/c : (-> #:on-opaque (-> Val Blame Neg-Party (U Val Error)) Chaperone-Contract)
(define (make-any-wrap/c #:on-opaque on-opaque)
  (make-chaperone-contract
   #:name 'Any
   #:first-order (lambda (x) #t)
   #:late-neg-projection (late-neg-projection #:on-opaque on-opaque)))

(define any-wrap/c
  (make-any-wrap/c #:on-opaque on-opaque-error))

(define any-wrap-warning/c
  (make-any-wrap/c #:on-opaque on-opaque-display-warning))

;; struct?/inspector : (-> Inspector (-> Any Boolean))
(define ((struct?/inspector inspector) v)
  (parameterize ([current-inspector inspector])
    (struct? v)))

;; raise-any-wrap/c-opaque-error : Any Blame Neg-Party -> Error
(define (raise-any-wrap/c-opaque-error v blame neg-party)
  (raise-blame-error
   blame #:missing-party neg-party
   v
   (string-append
    "any-wrap/c: Unable to protect opaque value passed as `Any`\n"
    "  value: ~e\n")
   v))

;; display-any-wrap/c-opaque-warning : (-> Any Blame Neg-Party Void)
(define (display-any-wrap/c-opaque-warning v blame neg-party)
  (displayln
   ((current-blame-format)
    (blame-add-missing-party blame neg-party)
    v
    (format
     (string-append
      "any-wrap/c: Unable to protect opaque value passed as `Any`\n"
      "  value: ~e\n"
      "  This warning will become an error in a future release.\n")
     v))
   (current-error-port)))

;; Contract for "safe" struct predicate procedures.
;; We can trust that these obey the type (-> Any Boolean).
(define (struct-predicate-procedure?/c x)
  (and (or (struct-predicate-procedure? x)
           (cpointer-predicate-procedure? x))
       (not (impersonator? x))))

(provide any-wrap/c any-wrap-warning/c struct-predicate-procedure?/c)
