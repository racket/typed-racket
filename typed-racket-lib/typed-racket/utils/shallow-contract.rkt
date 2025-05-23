#lang racket/base

;; Extra contracts and tools for the Shallow runtime,
;; which is based on Michael Vitousek's Transient semantics

(provide
  procedure-arity-includes-keywords?
  shallow-and/c
  shallow-or/c
  shallow-shape-check
  raise-shallow-check-error)

;; ---------------------------------------------------------------------------------------------------

;; procedure-arity-includes-keywords? : (-> procedure? (listof keyword?) (listof keyword?) boolean?)
;; Returns true if the procedure accepts calls that supply all mandatory keywords
;; and some optional keywords --- in the sense of racket/contract arity checking.
;; + function must declare all optional keywords as optional
;; + function may declare all mandatory keywords as either mandatory or optional
(define (procedure-arity-includes-keywords? f mand-kw* opt-kw*)
  (define-values [f-mand-kw* f-opt-kw*] (procedure-keywords f))
  ;; note: f-opt-kw* = (sort f-opt-kw* keyword<?)
  (define mand-ok/extra*
    ;; subtract f's mandatory keywords from the expected list (must be a prefix)
    (let loop ((expected-kw* mand-kw*)
               (actual-kw* f-mand-kw*))
      (cond
        [(null? actual-kw*)
         expected-kw*]
        [(null? expected-kw*)
         #f]
        [else
         (and (eq? (car expected-kw*) (car actual-kw*))
              (loop (cdr expected-kw*) (cdr actual-kw*)))])))
  (and mand-ok/extra*
       (let loop ((expected-kw* (sort (append mand-ok/extra* opt-kw*) keyword<?))
                  (actual-kw* f-opt-kw*))
         ;; match the remaining keywords against f's optionals
         (cond
          ((null? expected-kw*)
           #true)
          ((or (null? actual-kw*) (keyword<? (car expected-kw*) (car actual-kw*)))
           #false)
          ((eq? (car actual-kw*) (car expected-kw*))
           (loop (cdr expected-kw*) (cdr actual-kw*)))
          (else ;#(keyword<? actual expected)
           (loop expected-kw* (cdr actual-kw*)))))))

(define ((shallow-and/c . pred*) x)
  (let loop ([p?* pred*])
    (if (null? p?*)
        #true
        (if ((car p?*) x)
            (loop (cdr p?*))
            #false))))

(define ((shallow-or/c . pred*) x)
  (let loop ([p?* pred*])
    (if (null? p?*)
        #false
        (if ((car p?*) x)
            #true
            (loop (cdr p?*))))))

(define (shallow-shape-check val pred ty-str ctx)
  (if (pred val)
    val
    (raise-shallow-check-error val ty-str ctx)))

(define (raise-shallow-check-error val ty ctx)
  (raise-arguments-error 'shape-check
                         "value does not match expected type"
                         "value" val
                         "type" (unquoted-printing-string ty)
                         "lang" 'typed/racket/shallow
                         "src" ctx))

