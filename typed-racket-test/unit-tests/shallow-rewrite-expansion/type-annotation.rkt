#lang typed/racket/shallow

;; 2022-02-04 currently broken, not upgrading type annotations because it breaks toplevel redefinitions

;; Annotated function types should quietly get upgraded when Shallow TR
;;  can trust their results

(module u racket/base
  (provide v)
  (define v (list (lambda () 42))))
(require/typed 'u (v (List (-> String)))) ;; yes shape-check

(: unsafe-fn (-> String))
(define unsafe-fn (car v)) ;; yes shape-check

(: safe-id-0 (-> Symbol Symbol))
(define (safe-id-0 x) x) ;; yes shape-check

(: safe-id-1 (All (A) (-> A A)))
(define (safe-id-1 x) x)

(: safe-id-2 (All (B ...) (-> (List B ... B) (List B ... B))))
(define (safe-id-2 x) x) ;; yes shape-check

(: safe-id-3 (All (r #:row) (-> (Class #:row-var r) (Class #:row-var r))))
(define (safe-id-3 cls) cls) ;; yes shape-check

(let ((x : String (unsafe-fn))) ;; yes shape-check
  (void))

;; no shape-check below this line

(let ((x : Symbol (safe-id-0 'hello)))
  (void))

(let ((x : Symbol (safe-id-1 'hello)))
  (void))

(let ((x : (List Symbol) (safe-id-2 (list 'hello))))
  (void))

(let ((x : (Class) (safe-id-3 (class object% (super-new)))))
  (void))
