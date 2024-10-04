#lang racket/base

;; TODO
;; - fix datum-literals ?
;; - enable type-annotation test by upgrading user's annotations to trust codomain

(require racket/list
         racket/pretty
         racket/set
         rackunit
         syntax/parse
         (only-in racket/format ~a)
         (only-in syntax/modread with-module-reading-parameterization)
         "../test-utils.rkt")

(provide tests)
(gen-test-main)

(define (filename->path fn)
  (collection-file-path fn "typed-racket-test" "unit-tests" "shallow-rewrite-expansion"))

(define (filename->stx filename)
  (define mod-stx
    ;; thanks: read-lang-file
    (call-with-input-file (filename->path filename)
      (lambda (in-port)
        (port-count-lines! in-port)
        (with-module-reading-parameterization
          (lambda ()
            (read-syntax (object-name in-port) in-port))))))
  (parameterize ([current-namespace (make-base-namespace)])
    (expand mod-stx)))

;; ---

(define-syntax-rule (syntax-predicate pat)
  (syntax-parser [pat #true] [_ #false]))

(define (no-shape-check? stx)
  (null? (stx-find-shape-check* stx)))

(define (not-implemented stx)
  #f)

;; ---

(define (stx-find orig-stx p?)
  (define stx* (stx-find* orig-stx p?))
  (when (or (null? stx*) (not (null? (cdr stx*))))
    (raise-arguments-error 'stx-find
                           "non-unique results"
                           "num matches"
                           (length stx*)
                           "orig-stx"
                           orig-stx
                           "predicate"
                           p?
                           "matches"
                           stx*))
  (car stx*))

(define (stx-find* orig-stx p?)
  (let loop ((stx orig-stx))
    (cond
      [(syntax? stx)
       (define v (p? stx))
       (if v (list (if (eq? #true v) stx v)) (loop (syntax-e stx)))]
      [(pair? stx)
       (append (loop (car stx)) (loop (cdr stx)))]
      [else
        '()])))

(define (stx-find-shape-check orig-stx)
  (stx-find orig-stx shape-check?))

(define (stx-find-shape-check* orig-stx)
  (stx-find* orig-stx shape-check?))

(define shape-check?
  (syntax-predicate
    ((~literal #%plain-app) (~datum shallow-shape-check) . _)))

(define (stx-find-define-predicate-ctc stx pred-name)
  (define lift-id
    (stx-find stx
              (syntax-parser
                #:datum-literals (define-values let-values #%app)
                [(define-values (name:id)
                   (let-values ([(:id) (#%app fcp:id lift:id)])
                     _))
                 #:when (eq? (syntax-e #'name) pred-name)
                 #'lift]
                [_ #f])))
  (define lift-ctc
    (stx-find stx
              (syntax-parser
                [((~datum define-values) (lt:id) ctc:id)
                 #:when (eq? (syntax-e #'lt) (syntax-e lift-id))
                 #'ctc]
                [_ #f])))
  (define ctc*
    (stx-find* stx
               (syntax-parser
                 #:datum-literals (define-values lambda)
                 [(define-values (g:id) (lambda (_) body))
                  #:when (eq? (syntax-e #'g) (syntax-e lift-ctc))
                  #'body]
                 [_ #f])))
  (cond
    [(null? ctc*) lift-ctc]
    [(null? (cdr ctc*)) (car ctc*)]
    [else
     (raise-arguments-error 'stx-find-define-predicate-ctc "cannot find lifted" "pred" pred-name)]))

(define (split-shape-check stx)
  (define expr (->datum stx))
  (define line-num
    (let* ((v (sixth expr))
           (snd (and (pair? v) (pair? (cdr v)) (cadr v)))
           (num (cadr (if (eq? snd 'kernel:srcloc) (fourth v) snd))))
      num))
  (values (third expr)
          (fourth expr)
          (second (fifth expr))
          line-num))

(define (->datum x)
  (if (syntax? x)
    (syntax->datum x)
    x))

(define (shape-check-matches #:expr [this-expr #f] #:shape [this-shape #f] #:type [this-type #f] #:line [this-line #f])
  (procedure-rename
    (lambda (stx)
      (define-values [that-expr that-shape that-type that-line] (split-shape-check stx))
      (and
        (asym-equal?! this-expr that-expr "expression")
        (asym-equal?! this-shape that-shape "shape")
        (asym-equal?! this-type that-type "surface type")
        (asym-equal?! this-line that-line "line")))
    (string->symbol
      (format "shape-check-matcher:~a" (or this-shape this-line)))))

(define (asym-equal?! a b kind)
  (when a
    (unless (equal? a b)
      (raise-arguments-error
        'shape-check-matches
        (format "~a mismatch" kind)
        "expected" a
        "actual" b))))

(define (one-assert-matches pred assert*)
  (define *exns (box '()))
  (define (collect-exn ex)
    (set-box! *exns (cons ex (unbox *exns)))
    #f)
  (or
    (for/or ((assert (in-list assert*)))
      (with-handlers ((exn:fail:contract? collect-exn))
        (pred assert)))
    (apply
      raise-arguments-error
      'one-assert-matches
      "no matches found"
      "predicate" (object-name pred)
      (apply append
        (for/list ((assert (in-list assert*))
                   (exn (in-list (reverse (unbox *exns))))
                   (i (in-naturals 1)))
          (list (format "stx ~a" i) assert
                (format "error ~a" i) (exn-message exn)))))))

(define (check-comments-for-shape-check filename)
  (list filename
        (lambda (stx) (check-comments filename stx))))

(define (check-comments filename stx)
  (define all-assert* (stx-find-shape-check* stx))
  (define line-nums
    (with-input-from-file (filename->path filename)
      (lambda ()
        (for/list ((ln (in-lines))
                   (n (in-naturals 1))
                   #:when (regexp-match? #rx"yes shape-check" ln))
          n))))
  (and
    (= (length all-assert*) (length line-nums))
    (for/and ((assert (in-list all-assert*))
              (num (in-list line-nums)))
      ((shape-check-matches #:line num) assert))))

;; ---

(define test-registry
  (list
    (list "base-env-numeric.rkt"
          no-shape-check?)

    (list "call-with-values.rkt"
          (lambda (stx)
            (define all-assert* (stx-find-shape-check* stx))
            (and (= 1 (length all-assert*))
                 (andmap (shape-check-matches #:type "Symbol") all-assert*))))

    (list "case-lambda.rkt"
          (lambda (stx)
            (and
              (let* ((f0-stx (stx-find
                               stx
                               (syntax-predicate
                                 ((~literal define-values) ((~datum f0)) ((~literal case-lambda) _)))))
                     (f0-assert (stx-find-shape-check f0-stx))
                     (f0-match? (shape-check-matches #:expr 'x #:shape 'symbol?  #:type "Symbol")))
                (f0-match? f0-assert))
              (let* ((all-assert* (stx-find-shape-check* stx))
                     (y-pred (shape-check-matches #:expr 'y #:shape 'none/c/proc #:type "Nothing"))
                     (z-pred (shape-check-matches #:expr 'z #:shape 'none/c/proc #:type "Nothing")))
                (and
                  (one-assert-matches y-pred all-assert*)
                  (one-assert-matches z-pred all-assert*))))))

    (list "cdr.rkt"
          no-shape-check?)

    (list "default-continuation-prompt-tag.rkt"
          no-shape-check?)

    (list "for.rkt"
          (lambda (stx)
            (define all-assert* (stx-find-shape-check* stx))
            (define num-expected 2)
            (define car-pred (shape-check-matches #:type "(U 'A 'B 'C)"))
            (unless (= num-expected (length all-assert*))
              (raise-arguments-error 'for.rkt
                                     "wrong number of shape-check"
                                     "expected" num-expected
                                     "actual" (length all-assert*)))
            (for/and ((assert (in-list all-assert*)))
              (car-pred assert))))

    (check-comments-for-shape-check "list.rkt")

    (list "object-private.rkt"
          (lambda (stx)
            (define all-assert* (stx-find-shape-check* stx))
            (and
              (= 1 (length all-assert*))
              (andmap (shape-check-matches #:type "Symbol") all-assert*))))

    (list "parameter.rkt"
          (lambda (stx)
            (define all-assert* (stx-find-shape-check* stx))
            (and
              (= 1 (length all-assert*))
              (andmap (shape-check-matches #:type "Symbol") all-assert*))))

    (list "predicate.rkt"
          (lambda (stx)
            (define List?-stx (stx-find-define-predicate-ctc stx 'List?))
            (identifier? List?-stx)))

    (list "struct-predicate.rkt"
          (lambda (stx)
            (define all-assert* (stx-find-shape-check* stx))
            (define all-line*
              (for/list ((assert (in-list all-assert*)))
                (define-values [_a _b _c line] (split-shape-check assert))
                line))
            (define struct-line-num
              (with-input-from-file (filename->path "struct-predicate.rkt")
                (lambda ()
                  (for/first ((ln (in-lines))
                              (n (in-naturals))
                              #:when (regexp-match? #rx"^\\(struct .*\\)$" ln))
                    n))))
            (define late-line*
              (filter (lambda (n) (and n (< struct-line-num n))) all-line*))
            (= 1 (length late-line*))))

    ;; TODO need to infer trusted-positive for user-defined types, or something
    #;(list "type-annotation.rkt"
          (lambda (stx)
            (define all-assert* (stx-find-shape-check* stx))
            (define line-nums
              (with-input-from-file (filename->path "type-annotation.rkt")
                (lambda ()
                  (for/list ((ln (in-lines))
                             (n (in-naturals 1))
                             #:when (regexp-match? #rx"yes shape-check" ln))
                    n))))
            (and
              (= (length all-assert*) (length line-nums))
              (for/and ((assert (in-list all-assert*))
                        (num (in-list line-nums)))
                ((shape-check-matches #:line num) assert)))))

    (check-comments-for-shape-check "typed-pict.rkt")

    (check-comments-for-shape-check "typed-framework.rkt")

    (list "typed-gui-base.rkt"
          no-shape-check?)

    (check-comments-for-shape-check "typed-stx.rkt")

  ))

(define tests
  (test-suite "Shallow Rewrite Tests"
    (for ((name+pred (in-list test-registry)))
      (define test-name (car name+pred))
      (define test-pred (cadr name+pred))
      (with-check-info (('test-file test-name)
                        ('test-predicate (object-name test-pred)))
        (check-pred test-pred (filename->stx test-name))))))

