#lang racket/base

(require syntax/parse syntax/stx syntax/id-table racket/promise
         racket/syntax racket/match syntax/parse/experimental/specialize
         "../utils/utils.rkt" racket/unsafe/ops racket/sequence
         (for-template racket/base racket/math racket/flonum racket/unsafe/ops)
         (types numeric-tower subtype type-table utils)
         (optimizer utils numeric-utils logging float unboxed-tables)
         (utils tc-utils))

(provide float-complex-opt-expr
         float-complex-expr
         binding-names
         float-complex-arith-expr
         unboxed-float-complex-opt-expr
         float-complex-call-site-opt-expr arity-raising-opt-msg)

(define-literal-syntax-class +)
(define-literal-syntax-class -)
(define-literal-syntax-class *)
(define-literal-syntax-class /)
(define-literal-syntax-class conjugate)
(define-literal-syntax-class magnitude)
(define-literal-syntax-class make-polar)
(define-literal-syntax-class exp)

(define-literal-syntax-class make-rectangular^ (make-rectangular unsafe-make-flrectangular))
(define-literal-syntax-class real-part^ (real-part flreal-part unsafe-flreal-part))
(define-literal-syntax-class imag-part^ (imag-part flimag-part unsafe-flimag-part))
(define-merged-syntax-class projection^ (real-part^ imag-part^))

(define-merged-syntax-class float-complex-op (+^ -^ *^ conjugate^ exp^))

(define-syntax-class/specialize number-expr (subtyped-expr -Number))
(define-syntax-class/specialize float-expr (subtyped-expr -Flonum))
(define-syntax-class/specialize float-complex-expr (subtyped-expr -FloatComplex))

(define (binding-names)
  (generate-temporaries (list "unboxed-real-" "unboxed-imag-")))

(define arity-raising-opt-msg "Complex number arity raising.")
(define-syntax-rule (log-unboxing-opt opt-label)
  (log-opt opt-label "Complex number unboxing."))
(define-syntax-rule (log-arity-raising-opt opt-label)
  (log-opt opt-label arity-raising-opt-msg))
(define-syntax-rule (log-missed-complex-expr)
  (log-missed-optimization
    "non-complex value in complex arithmetic"
    (string-append
      "This expression has a non-float Complex number type. "
      "The optimizer could optimize it better if it had type Float-Complex.")
    this-syntax))

;; keep track of operands that were reals (and thus had exact 0 as imaginary part)
(define real-id-table (make-free-id-table))
(define (was-real? stx)
  (free-id-table-ref real-id-table stx #f))
(define (mark-as-real stx)
  (free-id-table-set! real-id-table stx #t)
  stx)
;; keep track of operands that were not floats (i.e. rationals and single floats)
;; to avoid prematurely converting to floats, which may change results
(define non-float-table (make-hash))
(define (as-non-float stx)
  (hash-ref non-float-table stx #f))
(define (mark-as-non-float stx [orig stx])
  (hash-set! non-float-table stx orig)
  stx)

(define (n-ary->binary/non-floats op unsafe this-syntax cs)
  (let loop ([o (stx-car cs)] [cs (stx-cdr cs)])
    ;; we're guaranteed to hit non-"non-float" operands before
    ;; we hit the end of the list. otherwise we wouldn't be doing
    ;; float-complex optimizations
    (define c1 (stx-car cs))
    (define o-nf (as-non-float o))
    (define c1-nf (as-non-float c1))
    (if (or o-nf c1-nf)
        ;; can't convert those to floats just yet, or may change
        ;; the result
        (let ([new-o (mark-as-non-float
                      (quasisyntax/loc this-syntax
                        (#,op #,(or o-nf o) #,(or c1-nf c1))))])
          (if (stx-null? (stx-cdr cs))
              new-o
              (loop new-o
                    (stx-cdr cs))))
        ;; we've hit floats, can start coercing
        (n-ary->binary this-syntax unsafe (cons #`(real->double-flonum #,(or o-nf o)) cs)))))



;; it's faster to take apart a complex number and use unsafe operations on
;; its parts than it is to use generic operations
;; we keep the real and imaginary parts unboxed as long as we stay within
;; complex operations
(define-syntax-class unboxed-float-complex-opt-expr
  #:commit
  #:attributes (real-binding imag-binding (bindings 1))
  #:literal-sets (kernel-literals)

  ;; We let racket's optimizer handle optimization of 0.0s
  (pattern (#%plain-app op:+^ (~between cs:unboxed-float-complex-opt-expr 2 +inf.0) ...)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unboxed binary float complex")]
    #:with (bindings ...)
      #`(cs.bindings ... ...
         #,@(let ()
              (define (fl-sum cs)
                (n-ary->binary/non-floats #'+ #'unsafe-fl+ this-syntax cs))
              (define non-0-imags
                ;; to preserve result sign, ignore exact 0s
                ;; o/w, can have (+ -0.0 (->fl 0)) => 0.0, but would be -0.0
                ;; without the coercion
                (for/list ([i (syntax->list #'(cs.imag-binding ...))]
                           #:unless (was-real? i))
                  i))
               (list
                #`((real-binding) #,(fl-sum #'(cs.real-binding ...)))
                #`((imag-binding)
                   #,(if (null? (cdr non-0-imags)) ; only one actual imag part
                         (car non-0-imags)
                         (fl-sum non-0-imags)))))))
  (pattern (#%plain-app op:+^ :unboxed-float-complex-opt-expr)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:do [(log-unboxing-opt "unboxed unary float complex")])

  (pattern (#%plain-app op:-^ (~between cs:unboxed-float-complex-opt-expr 2 +inf.0) ...)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unboxed binary float complex")]
    #:with (bindings ...)
      #`(cs.bindings ... ...
         #,@(let ()
              (define (fl-subtract cs)
                (n-ary->binary/non-floats #'- #'unsafe-fl- this-syntax cs))
              (list
               #`((real-binding) #,(fl-subtract #'(cs.real-binding ...)))
               #`((imag-binding)
                  ;; can't ignore exact 0 imag parts from real numbers, as with
                  ;; addition, because the first value is special
                  ;; so just conservatively use generic subtraction
                  #,(if (ormap was-real? (syntax->list #'(cs.imag-binding ...)))
                        (n-ary->binary
                         this-syntax
                         #'-
                         (for/list ([i (syntax->list #'(cs.imag-binding ...))])
                           (if (was-real? i) #'0 i)))
                        (fl-subtract #'(cs.imag-binding ...))))))))
  (pattern (#%plain-app op:-^ c1:unboxed-float-complex-opt-expr) ; unary -
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unboxed unary float complex")]
    #:with (bindings ...)
      #`(c1.bindings ...
         [(real-binding) (unsafe-fl* -1.0 c1.real-binding)]
         [(imag-binding) (unsafe-fl* -1.0 c1.imag-binding)]))

  (pattern (#%plain-app op:*^
                        c1:unboxed-float-complex-opt-expr
                        c2:unboxed-float-complex-opt-expr
                        cs:unboxed-float-complex-opt-expr ...)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unboxed binary float complex")]
    #:with (bindings ...)
      #`(c1.bindings ... c2.bindings ... cs.bindings ... ...
         ;; we want to bind the intermediate results to reuse them
         ;; the final results are bound to real-binding and imag-binding
         #,@(let ((lr (syntax->list #'(c1.real-binding c2.real-binding cs.real-binding ...)))
                  (li (syntax->list #'(c1.imag-binding c2.imag-binding cs.imag-binding ...))))
              (let loop ([o1 (car lr)]
                         [o2 (car li)]
                         [e1 (cdr lr)]
                         [e2 (cdr li)]
                         [rs (append (stx-map (lambda (x) (generate-temporary "unboxed-real-"))
                                              #'(cs.real-binding ...))
                                     (list #'real-binding))]
                         [is (append (stx-map (lambda (x) (generate-temporary "unboxed-imag-"))
                                              #'(cs.imag-binding ...))
                                     (list #'imag-binding))]
                         [res '()])
                (cond
                 [(null? e1)
                  (reverse res)]
                 [else
                  (define o-real? (was-real? o2))
                  (define e-real? (was-real? (car e2)))
                  (define both-real? (and o-real? e-real?))
                  (define o-nf (as-non-float o1))
                  (define e-nf (as-non-float (car e1)))
                  (define new-imag-id (if both-real?
                                          (mark-as-real (car is))
                                          (car is)))
                  (loop (car rs) new-imag-id (cdr e1) (cdr e2) (cdr rs) (cdr is)
                        ;; complex multiplication, imag part, then real part (reverse)
                        ;; we eliminate operations on the imaginary parts of reals
                        (list* #`((#,new-imag-id)
                                  #,(cond ((and o-real? e-real?) #'0.0)
                                          (o-real? #`(unsafe-fl* #,o1 #,(car e2)))
                                          (e-real? #`(unsafe-fl* #,o2 #,(car e1)))
                                          (else
                                           #`(unsafe-fl+ (unsafe-fl* #,o2 #,(car e1))
                                                         (unsafe-fl* #,o1 #,(car e2))))))
                               #`((#,(car rs))
                                  #,(cond [(and o-nf e-nf both-real?)
                                           ;; we haven't seen float operands yet, so
                                           ;; shouldn't prematurely convert to floats
                                           (mark-as-non-float (car rs))
                                           #`(* #,o-nf #,e-nf)]
                                          [(or o-real? e-real?)
                                           #`(unsafe-fl*
                                              #,(if (as-non-float o1)
                                                    ;; we hit floats, need to coerce
                                                    #`(real->double-flonum #,o1)
                                                    o1)
                                              #,(car e1))]
                                          [else
                                           #`(unsafe-fl- (unsafe-fl* #,o1 #,(car e1))
                                                         (unsafe-fl* #,o2 #,(car e2)))]))
                               res))])))))
  (pattern (#%plain-app op:*^ :unboxed-float-complex-opt-expr)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:do [(log-unboxing-opt "unboxed unary float complex")])

  (pattern (#%plain-app op:conjugate^ c:unboxed-float-complex-opt-expr)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with real-binding #'c.real-binding
    #:with imag-binding (generate-temporary "unboxed-imag-")
    #:do [(log-unboxing-opt "unboxed unary float complex")]
    #:with (bindings ...)
      #`(c.bindings ...
         ((imag-binding) (unsafe-fl* -1.0 c.imag-binding))))


  (pattern (#%plain-app op:exp^ c:unboxed-float-complex-opt-expr)
    #:when (or (subtypeof? this-syntax -FloatComplex)
               (and (log-missed-complex-expr) #f))
    #:with (real-binding imag-binding) (binding-names)
    #:with scaling-factor (generate-temporary "unboxed-scaling-")
    #:do [(log-unboxing-opt "unboxed unary float complex")]
    #:with (bindings ...)
      #`(c.bindings ...
         ((scaling-factor) (unsafe-flexp c.real-binding))
         ((real-binding) (unsafe-fl* (unsafe-flcos c.imag-binding) scaling-factor))
         ((imag-binding) (unsafe-fl* (unsafe-flsin c.imag-binding) scaling-factor))))


  ;; we can eliminate boxing that was introduced by the user
  (pattern (#%plain-app op:make-rectangular^ real:float-arg-expr imag:float-arg-expr)
    #:when (or (subtypeof? this-syntax -FloatComplex)
               (and (log-missed-complex-expr) #f))
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "make-rectangular elimination")]
    #:with (bindings ...)
      #'(((real-binding) real.opt)
         ((imag-binding) imag.opt)))
  (pattern (#%plain-app op:make-polar^ r:float-arg-expr theta:float-arg-expr)
    #:when (or (subtypeof? this-syntax -FloatComplex)
               (and (log-missed-complex-expr) #f))
    #:with radius       (generate-temporary)
    #:with angle        (generate-temporary)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "make-rectangular elimination")]
    #:with (bindings ...)
      #'(((radius)       r.opt)
         ((angle)        theta.opt)
         ((real-binding) (unsafe-fl* radius (unsafe-flcos angle)))
         ((imag-binding) (unsafe-fl* radius (unsafe-flsin angle)))))

  ;; if we see a variable that's already unboxed, use the unboxed bindings
  (pattern :unboxed-var
    #:do [(log-unboxing-opt "leave var unboxed")]
    #:with (bindings ...) #'())

  ;; else, do the unboxing here

  ;; we can unbox literals right away
  (pattern (~or (quote n*:number) (#%expression (quote n*:number)))
    #:do [(define n (syntax->datum #'n*))]
    #:when (not (equal? (imag-part n) 0))
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unboxed literal")]
    #:with (bindings ...)
      #`(((real-binding) '#,(real->double-flonum (real-part n)))
         ((imag-binding) '#,(real->double-flonum (imag-part n)))))

  (pattern e:float-complex-expr
    #:with e* (generate-temporary)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unbox float-complex")]
    #:with (bindings ...)
      #`(((e*) e.opt)
         ((real-binding) (unsafe-flreal-part e*))
         ((imag-binding) (unsafe-flimag-part e*))))

  (pattern e:number-expr
    #:with e* (generate-temporary)
    #:with (real-binding* imag-binding*) (binding-names)
    #:with real-binding (if (and (subtypeof? #'e -Real)
                                 (not (subtypeof? #'e -Flonum)))
                            ;; values that were originally non-floats (e.g.
                            ;; rationals or single floats) may need to be
                            ;; handled specially
                            (mark-as-non-float #'real-binding* #'e*)
                            #'real-binding*)
    #:with imag-binding (if (subtypeof? #'e -Real)
                            ;; values that were originally reals may need to be
                            ;; handled specially
                            (mark-as-real #'imag-binding*)
                            #'imag-binding*)
    #:do [(log-unboxing-opt
            (if (subtypeof? #'e -Flonum)
                "float in complex ops"
                "non float complex in complex ops"))]
    #:with real-binding-value
      (cond
        [(subtypeof? #'e -Flonum) #'e*]
        [(subtypeof? #'e -Real) #'(real->double-flonum e*)]
        [else #'(real->double-flonum (real-part e*))])
    #:with imag-binding-value
      (cond
        [(subtypeof? #'e -Real) #'0.0]
        [else #'(real->double-flonum (imag-part e*))])

    #:with (bindings ...)
      #'(((e*) e.opt)
         ((real-binding) real-binding-value)
         ((imag-binding) imag-binding-value)))
  (pattern e:expr
    #:do [(error (format "non exhaustive pattern match ~a" #'e))]
    #:with (bindings ...) (list)
    #:with real-binding #f
    #:with imag-binding #f))


(define-syntax-class float-complex-opt-expr
  #:commit
  #:attributes (opt)
  #:literal-sets (kernel-literals)
  ;; Dummy pattern that can't actually match.
  ;; We just want to detect "unexpected" Complex _types_ that come up.
  ;; (not necessarily complex _values_, in fact, most of the time this
  ;; case would come up, no actual complex values will be generated,
  ;; but the type system has to play it safe, and must assume that it
  ;; could happen. ex: (sqrt Integer), if the type system can't prove
  ;; that the argument is non-negative, it must assume that complex
  ;; results can happen, even if it never does in the user's program.
  ;; This is exactly what makes complex types like this "unexpected")
  ;; We define unexpected as: the whole expression has a Complex type,
  ;; but none of its subexpressions do. Since our definition of
  ;; arithmetic expression (see the arith-expr syntax class) exclude
  ;; constructors (like make-rectangular) and coercions, this is a
  ;; reasonable definition.
  (pattern e:arith-expr
           #:when (when (and (in-complex-layer? #'e)
                             (for/and ([subexpr (in-syntax #'(e.args ...))])
                               (subtypeof? subexpr -Real)))
                    (log-missed-optimization
                     "unexpected complex type"
                     (string-append
                      "This expression has a Complex type, despite all its "
                      "arguments being reals. If you do not want or expect "
                      "complex numbers as results, you may want to restrict "
                      "the type of the arguments or use float-specific "
                      "operations (e.g. flsqrt), which may have a beneficial "
                      "impact on performance.")
                     this-syntax))
           ;; We don't actually want to match.
           #:when #f
           ;; required, otherwise syntax/parse is not happy
           #:with opt #'#f)


  (pattern (#%plain-app op:make-polar^ r theta)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with exp:unboxed-float-complex-opt-expr this-syntax
    #:do [(log-unboxing-opt "make-polar")]
    #:with opt #`(let*-values (exp.bindings ...)
                   (unsafe-make-flrectangular exp.real-binding exp.imag-binding)))

  (pattern (#%plain-app op:unboxed-fun .
             (~var call (float-complex-call-site-opt-expr #'op.unboxed-info)))
    #:do [(log-unboxing-opt "unboxed call site")
          (log-arity-raising-opt "call to fun with unboxed args")]
    #:with opt ((attribute call.opt-app) #'op))

  (pattern :float-complex-arith-opt-expr))

;; Supports not optimizing in order to support using it to check for optimizable expressions.
;; Thus side effects are hidden behind the optimizing argument and referencing the opt attribute.
(define-syntax-class (float-complex-arith-expr* optimizing)
  #:commit
  #:attributes (opt)
  #:literal-sets (kernel-literals)

  ;; we can optimize taking the real of imag part of an unboxed complex
  ;; hopefully, the compiler can eliminate unused bindings for the other part if it's not used
  (pattern (#%plain-app _:projection^ _:float-complex-expr)
    #:attr opt
      (delay
        (syntax-parse this-syntax
          [(#%plain-app op:projection^ c:unboxed-float-complex-opt-expr)
           (log-unboxing-opt "complex accessor elimination")
           #`(let*-values (c.bindings ...)
               #,(if (or (free-identifier=? #'op #'real-part)
                         (free-identifier=? #'op #'flreal-part)
                         (free-identifier=? #'op #'unsafe-flreal-part))
                     #'c.real-binding
                     #'c.imag-binding))])))

  (pattern (#%plain-app _:magnitude^ _:float-complex-expr)
    #:attr opt
      (delay
        (syntax-parse this-syntax
          [(#%plain-app op:magnitude^ c:unboxed-float-complex-opt-expr)
           (log-unboxing-opt "unboxed unary float complex")
           #`(let*-values (c.bindings ...)
               ;; reuses the algorithm used by the Racket runtime
               (let*-values ([(r) (unsafe-flabs c.real-binding)]
                             [(i) (unsafe-flabs c.imag-binding)])
                 (if (zero? i)
                     r
                     (if (unsafe-fl< i r)
                         (let-values ([(q) (unsafe-fl/ i r)])
                           (unsafe-fl* r
                                       (unsafe-flsqrt (unsafe-fl+ 1.0
                                                                  (unsafe-fl* q q)))))
                         (let-values ([(q) (unsafe-fl/ r i)])
                           (unsafe-fl* i
                                       (unsafe-flsqrt (unsafe-fl+ 1.0
                                                                  (unsafe-fl* q q)))))))))])))


  (pattern (#%plain-app op:float-complex-op e:expr ...)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:attr opt
      (delay
        (syntax-parse this-syntax
          (exp:unboxed-float-complex-opt-expr
           #'(let*-values (exp.bindings ...)
               (unsafe-make-flrectangular exp.real-binding exp.imag-binding))))))

  (pattern (~and :unboxed-var v:float-complex-expr)
    ;; unboxed variable used in a boxed fashion, we have to box
    #:attr opt
      (delay
       (log-unboxing-opt "unboxed complex variable")
       #'(unsafe-make-flrectangular real-binding imag-binding))))


(define-syntax-class possibly-unboxed
  #:attributes ([bindings 1] [real-binding 1] [imag-binding 1] [boxed-binding 1])
  (pattern (#t arg:unboxed-float-complex-opt-expr)
    #:with (bindings ...) #'(arg.bindings ...)
    #:with (real-binding ...) #'(arg.real-binding)
    #:with (imag-binding ...) #'(arg.imag-binding)
    #:with (boxed-binding ...) #'())
  (pattern (#f arg:opt-expr)
    #:with binding-name (generate-temporary 'boxed-binding)
    #:with (bindings ...) #'(((binding-name) arg.opt))
    #:with (real-binding ...) #'()
    #:with (imag-binding ...) #'()
    #:with (boxed-binding ...) #'(binding-name)))

;; takes as argument a structure describing which arguments will be unboxed
;; We cannot log opt here because this doesn't see the full original syntax
(define-syntax-class (float-complex-call-site-opt-expr unboxed-info)
  #:commit
  #:attributes (opt-app)
  ;; call site of a function with unboxed parameters
  ;; the calling convention is: real parts of unboxed, imag parts, boxed
  (pattern (orig-args:expr ...)
    #:with (unboxed-args ...) unboxed-info
    #:attr opt-app
      (λ (op)
        (syntax-parse #'((unboxed-args orig-args) ...)
          [(e:possibly-unboxed ...)
           #`(let*-values (e.bindings ... ...)
               (#,op e.real-binding ... ...
                     e.imag-binding ... ...
                     e.boxed-binding ... ...))]))))


(define-syntax-class/specialize float-complex-arith-opt-expr (float-complex-arith-expr* #t))
(define-syntax-class/specialize float-complex-arith-expr (float-complex-arith-expr* #f))
