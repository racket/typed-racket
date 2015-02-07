#lang racket/base
(require (except-in "../utils/utils.rkt" infer)
         racket/match racket/lazy-require racket/list
         (except-in racket/contract ->* -> one-of/c)
         (prefix-in c: (contract-req))
         (rep type-rep filter-rep object-rep rep-utils)
         (types utils base-abbrev abbrev filter-ops)
         (utils tc-utils))

(lazy-require
 ("../typecheck/tc-subst.rkt" (subst-type subst-filter)))


(provide flatten-nested-props extract-props-from-type)

;; recursively descend into filters/types extracting info
;; that should be propogated to the top level but is
;; currently in a refinement's prop


(define/cond-contract (flatten-nested-props f)
  (c:-> Filter/c (c:listof Filter/c))

  (define stack (list (cons #f f)))
  (define (push obj prop)
    (set! stack (cons (cons obj prop)
                      stack)))
  (define (pop)
    (match stack
      ['() #f]
      [(cons x xs) (begin (set! stack xs)
                          x)]))
  
  (let loop ([ps '()])
    (match (pop)
      [#f ps] 
      [(cons obj f) 
       (define f* (extract-nested-props f obj push))
       (loop (cons f* ps))]
      [x (int-err "invalid list of objs/props! ~a" x)])))

;; TODO(amk) turn unions w/ nested refinements
;; into logical propositions instead!
(define/cond-contract (extract-props-from-type x ty)
  (c:-> (c:or/c identifier? Object?) Type? 
        (values Type? (c:listof Filter/c)))
  
  (define stack empty)
  (define (push obj prop)
    (set! stack (cons (cons obj prop)
                      stack)))
  (define (pop)
    (match stack
      ['() #f]
      [(cons x xs) (begin (set! stack xs)
                          x)]))
  
  (define obj (if (identifier? x) (-id-path x) x))
  (define ty* (extract-nested-props ty obj push))
  
  
  (let loop ([ps '()])
      (match (pop)
        [#f (values ty* ps)]
        [(cons obj f) 
         (define f* (extract-nested-props f obj push))
         (loop (cons f* ps))]
        [x (int-err "invalid list of objs/props! ~a" x)])))

(define/cond-contract (extract-nested-props a obj save)
  (c:-> (c:or/c Type? Filter/c) (c:or/c #f Object?) (c:-> Object? Filter/c void?)
        (c:or/c Type? Filter/c))
  
  (define ((sift-t obj) ty)
    (type-case 
     (#:Type (sift-t #f) #:Filter (sift-f #f) #:Object values)
     ty
     [#:arr dom rng rest drest kws dep?   ty]
     [#:Union elems   ty]
     ;; TODO(AMK) Any other types we have to ignore the inside of?
     ;; Listof should get covered by union... right? maybe ignore Mu? or any
     ;; that needs resolved?
     [#:Ref x type prop (if obj
                            (begin (save obj (subst-filter prop x obj #t))
                                   ((sift-t obj) (subst-type type x obj #t)))
                            ty)]
     
     [#:Pair t1 t2 (if obj
                       (-pair ((sift-t (ObjCar obj)) t1)
                              ((sift-t (ObjCdr obj)) t2))
                       ty)]
     
     [#:MPair t1 t2 (if obj
                        (-mpair ((sift-t (ObjCar obj)) t1)
                                ((sift-t (ObjCdr obj)) t2))
                        ty)]
     ;;TODO(amk) support these
     #;[#:Syntax t (if obj
                     (-syntax ((sift-t (ObjSyntax obj)) t))
                     ty)]
     
     #;[#:Promise t (if obj
                      (-promise ((sift-t (ObjForce obj)) t))
                      ty)]
     ;; TODO(amk) recurse into each struct field w/ approp path?
     #;[#:Struct ]))
  (define ((sift-f obj) f)
    
    (filter-case (#:Type values #:Filter (sift-f obj) #:Object values)
                 f
                 [#:TypeFilter t obj* (if (debruijn-path? obj*)
                                          f
                                          (-filter ((sift-t obj*) t) obj*))]
                 [#:AndFilter
                  fs
                  (apply -and (map (sift-f obj) fs))]
                 [#:OrFilter 
                  fs 
                  ;; we want to expand ors, but we have to keep the expanded
                  ;; info contained inside the disjunction
                  (let ([fs* (for/list ([f (in-list fs)])
                               (apply -and (flatten-nested-props f)))])
                    (apply -or fs*))]))
  
  (cond
    [(Filter? a) ((sift-f obj) a)]
    [(Type? a) ((sift-t obj) a)]
    [else (int-err "invalid flatten-nested-props argument ~a" a)]))

(define (debruijn-path? o)
  (match o 
    [(Path: _ x) (list? x)]
    [_ #f]))

;; TODO(amk) I can't figure out a place to put these functions where
;; they're accessible everywhere they need to be and don't have
;; issues where things like 'make-CarPE' aren't defined yet
(define (ObjCar p) p
  (match p
    [(Path: lpe x) (make-Path (cons (make-CarPE) lpe) x)]
    [_ p]))

(define (ObjCdr p) p
  (match p
    [(Path: lpe x) (make-Path (cons (make-CdrPE) lpe) x)]
    [_ p]))

(define (ObjSyntax p)
  (match p
    [(Path: lpe x) (make-Path (cons (make-SyntaxPE) lpe) x)]
    [_ p]))

(define (ObjForce p)
  (match p
    [(Path: lpe x) (make-Path (cons (make-ForcePE) lpe) x)]
    [_ p]))
