#lang racket/base

;; Functionality to optimize a static contract to provide faster checking.
;; Also supports droping checks on either side.

(require
  "../utils/utils.rkt"
  racket/set
  (contract-req)
  "combinators.rkt"
  "structures.rkt"
  racket/syntax
  syntax/private/id-table
  racket/list
  racket/match)



(provide/cond-contract
 [optimize ((static-contract?) (#:trusted-positive boolean? #:trusted-negative boolean? #:recursive-kinds (or/c #f hash?))
                               . ->* . static-contract?)])

;; Reduce a static contract to a smaller simpler one that protects in the same way
(define (reduce sc flat-sc?)
  (match sc
    ;; none/sc cases
    [(listof/sc: (none/sc:)) empty-list/sc]
    [(list/sc: sc1 ... (none/sc:) sc2 ...) none/sc]
    [(set/sc: (none/sc:)) empty-set/sc]
    [(syntax/sc: (none/sc:)) none/sc]
    ;; The following are unsound because chaperones allow operations on these data structures to
    ;; can call continuations and thus be useful even if they cannot return values.
    ;[(vectorof/sc: (none/sc:)) empty-vector/sc]
    ;[(vector/sc: sc1 ... (none/sc:) sc2 ...) none/sc]
    ;[(box/sc: (none/sc:)) none/sc]
    ;[(promise/sc: (none/sc:)) none/sc]
    ;[(hash/sc: (none/sc:) value/sc) empty-hash/sc]
    ;[(hash/sc: key/sc (none/sc:)) empty-hash/sc]

    ;; any/sc cases
    [(cons/sc: (any/sc:) (any/sc:)) cons?/sc]
    [(listof/sc: (any/sc:)) list?/sc]
    [(list/sc: (and scs (any/sc:)) ...) (list-length/sc (length scs))]
    [(immutable-vectorof/sc: (any/sc:)) immutable-vector?/sc]
    [(mutable-vectorof/sc: (any/sc:)) mutable-vector?/sc]
    [(vectorof/sc: (any/sc:)) vector?/sc]
    [(immutable-vector/sc: (and scs (any/sc:)) ...) (immutable-vector-length/sc (length scs))]
    [(mutable-vector/sc: (and scs (any/sc:)) ...) (mutable-vector-length/sc (length scs))]
    [(vector/sc: (and scs (any/sc:)) ...) (vector-length/sc (length scs))]
    [(set/sc: (any/sc:)) set?/sc]
    [(box/sc: (any/sc:)) box?/sc]
    [(syntax/sc: (any/sc:)) syntax?/sc]
    [(promise/sc: (any/sc:)) promise?/sc]
    [(hash/sc: (any/sc:) (any/sc:)) hash?/sc]
    [(mutable-hash/sc: (any/sc:) (any/sc:)) mutable-hash?/sc]
    [(immutable-hash/sc: (any/sc:) (any/sc:)) immutable-hash?/sc]
    [(weak-hash/sc: (any/sc:) (any/sc:)) weak-hash?/sc]

    ;; or/sc cases
    [(or/sc: scs ...)
     (match scs
      [(list) none/sc]
      [(list sc) sc]
      [(? (λ (l) (member any/sc l))) any/sc]
      [(? (λ (l) (member none/sc l)))
       (apply or/sc (remove* (list none/sc) scs))]
      [(? (λ (l) (ormap (match-lambda [(or/sc: _ ...) #true] [_ #false]) l)))
       (define new-scs (flatten-or/sc scs flat-sc?))
       (if new-scs
         (apply or/sc new-scs)
         sc)]
      [else sc])]

    ;; and/sc cases
    [(and/sc: scs ...)
     (match scs
      [(list) any/sc]
      [(list sc) sc]
      [(? (λ (l) (member none/sc l))) none/sc]
      [(? (λ (l) (member any/sc l)))
       (apply and/sc (remove* (list any/sc) scs))]
      [else sc])]


    ;; case->/sc cases
    [(case->/sc: arrs ...)
     (match arrs
       ;; We can turn case->/sc contracts int ->* contracts in some cases.
       [(list (arr/sc: args #f ranges) ...) (=> fail)
        ;; All results must have the same range
        (unless (equal? (set-count (list->set ranges)) 1)
          (fail))
        (define sorted-args (sort args (λ (l1 l2) (< (length l1) (length l2)))))
        (define shortest-args (first sorted-args))
        (define longest-args (last sorted-args))
        ;; The number of arguments must increase by 1 with no gaps
        (unless (equal? (map length sorted-args)
                        (range (length shortest-args)
                               (add1 (length longest-args))))
          (fail))
        ;; All arities must be prefixes of the longest arity
        (unless (for/and ([args (in-list sorted-args)])
                  (equal? args (take longest-args (length args))))
          (fail))
        ;; All the checks passed
        (function/sc
          #t
          (take longest-args (length shortest-args))
          (drop longest-args (length shortest-args))
          empty
          empty
          #f
          (first ranges))]
       [else sc])]



    [else sc]))

;; flatten-or/sc : (listof static-contract?) -> (listof static-contract?)
;; Basically, flatten a list `(list pre-scs ... (or/sc mid-scs ...) post-scs ...)`
;;  to `(list pre-scs ... mid-scs ... post-scs ...)`, but:
;;  - flatten all `or/sc` contracts in the given list
;;  - remove duplicate contracts from the result
;; Uses `flat-sc?` to check that the `mid-scs ...` are either (1) all flat
;;  or (2) all non-flat. Without this check, the flattened contract might
;;  accept a value that the original contract failed for. Example:
;;  consider `(or/c (or/c procedure? (-> boolean?)) (-> integer?))`
;;  and `(or/c procedure? (-> boolean?) (-> integer?))`
;;  and any thunk. The first contract fails and the second passes.
(define (flatten-or/sc scs flat-sc?)
  (define flattened-any? (box #false))
  (define new-scs
    (for/fold ([acc '()])
              ([sc (in-list scs)])
      (match sc
        [(or/sc: inner-scs ...)
         #:when (eq?*/f inner-scs flat-sc?)
         (set-box! flattened-any? #true)
         (set-union acc inner-scs)]
        [_
         (set-add acc sc)])))
  (and (unbox flattened-any?) new-scs))

;; eq?*/f : (-> (listof a) (-> a b) boolean?)
;; Returns #true if (f x) is `eq?` to `(f y)` for all `x`, `y` in the given list.
(define (eq?*/f x* f)
  (define undef 'undef)
  (let loop ((x* x*)
             (prev undef))
    (cond
      [(null? x*)
       #true]
      [(eq? prev undef)
       (loop (cdr x*) (f (car x*)))]
      [(eq? prev (f (car x*)))
       (loop (cdr x*) prev)]
      [else
        #false])))

;; Reduce a static contract assuming that we trusted the current side
;; If `is-weak-side?` is true, then preserve the "head constructor" of the
;;  result --- if `((make-trusted-side-reduce ...) sc #true) = sc+`, then
;;  both `sc` and `sc+` must give the same answer to `contract-first-order`
;;  after instantiation.
(define ((make-trusted-side-reduce flat-sc?) sc is-weak-side?)
  (match sc
    [(->/sc: mand-args opt-args mand-kw-args opt-kw-args rest-arg (list (any/sc:) ...))
     (function/sc #t mand-args opt-args mand-kw-args opt-kw-args rest-arg #f)]
    [(arr/sc: args rest (list (any/sc:) ...))
     (arr/sc args rest #f)]
    [(none/sc:) any/sc]
    [(or/sc: (? flat-sc?) ...)
     #:when (not is-weak-side?)
     any/sc]
    [(? flat-sc?)
     #:when (not is-weak-side?)
     any/sc]
    [(syntax/sc: (? recursive-sc?))
     #:when (not is-weak-side?)
     ;;bg; _temporary_ case to allow contracts from the `Syntax` type.
     ;;    This is temporary until TR has types for immutable-vector
     ;;    and box-immutable & changes the definition of the `Syntax` type.
     any/sc]
    [else sc]))

;; The side of a static contract describes the source of the values that
;;  the contract needs to check.
;; - 'positive : values exported by the server module
;; - 'negative : values imported from a client module
;; - 'both     : values from both server & client
(define (side? v)
  (memq v '(positive negative both)))

;; A _weak side_ is a side that may be optimized with caution --- optimization
;;  cannot change the value of `contract-first-order` on the result.
;; Example:
;;  when optimizing an `(or/sc scs ...)` on the 'positive side,
;;  each of the `scs` should be optimized on the '(weak positive) side,
;;  and their sub-contracts --- if any --- may be optimized on the 'positive side
;;
;;  - `(or/sc integer? (-> boolean? boolean?))`
;;    ==> `(or/sc integer? (-> boolean? any))`
;;    is OK
;;  - `(or/sc integer? (-> boolean? boolean?))`
;;    ==> `(or/sc any/c (-> boolean? boolean?))`
;;    is NOT ok, because the second contract accepts any value and will
;;    let typed functions cross without protection into untyped code.
(define (weak-side? x)
  (match x
   [(list 'weak (? side?))
    #true]
   [_
    #false]))

(define (strengthen-side side)
  (if (weak-side? side)
    (second side)
    side))

(define (weaken-side side)
  (if (weak-side? side)
    side
    `(weak ,side)))

(define (invert-side v)
  (if (weak-side? v)
    (weaken-side (invert-side v))
    (case v
      [(positive) 'negative]
      [(negative) 'positive]
      [(both) 'both])))

(define (combine-variance side var)
  (case var
    [(covariant) side]
    [(contravariant) (invert-side side)]
    [(invariant) 'both]))

;; update-side : sc? weak-side? -> weak-side?
;; Change the current side to something safe & strong-as-possible
;;  for optimizing the sub-contracts of the given `sc`.
(define ((make-update-side flat-sc?) sc side)
  (match sc
   [(or/sc: scs ...)
    #:when (not (andmap flat-sc? scs))
    (weaken-side side)]
   [_
    #:when (guarded-sc? sc)
    (strengthen-side side)]
   [_
    ;; Keep same side by default.
    ;; This is precisely safe for "unguarded" static contracts like and/sc
    ;;  and conservatively safe for everything else.
    side]))

;; guarded-sc? : sc? -> boolean?
;; Returns #true if the given static contract represents a type with a "real"
;;  type constructor. E.g. list/sc is "real" and or/sc is not.
(define (guarded-sc? sc)
  (match sc
   [(or (->/sc: _ _ _ _ _ _)
        (arr/sc: _ _ _)
        (async-channel/sc: _)
        (box/sc: _)
        (case->/sc: _)
        (channel/sc: _)
        (cons/sc: _ _)
        (continuation-mark-key/sc: _)
        (evt/sc: _)
        (hash/sc: _ _)
        (immutable-hash/sc: _ _)
        (immutable-vector/sc: _ ...)
        (immutable-vectorof/sc: _)
        (list/sc: _ ...)
        (listof/sc: _)
        (mutable-hash/sc: _ _)
        (mutable-vector/sc: _ ...)
        (mutable-vectorof/sc: _)
        (parameter/sc: _ _)
        (promise/sc: _)
        (prompt-tag/sc: _ _)
        (sequence/sc: _ ...)
        (set/sc: _)
        (struct/sc: _ _)
        (prefab/sc: _ _)
        (syntax/sc: _)
        (vector/sc: _ ...)
        (vectorof/sc: _)
        (weak-hash/sc: _ _))
    #true]
   [_
     ;; class/sc object/sc rec/sc ...
    #false]))

(define (remove-unused-recursive-contracts sc)
  (define root (generate-temporary))
  (define main-table (make-free-id-table))
  (define (search)
    (define table (make-free-id-table))
    (define (recur sc variance)
      (match sc
        [(recursive-sc-use id)
         (free-id-table-set! table id #t)]
        [(recursive-sc names values body)
         (recur body 'covariant)
         (for ([name (in-list names)]
               [value (in-list values)])
          (free-id-table-set! main-table name ((search) value)))]
        [else
          (sc-traverse sc recur)]))
    (lambda (sc)
      (recur sc 'covariant)
      table))
  (define reachable ((search) sc))
  (define seen (make-free-id-table reachable))
  (let loop ((to-look-at reachable))
    (unless (zero? (free-id-table-count to-look-at))
      (define new-table (make-free-id-table))
      (for ([(id _) (in-free-id-table to-look-at)])
        (for ([(id _) (in-free-id-table (free-id-table-ref main-table id))])
          (unless (free-id-table-ref seen id #f)
            (free-id-table-set! seen id #t)
            (free-id-table-set! new-table id #t))))
      (loop new-table)))

  ;; Determine if the recursive name is referenced in the static contract
  (define (unused? new-name sc)
    (let/ec exit
      (define (recur sc variance)
        (match sc
          [(recursive-sc-use (== new-name free-identifier=?))
           (exit #f)]
          [else
            (sc-traverse sc recur)]))
      (recur sc 'covariant)
      #t))

  (define (trim sc variance)
    (match sc
      [(recursive-sc names values body)
       (define new-body (trim body 'covariant))

       (define new-name-values
         (for/list ([name (in-list names)]
                    [value (in-list values)]
                    #:when (free-id-table-ref seen name #f))
            (list name value)))
       (define new-names (map first new-name-values))
       (define new-values (map (λ (v) (trim v 'covariant))
                               (map second new-name-values)))
       (cond
         [(empty? new-names) new-body]
         [(and
            (equal? (length new-names) 1)
            (recursive-sc-use? new-body)
            (free-identifier=? (first new-names) (recursive-sc-use-name new-body))
            (unused? (first new-names) (first new-values)))
          (first new-values)]
         [else
          (recursive-sc new-names new-values new-body)])]
      [else
        (sc-map sc trim)]))
  (trim sc 'covariant))

(define (make-sc->kind recursive-kinds)
  (if recursive-kinds
    (λ (sc)
      (let loop ([sc sc])
        (match sc
         [(recursive-sc _ _ body)
          (loop body)]
         [(or (recursive-sc-use id)
              (name/sc: id))
          (hash-ref recursive-kinds id #f)]
         [_
          (sc-terminal-kind sc)])))
    sc-terminal-kind))

;; If we trust a specific side then we drop all contracts protecting that side.
(define (optimize sc #:trusted-positive [trusted-positive #f] #:trusted-negative [trusted-negative #f] #:recursive-kinds [recursive-kinds #f])
  (define flat-sc?
    (let ([sc->kind (make-sc->kind recursive-kinds)])
      (λ (sc) (eq? 'flat (sc->kind sc)))))
  (define trusted-side-reduce (make-trusted-side-reduce flat-sc?))
  (define update-side (make-update-side flat-sc?))

  ;; single-step: reduce and trusted-side-reduce if appropriate
  (define (single-step sc maybe-weak-side)
    (define trusted
      (case (strengthen-side maybe-weak-side)
        [(positive) trusted-positive]
        [(negative) trusted-negative]
        [(both) (and trusted-positive trusted-negative)]))

    (reduce
      (if trusted
          (trusted-side-reduce sc (weak-side? maybe-weak-side))
          sc)
      flat-sc?))

  ;; full-pass: single-step at every static contract subpart
  (define (full-pass sc)
    (define ((recur side) sc variance)
      (define curr-side (combine-variance side variance))
      (define next-side (update-side sc curr-side))
      (single-step (sc-map sc (recur next-side)) curr-side))
    ((recur 'positive) sc 'covariant))

  ;; Do full passes until we reach a fix point, and then remove all unneccessary recursive parts
  (let loop ([sc sc])
    (define new-sc (full-pass sc))
    (if (equal? sc new-sc)
        (remove-unused-recursive-contracts new-sc)
        (loop new-sc))))
