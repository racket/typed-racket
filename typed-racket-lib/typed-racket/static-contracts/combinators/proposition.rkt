#lang racket/base


(require
  "../../utils/utils.rkt"
  "../structures.rkt"
  "../constraints.rkt"
  "symbolic-object.rkt"
  racket/match
  racket/generic
  (for-template racket/base racket/contract/base)
  (contract-req))

(provide-for-cond-contract proposition-contract?)

(struct proposition-contract static-contract () #:transparent)


(define/match (flat-lambda-write v port mode)
  [((flat-named-lambda/sc _ arg body) port mode)
   (define recur
     (case mode
       [(#t) write]
       [(#f) display]
       [else (lambda (p port) (print p port mode))]))
   (define-values (open close)
     (if (equal? mode 0)
         (values "(" ")")
         (values "#<" ">")))
   (display open port)
   (fprintf port "λ")
   (display " (" port)
   (recur arg port)
   (display ") " port)
   (recur body port)
   (display close port)])

;; flat-named-lambda/sc
;;
;; represents a contract of the following form:
;; (λ (arg) body) which is printed as "name".
;;
;; These are used to represent the relatively arbitrary logical
;; predicates that can occur in areas like refinement types.
;; For example, (Refine [x : τ] ψ) can generate τ/c for the
;; τ-type as we always have for simple types, and the refining
;; proposition can we written as a contract ψ/c where 'x' might be
;; free in ψ/c, then we can generate the following compound contract:
;; (and/c τ/c (flat-named-lambda/sc x ψ/c))
;;
;; (where the name is ommitted for simplicity)
;;
;; Note: the contents of 'body' may count on 'arg' being bound
;; in the contract syntax they generate.
(struct flat-named-lambda/sc static-contract (name arg body)
  #:transparent
  #:methods gen:sc
  [(define/match (sc-map v f)
     [((flat-named-lambda/sc name arg body) f)
      (flat-named-lambda/sc name (f arg 'covariant) (f body 'covariant))])
   (define/match (sc-traverse v f)
     [((flat-named-lambda/sc _ arg body) f)
      (f arg 'covariant)
      (f body 'covariant)])
   (define/generic sc->c sc->contract)
   (define/match (sc->contract v f)
     [((flat-named-lambda/sc name arg body) f)
      #`(flat-named-contract
         #,name
         (λ (#,(sc->c arg f)) #,(sc->c body f)))])
   (define/match (sc->constraints v f)
     [((flat-named-lambda/sc _ arg body) f)
      (merge-restricts 'flat (f arg) (f body))])
   (define (sc-terminal-kind _) 'flat)]
  #:methods gen:custom-write
  [(define write-proc flat-lambda-write)])

;; is-flat-type/sc
;;
;; Allows us to generate a contract that will check
;; if 'obj' is of type 'type'
;;
;; For this to be valid, these often need to appear in
;; a flat-named-lambda/sc to ensure all identifiers in
;; the generated contract syntax are bound.
;;
;; type -- needs to be a type w/ a flat contract
;; so we can use it directly as a predicate.
(struct is-flat-type/sc proposition-contract (obj type)
  #:transparent
  #:methods gen:sc
  [(define/match (sc-map v f)
     [((is-flat-type/sc obj type) f)
      (is-flat-type/sc (f obj 'covariant) (f type 'covariant))])
   (define/match (sc-traverse v f)
     [((is-flat-type/sc obj type) f)
      (f obj 'covariant)
      (f type 'covariant)])
   (define/generic sc->c sc->contract)
   (define/match (sc->contract v f)
     [((is-flat-type/sc obj type) f)
      #`(#,(f type) #,(sc->c obj f))])
   (define/match (sc->constraints v f)
     [((is-flat-type/sc obj type) f)
      (merge-restricts 'flat (f obj) (f type))])])

;; not-flat-type/sc
;;
;; Allows us to generate a contract that will check
;; if 'obj' is not of type 'type'.
;;
;; See is-flat-type/sc for more details.
(struct not-flat-type/sc proposition-contract (obj type)
  #:transparent
  #:methods gen:sc
  [(define/match (sc-map v f)
     [((not-flat-type/sc obj type) f)
      (not-flat-type/sc (f obj 'covariant) (f type 'covariant))])
   (define/match (sc-traverse v f)
     [((not-flat-type/sc obj type) f)
      (f obj 'covariant)
      (f type 'covariant)])
   (define/generic sc->c sc->contract)
   (define/match (sc->contract v f)
     [((not-flat-type/sc obj type) f) #`(not (#,(f type) #,(sc->c obj f)))])
   (define/match (sc->constraints v f)
     [((not-flat-type/sc obj type) f)
      (merge-restricts 'flat (f obj) (f type))])])

;; used to generate contracts specifying that
;; lhs is <= rhs
;;
;; These can contain references to variables that
;; might be free if they are not contained in a
;; flat-named-lambda/sc.
(struct leq/sc proposition-contract (lhs rhs)
  #:transparent
  #:methods gen:sc
  [(define/match (sc-map v f)
     [((leq/sc lhs rhs) f)
      (leq/sc (f lhs 'covariant) (f rhs 'covariant))])
   (define/match (sc-traverse v f)
     [((leq/sc lhs rhs) f)
      (f lhs 'covariant)
      (f rhs 'covariant)])
   (define/generic sc->c sc->contract)
   (define/match (sc->contract v f)
     [((leq/sc lhs rhs) f) #`(<= #,(sc->c lhs f) #,(sc->c rhs f))])
   (define/match (sc->constraints v f)
     [((leq/sc lhs rhs) f)
      (merge-restricts 'flat (f lhs) (f rhs))])])

(struct and-prop/sc proposition-contract (args)
  #:transparent
  #:methods gen:sc
  [(define/match (sc-map v f)
     [((and-prop/sc args) f)
      (and-prop/sc (for/list ([arg (in-list args)])
                     (f arg 'covariant)))])
   (define/match (sc-traverse v f)
     [((and-prop/sc args) f)
      (for ([arg (in-list args)])
        (f arg 'covariant))])
   (define/generic sc->c sc->contract)
   (define/match (sc->contract v f)
     [((and-prop/sc args) f)
      #`(and #,@(for/list ([arg (in-list args)])
                  (sc->c arg f)))])
   (define/match (sc->constraints v f)
     [((and-prop/sc args) f)
      (merge-restricts* 'flat (map f args))])])

(struct or-prop/sc proposition-contract (args)
  #:transparent
  #:methods gen:sc
  [(define/match (sc-map v f)
     [((or-prop/sc args) f)
      (or-prop/sc (for/list ([arg (in-list args)])
                    (f arg 'covariant)))])
   (define/match (sc-traverse v f)
     [((or-prop/sc args) f)
      (for ([arg (in-list args)])
        (f arg 'covariant))])
   (define/generic sc->c sc->contract)
   (define/match (sc->contract v f)
     [((or-prop/sc args) f)
      #`(or #,@(for/list ([arg (in-list args)])
                 (sc->c arg f)))])
   (define/match (sc->constraints v f)
     [((or-prop/sc args) f)
      (merge-restricts* 'flat (map f args))])])


(provide/cond-contract
 [struct flat-named-lambda/sc ([name string?] [arg id/sc?] [body proposition-contract?])]
 [struct is-flat-type/sc ([obj symbolic-object-contract?] [type static-contract?])]
 [struct not-flat-type/sc ([obj symbolic-object-contract?] [type static-contract?])]
 [struct leq/sc ([lhs symbolic-object-contract?] [rhs symbolic-object-contract?])]
 [struct and-prop/sc ([args (listof proposition-contract?)])]
 [struct or-prop/sc ([args (listof proposition-contract?)])])
