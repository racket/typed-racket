#lang racket/base

;; this file cheats to define types for unexported variables
;; that are expanded into by Racket macros
(require
 "../utils/utils.rkt"
 (only-in "../rep/type-rep.rkt" -StructTypeTop)
 racket/promise
 string-constants/string-constant
 racket/private/kw racket/file racket/port syntax/parse racket/path
 (for-template (only-in racket/private/kw kw-expander-proc kw-expander-impl)
               racket/base racket/file racket/port racket/path racket/list)
 (env init-envs)
 (types abbrev numeric-tower)
 (for-syntax racket/base syntax/parse
             (only-in racket/syntax syntax-local-eval)))
(provide make-template-identifier)

(define (make-template-identifier what where)
  (let ([name (module-path-index-resolve (module-path-index-join where #f))])
    (parameterize ([current-namespace (make-empty-namespace)])
      (namespace-attach-module (current-namespace) ''#%kernel)
      (parameterize ([current-module-declare-name name])
        (eval `(,#'module any '#%kernel
                 (#%provide ,what)
                 (define-values (,what) #f))))
      (namespace-require `(for-template ,name))
      (namespace-syntax-introduce (datum->syntax #f what)))))


(define-initial-env initialize-special
  ;; make-promise
  [(make-template-identifier 'delay 'racket/private/promise)
   (-poly (a) (-> (-> a) (-Promise a)))]
  ;; next four for string constants
  [(make-template-identifier 'dynamic-string-constant 'string-constants/string-constant)
   (-> -Symbol -String)]
  [(make-template-identifier 'dynamic-string-constants 'string-constants/string-constant)
   (-> -Symbol (-lst -String))]
  [(make-template-identifier 'this-language 'string-constants/string-constant)
   (-> -Symbol)]
  [(make-template-identifier 'all-languages 'string-constants/string-constant)
   (-> (-lst -Symbol))]
  ;; qq-append
  [(make-template-identifier 'qq-append 'racket/private/qq-and-or)
   (-poly (a b)
         (cl->*
          (-> (-lst a) -Null (-lst a))
          (-> (-lst a) (-lst b) (-lst (Un a b)))))]
  ;; range
  [(make-template-identifier 'range-proc 'racket/list)
   (cl->* (-> -NonPosReal -Null)
          (-> -One (-lst* -Zero))
          (-> -Byte (-lst -Byte))
          (-> -Index (-lst -Index))
          (-> -Fixnum (-lst -NonNegFixnum))
          (-> -Real (-lst -Nat))
          (->opt -PosInt -Byte [-Int] (-lst -PosByte))
          (->opt -Nat -Byte [-Int] (-lst -Byte))
          (->opt -PosInt -Index [-Int] (-lst -PosIndex))
          (->opt -Nat -Index [-Int] (-lst -Index))
          (->opt -Nat -NonNegFixnum [-Int] (-lst -NonNegFixnum))
          (->opt -PosInt -Fixnum [-Nat] (-lst -PosFixnum))
          (->opt -Nat -Fixnum [-Nat] (-lst -NonNegFixnum))
          (->opt -Nat -Nat [-Int] (-lst -Nat))
          (->opt -PosInt -Int [-Nat] (-lst -PosInt))
          (->opt -Nat -Int [-Nat] (-lst -Nat))
          ;; could add cases that guarantee lists of negatives, etc.
          (->opt -Int -Real [-Int] (-lst -Int))
          (->opt -Rat -Real [-Rat] (-lst -Rat))
          (->opt -Flonum -Real [-Flonum] (-lst -Flonum))
          (->opt -SingleFlonum -Real [-SingleFlonum] (-lst -SingleFlonum))
          (->opt -InexactReal -Real [-InexactReal] (-lst -InexactReal))
          (->opt -Real -Real [-Real] (-lst -Real)))]
  ;; normalise-inputs
  [(make-template-identifier 'normalise-inputs 'racket/private/for)
   (-poly (a)
          (-> -Symbol -String (-> a -Boolean) (-> a -Nat) a -Nat (Un (-val #f) -Nat) -Nat (-values (list a -Index -Index -Index))))]
  ;; make-sequence
  [(make-template-identifier 'make-sequence 'racket/private/for)
   (-poly (a b)
          (let ([seq-vals
                 (lambda (a)
                   (-values (list
                             (-> Univ (-values a))
                             (Un (-> Univ Univ) (-val #f))
                             (-> Univ Univ)
                             Univ
                             (Un (-> Univ Univ) (-val #f))
                             (Un (->* a Univ) (-val #f))
                             (Un (->* (cons Univ a) Univ) (-val #f)))))])
            (cl->*
             (-> Univ -Byte         (seq-vals (list -Byte)))
             (-> Univ -Index        (seq-vals (list -Index)))
             ;; Generous. Negative numbers aren't allowed.
             (-> Univ -Fixnum       (seq-vals (list -NonNegFixnum)))
             (-> Univ -Int          (seq-vals (list -Nat)))
             (-> Univ (-seq a) (seq-vals (list a)))
             (-> Univ (-seq a b) (seq-vals (list a b))))))]
  ;; check-range
  [(make-template-identifier 'check-range 'racket/private/for)
   (-> Univ Univ Univ -Void)]
  ;; check-naturals
  [(make-template-identifier 'check-naturals 'racket/private/for)
   (-> Univ -Void)]
  ;; check-string
  [(make-template-identifier 'check-string 'racket/private/for)
   (-> Univ -Void)]
  ;; check-bytes
  [(make-template-identifier 'check-bytes 'racket/private/for)
   (-> Univ -Void)]
  ;; check-list
  [(make-template-identifier 'check-list 'racket/private/for)
   (-> Univ -Void)]
  ;; check-fXvector for flvector
  [(make-template-identifier 'check-flvector 'racket/flonum)
   (-> Univ -Void)]
  ;; check-fXvector for fxvector
  [(make-template-identifier 'check-fxvector 'racket/fixnum)
   (-> Univ -Void)]
  ;; check-vector
  [(make-template-identifier 'check-vector 'racket/private/for)
   (-> Univ -Void)]
  ;; in-range
  [(make-template-identifier 'in-range 'racket/private/for)
   (cl->* (-> -Byte (-seq -Byte))
          (-> -Index (-seq -Index))
          (-> -Fixnum (-seq -NonNegFixnum))
          (-> -Real (-seq -Nat))
          (->opt -PosInt -Byte [-Int] (-seq -PosByte))
          (->opt -Nat -Byte [-Int] (-seq -Byte))
          (->opt -PosInt -Index [-Int] (-seq -PosIndex))
          (->opt -Nat -Index [-Int] (-seq -Index))
          (->opt -Nat -NonNegFixnum [-Int] (-seq -NonNegFixnum))
          (->opt -PosInt -Fixnum [-Nat] (-seq -PosFixnum))
          (->opt -Nat -Fixnum [-Nat] (-seq -NonNegFixnum))
          (->opt -Nat -Nat [-Int] (-seq -Nat))
          (->opt -PosInt -Int [-Nat] (-seq -PosInt))
          (->opt -Nat -Int [-Nat] (-seq -Nat))
          ;; could add cases that guarantee lists of negatives, etc.
          (->opt -Int -Real [-Int] (-seq -Int))
          (->opt -Rat -Real [-Rat] (-seq -Rat))
          (->opt -Flonum -Real [-Flonum] (-seq -Flonum))
          (->opt -SingleFlonum -Real [-SingleFlonum] (-seq -SingleFlonum))
          (->opt -InexactReal -Real [-InexactReal] (-seq -InexactReal))
          (->opt -Real -Real [-Real] (-seq -Real)))]
  ;; in-naturals
  [(make-template-identifier 'in-naturals 'racket/private/for)
   (cl->* (-> (-seq -Nat))
          (-> -PosInt (-seq -PosInt))
          (-> -Int (-seq -Nat)))]
  ;; in-list
  [(make-template-identifier 'in-list 'racket/private/for)
   (-poly (a) (-> (-lst a) (-seq a)))]
  ;; in-mlist
  [(make-template-identifier 'in-mlist 'racket/private/for)
   (-poly (a) (-> (-mlst a) (-seq a)))]
  ;; in-vector
  [(make-template-identifier 'in-vector 'racket/private/for)
   (-poly (a)
          (cl->* (->opt (-vec a) [-Int (-opt -Int) -Int] (-seq a))
                 (->opt -VectorTop [-Int (-opt -Int) -Int] (-seq Univ))))]
  ;; in-string
  [(make-template-identifier 'in-string 'racket/private/for)
   (->opt -String [-Int (-opt -Int) -Int] (-seq -Char))]
  ;; in-bytes
  [(make-template-identifier 'in-bytes 'racket/private/for)
   (->opt -Bytes [-Int (-opt -Int) -Int] (-seq -Byte))]
  ;; in-hash and friends
  [(make-template-identifier 'default-in-hash 'racket/private/for)
   (-poly (a b)
          (cl-> [((-HT a b)) (-seq a b)]
                [(-HashTableTop) (-seq Univ Univ)]))]
  [(make-template-identifier 'default-in-hash-keys 'racket/private/for)
   (-poly (a b)
          (cl-> [((-HT a b)) (-seq a)]
                [(-HashTableTop) (-seq Univ)]))]
  [(make-template-identifier 'default-in-hash-values 'racket/private/for)
   (-poly (a b)
          (cl-> [((-HT a b)) (-seq b)]
                [(-HashTableTop) (-seq Univ)]))]
  [(make-template-identifier 'default-in-hash-pairs 'racket/private/for)
   (-poly (a b)
          (cl-> [((-HT a b)) (-seq (-pair a b))]
                [(-HashTableTop) (-seq (-pair Univ Univ))]))]
  [(make-template-identifier 'default-in-immutable-hash 'racket/private/for)
   (-poly (a b) (-> (-Immutable-HT a b) (-seq a b)))]
  [(make-template-identifier 'default-in-immutable-hash-keys 'racket/private/for)
   (-poly (a b) (-> (-Immutable-HT a b) (-seq a)))]
  [(make-template-identifier 'default-in-immutable-hash-values 'racket/private/for)
   (-poly (a b) (-> (-Immutable-HT a b) (-seq b)))]
  [(make-template-identifier 'default-in-immutable-hash-pairs 'racket/private/for)
   (-poly (a b) (-> (-Immutable-HT a b) (-seq (-pair a b))))]
  [(make-template-identifier 'default-in-mutable-hash 'racket/private/for)
   (-poly (a b) (-> (-Mutable-HT a b) (-seq a b)))]
  [(make-template-identifier 'default-in-mutable-hash-keys 'racket/private/for)
   (-poly (a b) (-> (-Mutable-HT a b) (-seq a)))]
  [(make-template-identifier 'default-in-mutable-hash-values 'racket/private/for)
   (-poly (a b) (-> (-Mutable-HT a b) (-seq b)))]
  [(make-template-identifier 'default-in-mutable-hash-pairs 'racket/private/for)
   (-poly (a b) (-> (-Mutable-HT a b) (-seq (-pair a b))))]
  [(make-template-identifier 'default-in-weak-hash 'racket/private/for)
   (-poly (a b) (-> (-Weak-HT a b) (-seq a b)))]
  [(make-template-identifier 'default-in-weak-hash-keys 'racket/private/for)
   (-poly (a b) (-> (-Weak-HT a b) (-seq a)))]
  [(make-template-identifier 'default-in-weak-hash-values 'racket/private/for)
   (-poly (a b) (-> (-Weak-HT a b) (-seq b)))]
  [(make-template-identifier 'default-in-weak-hash-pairs 'racket/private/for)
   (-poly (a b) (-> (-Weak-HT a b) (-seq (-pair a b))))]
  [(make-template-identifier 'mutable? 'racket/private/for)
   (-> Univ -Boolean)]
  [(make-template-identifier 'not-weak? 'racket/private/for)
   (-> -HashTableTop -Boolean)]
  ;; check-in-hash and friends
  [(make-template-identifier 'check-in-hash 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-hash-keys 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-hash-values 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-hash-pairs 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-mutable-hash 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-mutable-hash-keys 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-mutable-hash-values 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-mutable-hash-pairs 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-immutable-hash 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-immutable-hash-keys 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-immutable-hash-values 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-immutable-hash-pairs 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-weak-hash 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-weak-hash-keys 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-weak-hash-values 'racket/private/for)
   (-> Univ -Void)]
  [(make-template-identifier 'check-in-weak-hash-pairs 'racket/private/for)
   (-> Univ -Void)]
  ;; in-port
  [(make-template-identifier 'in-port 'racket/private/for)
   (-poly (a)
          (cl->* (-> (-seq Univ))
                 (->opt (-> -Input-Port (Un a (-val eof))) [-Input-Port] (-seq a))))]
  ;; in-input-port-bytes
  [(make-template-identifier 'in-input-port-bytes 'racket/private/for)
   (-> -Input-Port (-seq -Byte))]
  ;; in-input-port-chars
  [(make-template-identifier 'in-input-port-chars 'racket/private/for)
   (-> -Input-Port (-seq -Char))]
  ;; in-lines
  [(make-template-identifier 'in-lines 'racket/private/for)
   (->opt [-Input-Port -Symbol] (-seq -String))]
  ;; in-bytes-lines
  [(make-template-identifier 'in-bytes-lines 'racket/private/for)
   (->opt [-Input-Port -Symbol] (-seq -Bytes))]
  ;; in-directory
  [(make-template-identifier '*in-directory 'racket/private/for)
   (->opt [(Un (-val #f) -Pathlike) (-> -Path Univ)] (-seq -Path))]
  ;; in-producer
  [(make-template-identifier 'in-producer 'racket/private/for)
   (-polydots (a b)
     (cl->* (-> (-> a) (-seq a))
            (->... (list (->... '() [b b] a)
                         (-> a -Boolean))
                   [b b]
                   (-seq a))))]
  ;; in-value
  [(make-template-identifier 'in-value 'racket/private/for)
   (-poly (a) (-> a (-seq a)))]
  ;; in-indexed
  [(make-template-identifier 'in-indexed 'racket/private/for)
   (-poly (a) (-> (-seq a) (-seq a -Nat)))]
  ;; in-set
  [(make-template-identifier 'in-set 'racket/private/set)
   (-poly (a) (-> (-set a) (-seq a)))]
  ;; check-in-bytes-lines
  [(make-template-identifier 'check-in-bytes-lines 'racket/private/for)
   (-> Univ Univ Univ)]
  ;; check-in-lines
  [(make-template-identifier 'check-in-lines 'racket/private/for)
   (-> Univ Univ Univ)]
  ;; check-in-port
  [(make-template-identifier 'check-in-port 'racket/private/for)
   (-> Univ Univ Univ)]
  ;; from the expansion of `with-syntax' or `quasisyntax'
  [(make-template-identifier 'apply-pattern-substitute 'racket/private/stxcase)
   (->* (list (-Syntax Univ) Univ) Univ (-Syntax Univ))]
  ;; same
  [(make-template-identifier 'with-syntax-fail 'racket/private/with-stx)
   (-> (-Syntax Univ) (Un))]
  ; from `quasisyntax'
  [(make-template-identifier 'check-splicing-list 'racket/private/qqstx)
   (-> Univ (-Syntax Univ) (-Syntax Univ))]
  ;; more from with-syntax, a Guide is ...
  [(make-template-identifier 't-append 'racket/private/template)
   (-> (-lst (-Syntax Univ)) -Stxish -Stxish)]
  ;; ... or a Guide is ...
  [(make-template-identifier 't-resyntax 'racket/private/template)
   (-> (Un (-val #f) (-Syntax Univ)) (-Syntax Univ) Univ (-Syntax Univ))]
  ;; ... or a Guide is ...
  [(make-template-identifier 't-relocate 'racket/private/template)
   (-> (-Syntax Univ) (Un (-val #f) (-Syntax Univ)) (-Syntax Univ))]
  ;; ... or a Guide is ...
  [(make-template-identifier 't-orelse* 'racket/private/template)
   (-> (-> -Stxish) (-> -Stxish) -Stxish)]
  ;; ... or a Guide is ...
  [(make-template-identifier 't-struct 'racket/private/template)
   (-> Univ (-lst (-Syntax Univ)) (-Syntax Univ))]
  ;; ... or a Guide is ...
  [(make-template-identifier 'h-splice 'racket/private/template)
   (-> Univ Univ Univ (-lst (-Syntax Univ)))]
  ;; ... or a Guide is ...
  [(make-template-identifier 't-subst 'racket/private/template)
   (->* (list (Un (-val #f) (-Syntax Univ)) (-Syntax Univ) Univ) Univ (-Syntax Univ))]
  ;; ... or a Guide is ...
  [(make-template-identifier 'check-same-length 'racket/private/template)
   (-> (Un (-val #f) (-Syntax Univ)) (Un (-val #f) (-Syntax Univ)) (-lst Univ) -Void)]
  ;; from the expansion of `make-temp-file`
  [(make-template-identifier 'make-temporary-file/proc 'racket/file)
   (->opt [-String (Un -Pathlike (-val 'directory) (-val #f)) (-opt -Pathlike)]
          -Path)]
  ;; from the (lifted) portion of the expansion of keyword lambdas
  [(make-template-identifier 'make-required 'racket/private/kw)
   (-> Univ Univ Univ Univ Univ)]
  [(make-template-identifier 'missing-kw 'racket/private/kw)
   (->* (list Univ) Univ Univ)]
  [(make-template-identifier 'prop:named-keyword-procedure 'racket/private/kw)
   -Struct-Type-Property]
  [(make-template-identifier 'struct:keyword-procedure/arity-error 'racket/private/kw)
   -StructTypeTop]
  [(make-template-identifier 'struct:keyword-method/arity-error 'racket/private/kw)
   -StructTypeTop]
  ;; from the expansion of `define-runtime-path`
  [(make-template-identifier 'path-of 'racket/runtime-path)
   (-> -Path -Path)]
  [(make-template-identifier 'resolve-paths 'racket/runtime-path)
   ;; finite approximation of the type
   ;;   (-> ... 2 args ... (List Pathlike ..._n) -> (List Path ..._n))
   (cl->* (-> -Variable-Reference (Un (-> -Path) (-> -Void))
              (-lst* (Un -Pathlike (-lst* (-val 'lib) -Pathlike #:tail (-lst -Pathlike))))
              (-lst* -Path))
          ;; this case is for `define-runtime-module-path-index`
          (-> -Variable-Reference (Un (-> -Path) (-> -Void))
              (-lst* (-lst* (-val 'module) -Module-Path -Variable-Reference))
              (-lst* -Module-Path-Index))
          (-> -Variable-Reference (Un (-> -Path) (-> -Void))
              (-lst* -Pathlike -Pathlike)
              (-lst* -Path -Path))
          (-> -Variable-Reference (Un (-> -Path) (-> -Void))
              (-lst* -Pathlike -Pathlike -Pathlike)
              (-lst* -Path -Path -Path)))]
  [(make-template-identifier 'extract-module-file 'racket/private/this-expression-source-directory)
   (-> (-Syntax Univ) -Path)]
  ;; for `define-runtime-module-path`
  [(make-template-identifier 'combine-module-path 'racket/runtime-path)
   (-> -Variable-Reference -Module-Path -Resolved-Module-Path)]
  ;; in-fxvector, in-flvector, in-extflvector
  [(make-template-identifier 'in-fxvector* 'racket/fixnum)
   (-> -FxVector (-seq -Fixnum))]
  [(make-template-identifier 'in-flvector* 'racket/flonum)
   (-> -FlVector (-seq -Flonum))]
  [(make-template-identifier 'in-extflvector* 'racket/extflonum)
   (-> -ExtFlVector (-seq -ExtFlonum))]
  [(make-template-identifier 'make-namespace-anchor 'racket/private/namespace)
   (-> -Variable-Reference -Namespace-Anchor)]
  [(make-template-identifier 'check-logger-or-false 'racket/private/logger)
   (-> -Symbol Univ (Un (-val #f) -Logger))]
  [(make-template-identifier 'place/proc 'racket/place)
   (-> -Variable-Reference -Symbol -Symbol (-> -Symbol -Module-Path -Symbol (-opt -Input-Port) (-opt -Output-Port) (-opt -Output-Port) -Place)
       (-opt -Input-Port) (-opt -Output-Port) (-opt -Output-Port) -Place)]
  [(make-template-identifier 'start-place 'racket/place)
   (-> -Symbol -Module-Path -Symbol (-opt -Input-Port) (-opt -Output-Port) (-opt -Output-Port) -Place)]
  )
