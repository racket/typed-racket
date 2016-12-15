#lang typed/racket/base

(provide (all-defined-out))

(define-type Directory-Record directory-record)
(define-type Info-Ref (->* (Symbol) ((-> Any)) Any))
(define-type Info-Directory-Mode (U 'preferred 'all-available 'no-planet 'no-user))
(define-type Info-Directory-Record-Key (U 'preferred 'all-available))

(require/typed/provide
 setup/getinfo
 [#:struct directory-record
  ([maj : Integer]
   [min : Integer]
   [spec : Any]
   [path : Path]
   [syms : (Listof Symbol)])
  #:extra-constructor-name make-directory-record]
 [get-info
  (-> (Listof String)
      [#:namespace (Option Namespace)]
      [#:bootstrap? Any]
      (Option Info-Ref))]
 [get-info/full
  (-> Path-String
      [#:namespace (Option Namespace)]
      [#:bootstrap? Any]
      (Option Info-Ref))]
 [find-relevant-directories
  (->* ((Listof Symbol)) (Info-Directory-Mode) (Listof Path))]
 [find-relevant-directory-records
  (-> (Listof Symbol) Info-Directory-Record-Key (Listof Directory-Record))]
 [reset-relevant-directories-state!
  (-> Void)])
