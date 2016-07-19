#lang racket/base

;; Test for PR 15330
;;
;; Make sure struct contracts with the same name bound in different
;; places will work correctly

(module base typed/racket/base
 
  (provide (struct-out Record))
  (struct Record ([id : Integer]) #:transparent))

(module yy typed/racket/base
 
  (require (prefix-in roles: (submod ".." base)))
  (provide (struct-out Record))
  (struct Record ([subrec : roles:Record]) #:transparent))

(require (prefix-in role: 'yy)
         (prefix-in roles: 'base))
         
(role:Record-subrec (role:Record (roles:Record 0)))
