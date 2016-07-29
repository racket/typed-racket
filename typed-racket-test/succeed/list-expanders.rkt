#lang racket
(require rackunit
         typed-racket/types/match-expanders
         typed-racket/types/abbrev
         typed-racket/types/base-abbrev)

(test-case
 "Listof: can appear in or patterns"
 (define list-base-type
   (match (make-Listof Univ)
     [(or (Listof: t) t) t]))
 (check-equal? list-base-type Univ))

(test-case
 "MListof: can appear in or patterns"
 (define mlist-base-type
   (match (-mlst Univ)
     [(or (MListof: t) t) t]))
 (check-equal? mlist-base-type Univ))
