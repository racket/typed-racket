#lang typed/racket/base
(module aaa-mod typed/racket/base
  (provide (all-defined-out))
  (struct animal ([a : Number] [b : (-> Number Number)]) #:property prop:procedure (struct-field-index b))
  (struct cat animal ([c : Number]))
  (struct a-cat cat ())
  (struct b-cat cat ([d : (-> Number String)]) #:property prop:procedure (struct-field-index d))
  (struct dog animal ([c : (-> String (Listof String))]) #:property prop:procedure (struct-field-index c))
  (struct super-dog dog () #:property prop:procedure
    (lambda ([me : super-dog] [a1 : String]) : Number
      (length ((dog-c me) a1))))

  (struct fruit ([a : Number] [b : (-> Number String)]) #:type-name Fruit #:property prop:procedure (struct-field-index b))
  (struct apple fruit ([c : Number]) #:type-name Apple)

  (struct plant ([a : Number]) #:property prop:procedure (lambda ([me : plant] [a1 : String]) : Number
                                                           (+ (plant-a me) (string-length a1))))
  (struct tree plant ())
  ((animal 2 add1) 10)
  ((cat 2 add1 42) 11)
  ((a-cat 2 add1 42) 11)
  ((b-cat 2 add1 42 number->string) 11)
  ((dog 2 add1 (lambda ([s : String])  : (Listof String)
                 (list s)))
   "1 1")
  ((super-dog 2 add1 (lambda ([s : String])  : (Listof String)
                       (list s)))
   "1 1")

  ((fruit 2 number->string) 10)
  ((apple 2 number->string 42) 11)

  ((plant 31) "hello world")
  ((tree 42) "hello world"))

(module bbb-mod typed/racket/base
  (require (submod ".." aaa-mod))
  (provide (all-defined-out))
  ;; test imported base structs
  (struct shark animal ([c : Number]))
  ((shark 2 add1 42) 11)
  (struct c-cat cat ([d : (-> Symbol String)]) #:property prop:procedure (struct-field-index d))
  ((c-cat 2 add1 42 symbol->string) 'aabbcc))


;; test imported bindings
(require 'aaa-mod)
(require 'bbb-mod)
((animal 2 add1) 10)
((cat 2 add1 42) 11)
((a-cat 2 add1 42) 11)
((b-cat 2 add1 42 number->string) 11)
((dog 2 add1 (lambda ([s : String])  : (Listof String)
               (list s)))
 "1 1")
((super-dog 2 add1 (lambda ([s : String])  : (Listof String)
                     (list s)))
 "1 1")
((shark 2 add1 42) 11)

((fruit 2 number->string) 10)
((apple 2 number->string 42) 11)

((plant 31) "hello world")
((tree 42) "hello world")
