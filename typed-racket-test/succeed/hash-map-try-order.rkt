#lang typed/racket

;; test support for the optional third `try-order?` argument
;; added to `hash-map` and `hash-for-each` in Racket 6.3

;; check HashTableTop variants work with 2 or 3 args
(λ ([hsh : HashTableTop]) : (Listof Void)
  (hash-map hsh void))
(λ ([hsh : HashTableTop]) : (Listof Void)
  (hash-map hsh void #f))
(λ ([hsh : HashTableTop]) : (Listof Void)
  (hash-map hsh void #t))
(λ ([hsh : HashTableTop])
  (hash-for-each hsh void))
(λ ([hsh : HashTableTop])
  (hash-for-each hsh void #f))
(λ ([hsh : HashTableTop])
  (hash-for-each hsh void 'try-order))


;; concrete check
(define a-hash : (HashTable Symbol Natural)
  #hash([b . 2]
        [c . 3]
        [a . 1]))

(unless (equal? '([a . 1][b . 2][c . 3])
                (hash-map a-hash
                          (inst cons Symbol Natural)
                          #t))
  (error 'hash-map "bad order"))

(unless (equal? "a1b2c3"
                (with-output-to-string
                  (λ ()
                    (hash-for-each a-hash
                                   (λ ([k : Symbol][v : Natural])
                                     (display k)
                                     (display v))
                                   "try order"))))
  (error 'hash-for-each "bad order"))



