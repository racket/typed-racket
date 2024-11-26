#lang typed/racket/base

(module+ test

  (require typed/rackunit
           "../../typed-racket-more/typed/xml.rkt")

  (let ([xml-p-i
         (p-i #f #f 'xml "version=\"1.0\" encoding=\"UTF-8\"")])
    (check-eq?
     (source-start xml-p-i)
     #f)
    (check-eq?
     (source-stop xml-p-i)
     #f))
               
  (let ([xml-p-i
         (p-i 'racket 'racket 'xml "version=\"1.0\" encoding=\"UTF-8\"")])
    (check-eq?
     (source-start xml-p-i)
     'racket)
    (check-eq?
     (source-stop xml-p-i)
     'racket)))
               
