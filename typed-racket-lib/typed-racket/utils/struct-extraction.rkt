(module struct-extraction racket/base
  (provide extract-struct-info/checked)
  (require racket/struct-info)
  (define (extract-struct-info/checked id)
    (syntax-case id ()
     [id
      (and (identifier? #'id)
           (struct-info? (syntax-local-value #'id (lambda () #f))))
      (extract-struct-info (syntax-local-value #'id))]
     [_
      (raise-syntax-error
       'require/typed
       "expected an identifier bound to a structure type" id)])))
