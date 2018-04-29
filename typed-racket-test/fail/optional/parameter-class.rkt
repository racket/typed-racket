#;
(exn-pred exn:fail:contract? #rx"pdf-dc")

#lang typed/racket/optional
(module p racket/base
  (define world:paper-height (make-parameter 792.0+0.0i))
  (provide world:paper-height))
(require/typed 'p (world:paper-height (Parameterof Float)))
(require (only-in typed/racket/draw pdf-dc%))
(define dc-output-port (open-output-bytes))
(define dc
  (new pdf-dc%
       [interactive #f][use-paper-bbox #f][as-eps #f]
       [output dc-output-port]
       [width 792.0]
       [height (world:paper-height)]))
