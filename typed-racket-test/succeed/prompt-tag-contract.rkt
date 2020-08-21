#lang typed/racket/base
(provide pt)

(define-type StringPromptTag (Prompt-Tagof String (-> (-> String) String)))

(: pt StringPromptTag)
(define pt (make-continuation-prompt-tag))

(module t racket/base
  (provide (all-defined-out))
  (define (call-cc tag v)
    ((call-with-continuation-prompt
      (λ ()
        (call/cc values tag))
      tag)
     v))
  (define (abort-cc tag v)
    (abort-current-continuation tag (λ () v))))

(require/typed 't
               [call-cc (StringPromptTag Any -> Nothing)]
               [abort-cc (StringPromptTag Any -> Nothing)])

(require typed/rackunit)

(check-exn exn:fail:contract?
 (λ () (call-with-continuation-prompt
        (λ () (call-cc pt 'bad))
        pt)))
(check-exn exn:fail:contract?
 (λ () (call-with-continuation-prompt
        (λ () (abort-cc pt 'bad))
        pt)))

(check-not-exn
 (λ () (call-with-continuation-prompt
        (λ () (call-cc pt "good"))
        pt)))
(check-not-exn
 (λ () (call-with-continuation-prompt
        (λ () (abort-cc pt "good"))
        pt)))