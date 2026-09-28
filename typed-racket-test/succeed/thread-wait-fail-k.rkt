#lang typed/racket

;; `thread-wait` takes an optional thunk that produces its results when
;; the thread did not finish normally. Its result is any number of
;; values, since a thread may keep its own results.
(define t (thread (λ () (sleep 1000))))
(kill-thread t)
(unless (equal? (call-with-values (λ () (thread-wait t (λ () 'failed)))
                                  (λ [rs : Any *] rs))
                '(failed))
  (error 'thread-wait "expected the failure thunk's result"))
(thread-wait (thread void))
