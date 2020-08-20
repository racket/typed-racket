#lang typed/racket

(: tag (Prompt-Tagof Integer (Integer -> Integer)))
(define tag (make-continuation-prompt-tag))

(define cc : (Option (-> Integer Nothing)) #f)

(call-with-continuation-prompt
 (λ ()
   (+ 1
      (call-with-current-continuation
       (λ ([k : (Integer -> Nothing)])
         (set! cc k)
         (k 1))
       tag)))
 tag
 (λ ([x : Integer]) (+ 1 x)))

(let ([k cc])
  (when k
    (call-with-continuation-prompt
     (λ ()
       (k 0))
     tag
     (λ ([x : Integer]) (+ 1 x)))))