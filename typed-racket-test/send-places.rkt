#lang racket

(require "places.rkt")

(require racket/place data/queue racket/async-channel)
(provide generate-log start-workers run-in-other-place places verbose? compile-path timeout)

(define places (make-parameter (and (place-enabled?) (min 8 (processor-count)))))
(define timeout (make-parameter 120))

(define-values (enq-ch deq-ch) (place-channel))
(define (start-workers)
  (define tm (timeout))
  (when (places)
    (define ws (for/list ([i (places)])
                 (start-worker deq-ch i)))
    (place/context me
      (let loop ([tests (set)])
        (define val (apply sync/timeout tm ws))
        (cond
          [val (define-values (path n _) (split-path val))
               (match n
                 [(and (? path?) (app path->string (or "slow-parser.rkt" "slow-check.rkt")))
                  (loop tests)]
                 [_ (loop (if (set-member? tests val)
                              (set-remove tests val)
                              (set-add tests val)))])]
          [else (unless (set-empty? tests)
                  ;;  the trailing ~n in the format string forces the standard
                  ;;  error to flush out. This helps when only 1 test is still
                  ;;  running.
                  (eprintf "~a tests are still running:~n~a~n"
                           (set-count tests)
                           (string-join (set-map tests
                                                 path->string)
                                        "\n")))
                (loop tests)])))))

(define (run-in-other-place p* error?)
  (define-values (res-ch res-ch*) (place-channel))
  (place-channel-put enq-ch (vector p* res-ch* error?))
  (define res (place-channel-get res-ch))
  (when (s-exn? res)
    (raise (deserialize-exn res))))


(define (generate-log name dir)
  (apply values
    (cond [(places)
           (define-values (res-ch res-ch*) (place-channel))
           (place-channel-put enq-ch (vector 'log name dir res-ch*))
           (define res (place-channel-get res-ch))
           (if (s-exn? res)
               (raise (deserialize-exn res))
               res)]
          [else
           (generate-log/place name dir)])))

(define (compile-path file)
  (cond [(places)
         (define-values (res-ch res-ch*) (place-channel))
         (place-channel-put enq-ch (vector 'compile file res-ch*))
         (define res (place-channel-get res-ch))
         (if (s-exn? res)
             (raise (deserialize-exn res))
             res)]
        [else
         (compile-path/place file)]))
