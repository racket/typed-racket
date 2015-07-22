#lang racket/base

(require racket/contract/base
         racket/logging)

;; re-exported from racket/logging, for backwards compatibility
(provide with-intercepted-logging
         with-logging-to-port)

;; helper used below
(define (receiver-thread receiver stop-chan intercept)
  (thread
   (lambda ()
     (define (clear-events)
       (let ([l (sync/timeout 0 receiver)])
         (when l ; still something to read
           (intercept l) ; interceptor gets the whole vector
           (clear-events))))
     (let loop ()
       (let ([l (sync receiver stop-chan)])
         (cond [(eq? l 'stop)
                ;; we received all the events we were supposed
                ;; to get, read them all (w/o waiting), then
                ;; stop
                (clear-events)]
               [else ; keep going
                (intercept l)
                (loop)]))))))

(struct listener (stop-chan
                  ;; ugly, but the thread and the listener need to know each
                  ;; other
                  [thread #:mutable]
                  [rev-messages #:mutable]
                  [done? #:mutable]))

;; [level] -> listener
(define (start-recording . log-spec)
  (let* ([receiver     (apply make-log-receiver (current-logger) log-spec)]
         [stop-chan    (make-channel)]
         [cur-listener (listener stop-chan #f '() #f)]
         [t (receiver-thread
             receiver stop-chan
             (lambda (l)
               (set-listener-rev-messages!
                cur-listener
                (cons l (listener-rev-messages cur-listener)))))])
    (set-listener-thread! cur-listener t)
    cur-listener))

;; listener -> listof messages
(define (stop-recording cur-listener)
  (unless (listener-done? cur-listener)
    (channel-put (listener-stop-chan cur-listener)
                 'stop) ; stop the receiver thread
    (thread-wait (listener-thread cur-listener))
    (set-listener-done?! cur-listener #t))
  (reverse (listener-rev-messages cur-listener)))

(provide/contract
 [start-recording (->* () #:rest (listof (or/c symbol? #f)) listener?)]
 [stop-recording  (-> listener? (listof log-message/c))])
