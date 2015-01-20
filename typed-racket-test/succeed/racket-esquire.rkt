#lang typed/racket

;; Racket Esquire: a mini DrRacket

(require typed/racket/gui)

;; The REPL editor class
(define esq-text%
  (class text%
    ;; lexical access to inherited methods
    (inherit insert last-position get-text erase)
    ;; private fields
    (define prompt-pos 0)
    (define locked? #t)
    ;; augment can-insert? to block pre-prompt inserts
    (define/augment (can-insert? start len)
      (and (>= start prompt-pos)
           (not locked?)))
    ;; augment can-delete? to block pre-prompt deletes
    (define/augment (can-delete? start end)
      (and (>= start prompt-pos)
           (not locked?)))
    ;; override on-char to detect Enter/Return
    (define/override (on-char c)
      (super on-char c)
      (when (and (eq? (send c get-key-code)
                      #\return)
                 (not locked?))
        (set! locked? #t)
        (evaluate
         (get-text prompt-pos
                   (last-position)))))
    ;; method to insert a new prompt
    (: new-prompt (-> Void))
    (define/public (new-prompt)
      (queue-output
       (lambda ()
         (set! locked? #f)
         (insert "> ")
         (set! prompt-pos (last-position)))))
     ;; method to display output
    (: output (-> String Void))
    (define/public (output str)
      (queue-output
       (lambda ()
         (let ((was-locked? locked?))
           (set! locked? #f)
           (insert str)
           (set! locked? was-locked?)))))
    ;; method to reset the REPL
    (: reset (-> Void))
    (define/public (reset)
      (set! locked? #f)
      (set! prompt-pos 0)
      (erase)
      (new-prompt))

    ;; initialize superclass-defined state
    (super-new)
    ;; create the initial prompt
    (new-prompt)))

;; Queueing REPL output as an event

(define esq-eventspace (current-eventspace))

(: queue-output (-> (-> Any) Void))
(define (queue-output proc)
  (parameterize ((current-eventspace
                  esq-eventspace))
    (queue-callback proc #f)))

;; GUI creation

(define frame
  (make-object frame% "RacketEsq" #f 425 175))
(define reset-button
  (make-object button% "Reset" frame
               (lambda (b e)
                 (reset-program))))
(define repl-editor (make-object esq-text%))

(let ([s (send (send repl-editor get-style-list) find-named-style "Standard")])
  (when s
    (let ([d (make-object style-delta% 'change-size 24)])
      (send s set-delta d))))

(define repl-display-canvas
  (make-object editor-canvas% frame))
(send repl-display-canvas set-editor repl-editor)

;; Disable showing the frame for the TR tests
;(send frame show #t)

;; User space initialization

(define user-custodian (make-custodian))

(define user-output-port
  (make-output-port
   'stdout
   ;; always ready
   always-evt
   ;; string printer
   (lambda ([s : Bytes] [start : Integer] [end : Integer] nonblock? w/break?)
     (send repl-editor output (bytes->string/utf-8 (subbytes s start end)))
     (- end start))
   ;; closer
   (lambda () 'nothing-to-close)))

(define user-eventspace
  (parameterize ((current-custodian user-custodian))
    (make-eventspace)))

(define user-namespace
  (make-gui-namespace))

(define esq-inspector (current-inspector))
(define user-inspector (make-inspector))

;; Evaluation and resetting

(: evaluate (-> String Void))
(define (evaluate expr-str)
  (parameterize ((current-eventspace user-eventspace))
    (queue-callback
     (lambda ()
       (current-command-line-arguments #())
       (current-output-port user-output-port)
       (current-namespace user-namespace)
       (current-inspector user-inspector)
       (exit-handler (lambda (v) (reset-program)))
       (with-handlers ((exn?
                        (lambda ([e : exn])
                          (display
                           (exn-message e))
                          (show-where
                           (exn-continuation-marks e)))))
         (call-with-values
          (lambda ()
            (eval (instrument
                   (cast (read (open-input-string
                                expr-str))
                         Sexp))))
          (lambda args
            (parameterize ((current-inspector user-inspector))
              (write (car args))))))
       (newline)
       (send repl-editor new-prompt)))))

(: reset-program (-> Void))
(define (reset-program)
  (custodian-shutdown-all user-custodian)
  (set! user-custodian (make-custodian))
  (parameterize ((current-custodian user-custodian))
    (set! user-eventspace (make-eventspace)))
  (set! user-namespace (make-gui-namespace))
  (send repl-editor reset))

;; Definition source instrumentation

(define where-key (gensym))

(: instrument (-> Sexp Sexp))
(define (instrument expr)
  (if (and (pair? expr)
           (eq? 'define (car expr))
           (pair? (cdr expr))
           (pair? (cadr expr)))
      `(define ,(cadr expr)
         (with-continuation-mark ',where-key ',(caadr expr)
           ,@(cddr expr)))
      expr))

(: show-where (-> Continuation-Mark-Set Void))
(define (show-where cm)
  (let ([l (continuation-mark-set->list cm where-key)])
    (unless (null? l)
      (printf " in ~a" (car l)))))
