#lang racket

(module automata racket

  (provide
   make-random-automaton)
  
  (define (make-random-automaton)
    (new automaton%))
  
  (define automaton%
    (class object%
      (super-new)
      (define/public (match-pair other) other)
      (define/public (guts) #f))))

(module typed typed/racket
  (require/typed (submod ".." automata)
                 (make-random-automaton
                  (-> (Instance Automaton))))
  
  (define-type Automaton
    (Class
     [match-pair
      (-> (Instance Automaton) (Instance Automaton2))]))
  
  (define-type Automaton2
    (Class
     [match-pair
      (-> (Instance Automaton) (Instance Automaton2))]
     [guts (-> #f)]))
  
  (for/fold ([a (make-random-automaton)])
            ([_ (in-range 10)])
    (send a match-pair a)))

(require 'typed)