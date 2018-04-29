#lang typed/racket/shallow

;; File should compile without unbound identifier errors
;;
;; Example of a bad error:
;;   g486: undefined;
;;    cannot reference an identifier before its definition
;;     module: "/Users/ben/code/racket/fork/extra-pkgs/typed-racket/typed-racket-test/shallow/pass/dungeon-main.rkt"
;;     internal name: g486.1

;; -----------------------------------------------------------------------------

(require
  typed/racket/class
  racket/match
)

(require/typed racket/set
  (set-intersect (All (A) (-> (Listof A) (Listof A) (Listof A))))
)

(define-type CCTable (HashTable Char (U Door% Cell%)))
(define-type Cell%
  (Class
    (init-field
     (items (Listof Any) #:optional)
     (occupant (U #f (Instance Cell%)) #:optional))
    (free? (-> Boolean))
    (open (-> Void))
    (show (-> Char))
    (close (-> Void))))
(define-type Door% Cell%)
(define-type Pos (Vector Index Index))
(define-type Grid (Vectorof (Vectorof (Instance Cell%))))
(define-type Poss->Cells (Listof (Pairof Pos Cell%)))
(define-type Direction (->* (Pos) (Index) Pos))
(define-type Cache (HashTable Pos Boolean))
(define-type ExtPoints (Listof (Pairof Pos Room)))
(struct room
  ((height : Index)
   (width : Index)
   (poss->cells : Poss->Cells) ; maps positions to cell constructors
   ;;            (so that we can construct the room later when we commit to it)
   (free-cells : (Listof Pos))   ; where monsters or treasure could go
   (extension-points : (Listof Pos)))) ; where a corridor could sprout
(define-type Room room)



(: dict-set (-> Poss->Cells Pos Cell% Poss->Cells))
(define (dict-set pc p c)
  (define ok : (Boxof Boolean) (box #f))
  (for/list : Poss->Cells
            ([x (in-list pc)])
    (if (and (not (unbox ok)) (equal? (car x) p))
      (begin (set-box! ok #t) (cons p c))
      x)))

(: array-set! (-> Grid Pos (Instance Cell%) Void))
(define (array-set! g p v)
  (vector-set! (vector-ref g (vector-ref p 0)) (vector-ref p 1) v))

(: build-array (-> Pos (-> Pos (Instance Cell%)) Grid))
(define (build-array p f)
  (for/vector : Grid
    ([x (in-range (vector-ref p 0))])
   (for/vector : (Vectorof (Instance Cell%))
                ([y (in-range (vector-ref p 1))])
    (f (vector (assert x index?) (assert y index?))))))
  ;(build-array p f)))

;; a Grid is a math/array Mutable-Array of cell%
;; (mutability is required for dungeon generation)

;; parses a list of strings into a grid, based on the printed representation
;; of each cell
(: parse-grid (-> (Listof String) Grid))
(define (parse-grid los)
  (for/vector : Grid
              ; #:shape (vector (length los)
              ;                (apply max (map string-length los)))
              ;#:fill (new void-cell%)
              ([s (in-list los)])
            (for/vector : (Vectorof (Instance Cell%))
               ([c (in-string s)]) ;: (Instance Cell%)
     (new (char->cell% c)))))

(: show-grid (-> Grid String))
(define (show-grid g)
  (with-output-to-string
    (lambda ()
      (for ([r (in-vector g)])
        (for ([c (in-vector r)])
          (display (send c show)))
        (newline)))))

(: grid-height (-> Grid Index))
(define (grid-height g)
  (vector-length g))
  ;(match-define (vector rows cols) (array-shape g))
  ;rows)

(: grid-width (-> Grid Index))
(define (grid-width g)
  (vector-length (vector-ref g 0)))
  ;(match-define (vector rows cols) (array-shape g))
  ;cols)

(: within-grid? (-> Grid Pos Boolean))
(define (within-grid? g pos)
  (and (<= 0 (vector-ref pos 0) (sub1 (grid-height g)))
       (<= 0 (vector-ref pos 1) (sub1 (grid-width  g)))))

(: grid-ref (-> Grid Pos (U #f (Instance Cell%))))
(define (grid-ref g pos)
  (and (within-grid? g pos)
       (vector-ref (vector-ref g (vector-ref pos 0)) (vector-ref pos 1))))

(: left (->* (Pos) (Index) Pos))
(define (left pos [n 1])
  (vector (vector-ref pos 0)
          (assert (- (vector-ref pos 1) n) index?)))

(: right (->* (Pos) (Index) Pos))
(define (right pos [n 1])
  (vector (vector-ref pos 0)
          (assert (+ (vector-ref pos 1) n) index?)))

(: up (->* (Pos) (Index) Pos))
(define (up pos [n 1])
  (vector (assert (- (vector-ref pos 0) n) index?)
          (vector-ref pos 1)))

(: down (->* (Pos) (Index) Pos))
(define (down pos [n 1])
  (vector (assert (+ (vector-ref pos 0) n) index?)
          (vector-ref pos 1)))

(define orig : (Listof Natural) '(2 10 24 3 0 2 10 45 2 2 2 2 49 3 1 5 1 0 0 2 1 0 2 1 0 0 2 2 5 0 0 0 3 0 1 2 0 3 0 0 2 2 0 2 2 0 0 3 0 0 2 0 3 1 0 2 0 0 1 1 0 2 0 0 3 0 0 1 2 0 3 1 0 2 0 0 0 1 3 1 1 0 1 2 0 3 2 0 1 2 0 1 1 0 2 2 0 1 1 0 2 2 0 0 0 2 1 0 0 0 0 3 4 0 0 2 1 0 2 1 0 3 1 0 1 0 0 1 0 0 1 2 0 1 0 0 2 2 0 2 2 0 3 1 0 1 0 0 1 1 0 2 1 0 3 2 0 3 0 0 2 2 0 0 0 3 4 2 0 3 0 0 3 1 0 0 3 0 4 0 0 2 0 0 2 2 0 2 1 0 0 0 3 6 1 0 3 0 0 0 2 1 3 0 0 3 1 0 1 1 0 2 0 0 3 2 0 2 1 0 1 2 0 0 3 0 2 2 0 2 2 0 2 2 0 1 1 0 3 1 0 2 1 0 1 2 0 0 2 0 3 1 0 1 1 0 2 2 0 2 2 0 1 5 3 3 2 1))
(define r* : (Boxof (Listof Natural)) (box orig))

(: reset! (-> Void))
(define (reset!)
  (set-box! r* orig))

(: random (-> Integer Natural))
(define (random n)
  (begin0 (car (unbox r*)) (set-box! r* (cdr (unbox r*)))))

(: article (->* (Boolean Boolean) (#:an? Boolean) String))
(define (article capitalize? specific?
                 #:an? [an? #f])
  (if specific?
      (if capitalize? "The" "the")
      (if an?
          (if capitalize? "An" "an")
          (if capitalize? "A"  "a"))))

(: random-between (-> Integer Integer Integer))
(define (random-between min max) ;; TODO replace with 6.4's `random`
  (+ min (random (- max min))))

(: d6 (-> Integer))
(define (d6)
  (random-between 1 7))

(: d20 (-> Integer))
(define (d20)
  (random-between 1 21))

(: random-from (All (A) (-> (Listof A) A)))
(define (random-from l)
  (first (shuffle l)))

(: shuffle (All (A) (-> (Listof A) (Listof A))))
(define (shuffle l)
  (reverse l))

;; =============================================================================
;; ---
(define enqueue-message! void)
(: dict-ref (-> CCTable Char Cell%))
(define (dict-ref tbl c)
  (or (hash-ref tbl c #f) (raise-user-error 'dict-ref)))
(: dict-set! (-> CCTable Char Cell% Void))
(define (dict-set! cc k v)
  (hash-set! cc k v))

;; =============================================================================

;; maps printed representations to cell classes
;; for map parsing
(: chars->cell%s CCTable)
(define chars->cell%s
  (make-hash))

(: register-cell-type! (-> Cell% Char Void))
(define (register-cell-type! c% char)
  (dict-set! chars->cell%s char c%))

(: char->cell% (-> Char Cell%))
(define (char->cell% char)
  (dict-ref chars->cell%s char))

(: cell% Cell%)
(define cell% ; some kind of obstacle by default
  (class object%
    (init-field [items    '()]
                [occupant #f]) ; player, monster, etc.
    (define/public (free?)
      #f)
    (define/public (show)
      #\*) ; for debugging
    (define/public (open)
      (enqueue-message! "Can't open that."))
    (define/public (close)
      (enqueue-message! "Can't close that."))
    (super-new)))
(register-cell-type! cell% #\*)

(: empty-cell% Cell%)
(define empty-cell%
  (class cell%
    (inherit-field occupant)
    (define/override (free?)
      (not occupant))
    (define/override (show)
      (if occupant
          (send (or occupant (raise-user-error 'show)) show)
          #\space))
    (super-new)))
(register-cell-type! empty-cell% #\space)

(: void-cell% Cell%)
(define void-cell%
  (class cell%
    (define/override (show) #\.) ; for testing only
    (super-new)))
(register-cell-type! void-cell% #\.)

(: wall% Cell%)
(define wall%
  (class cell%
    (define/override (show) #\X) ; for testing only
    (super-new)))
(register-cell-type! wall% #\X)

(define double-bar? #t)
(define-syntax-rule (define-wall name single-bar double-bar)
  (begin (define name : Cell%
           (class wall%
             (define/override (show) (if double-bar? double-bar single-bar))
             (super-new)))
         ;; parse either kind
         (register-cell-type! name single-bar)
         (register-cell-type! name double-bar)
         (provide name)))
(define-wall pillar%           #\+     #\#)
(define-wall vertical-wall%    #\u2502 #\u2551)
(define-wall horizontal-wall%  #\u2500 #\u2550)
(define-wall four-corner-wall% #\u253c #\u256c)
(define-wall north-east-wall%  #\u2510 #\u2557)
(define-wall north-west-wall%  #\u250c #\u2554)
(define-wall south-east-wall%  #\u2518 #\u255d)
(define-wall south-west-wall%  #\u2514 #\u255a)
(define-wall north-tee-wall%   #\u252c #\u2566)
(define-wall south-tee-wall%   #\u2534 #\u2569)
(define-wall east-tee-wall%    #\u2524 #\u2563)
(define-wall west-tee-wall%    #\u251c #\u2560)

(: door% Door%)
(define door%
  (class cell%
    ;(init-field [open? #f])
    (inherit-field occupant)
    (define/override (free?)
      (and #;open? (not occupant)))
    (define/override (open)
      (if #t ;open?
          (enqueue-message! "The door is already open.")
          (void) #;(set! open? #t)))
    (define/override (close)
      (if #t ;open?
          (void) #;(set! open? #f)
          (enqueue-message! "The door is already closed.")))
    (super-new)))
(: vertical-door% Door%)
(define vertical-door%
  (class door%
    (inherit-field #;open? occupant)
    (define/override (show)
      (if #t ;open?
          (if occupant (send (or occupant (raise-user-error 'vdoor)) show) #\_)
          #\|))
    (super-new)))
(register-cell-type! vertical-door% #\|)
(register-cell-type! (class vertical-door% (super-new #;[open? #t])) #\_)
(: horizontal-door% Door%)
(define horizontal-door%
  (class door%
    (inherit-field #;open? occupant)
    (define/override (show)
      (if #t ;open?
          (if occupant (send (or occupant (raise-user-error 'hdoor)) show) #\')
          #\-))
    (super-new)))
(register-cell-type! horizontal-door% #\-)
(register-cell-type! (class horizontal-door% (super-new #;[open? #t])) #\')
;; ---

;; -----------------------------------------------------------------------------

(define N 1)

(: wall-cache Cache)
(define wall-cache (make-hash))

(: free-cache Cache)
(define free-cache (make-hash))
(define animate-generation? #f) ; to see intermediate steps
(define ITERS : Index 10)
(define dungeon-height : Index 18) ; to be easy to display in 80x24, with other stuff
(define dungeon-width  : Index 60)

;; -----------------------------------------------------------------------------

(: try-add-rectangle (-> Grid Pos Index Index Direction (U #f Room)))
(define (try-add-rectangle grid pos height width direction)
  ;; height and width include a wall of one cell wide on each side
  (match-define (vector x y) pos)
  (define min-x (match direction
                  [(== down) x]
                  ;; expanding north, we have to move the top of the room
                  ;; up so the bottom reaches the starting point
                  [(== up) (+ (- x height) 1)]
                  ;; have the entrance be at a random position on the
                  ;; entrance-side wall
                  [else    (sub1 (- x (random (- height 2))))]))
  (define min-y (match direction
                  ;; same idea as for x
                  [(== right) y]
                  [(== left)  (+ (- y width) 1)]
                  [else       (sub1 (- y (random (- width 2))))]))
  (define max-x (+ min-x height))
  (define max-y (+ min-y width))
  (define-values (success? poss->cells free-cells extension-points)
    (for*/fold : (Values Boolean Poss->Cells (Listof Pos) (Listof Pos))
               ([success?         : Boolean #t]
                [poss->cells      : Poss->Cells '()]
                [free-cells       : (Listof Pos) '()]
                [extension-points : (Listof Pos) '()])
        ([x : Integer (in-range min-x max-x)]
         [y : Integer (in-range min-y max-y)])
      ;#:break (not success?)
      (cond
       [(not success?)
        (values success? poss->cells free-cells extension-points)]
       [success?
        (define c (and (index? x) (index? y) (grid-ref grid (vector x y))))
        (cond [(and c ; not out of bounds
                    (or (is-a? c void-cell%) ; unused yet
                        (is-a? c wall%)))    ; neighboring room, can abut
               (define p : Pos (vector (assert x index?) (assert y index?)))
               ;; tentatively add stuff
               (define x-wall? (or (= x min-x) (= x (sub1 max-x))))
               (define y-wall? (or (= y min-y) (= y (sub1 max-y))))
               (if (or x-wall? y-wall?)
                   ;; add a wall
                   (values #t ; still succeeding
                           (dict-set poss->cells p wall%)
                           free-cells
                           (if (and x-wall? y-wall?)
                               ;; don't extend from corners
                               extension-points
                               (cons p extension-points)))
                   (values #t
                           (dict-set poss->cells p empty-cell%)
                           (cons p free-cells)
                           extension-points))]
              [else ; hit something, give up
               (values #f '() '() '() )])])))
  (and success?
       (room height width poss->cells free-cells extension-points)))

