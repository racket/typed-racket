#lang typed/racket

;; Test typed/pict

(require typed/pict typed/racket/draw)

(pict-children (blank 50 50))

(blank)
(blank 1)
(blank 3 4 5)
(blank 3 4 5 6)

(text "a")
(text "a" (cons 'bold 'roman))
(text "a" 'modern 14)
(text "a" 'modern 14 30)

(hline 1 5)
(hline 1 5 #:segment #f)
(vline 1 5)
(vline 1 5 #:segment 4)

(frame (circle 5) #:segment 2)
(frame (circle 5) #:color "red")
(frame (circle 5) #:line-width 4)

(ellipse 40 80)
(ellipse 40 80 #:border-color "blue")
(ellipse 40 80 #:border-width 6)

(circle 20 #:border-color "black")
(circle 20 #:border-width #f)

(filled-ellipse 100 50)
(filled-ellipse 100 50 #:draw-border? #f)
(filled-ellipse 100 50 #:color "gray")
(filled-ellipse 100 50 #:draw-border? #t #:border-color "green")
(filled-ellipse 100 50 #:draw-border? #t #:border-width 2)

(disk 60)
(disk 60 #:draw-border? #t)
(disk 60 #:color "green")
(disk 60 #:border-color (make-object color% "blue"))
(disk 60 #:draw-border? #t #:border-width 3)

(rectangle 10 100)
(rectangle 10 100 #:border-color "red")
(rectangle 10 100 #:border-width 4)

(filled-rectangle 80 80)
(filled-rectangle 80 80 #:draw-border? #f)
(filled-rectangle 80 80 #:draw-border? #t #:color "white")
(filled-rectangle 80 80 #:draw-border? #t #:border-color "white")
(filled-rectangle 80 80 #:border-width 16)

(rounded-rectangle 80 80)
(rounded-rectangle 80 80 4)
(rounded-rectangle 80 80 #:angle 4)
(rounded-rectangle 80 80 #:border-color "salmon")
(rounded-rectangle 80 80 #:border-width 3)

(filled-rounded-rectangle 80 80)
(filled-rounded-rectangle 80 80 5)
(filled-rounded-rectangle 80 80 #:draw-border? #f)
(filled-rounded-rectangle 80 80 #:color "gainsboro")
(filled-rounded-rectangle 80 80 #:border-color "yellow")
(filled-rounded-rectangle 80 80 #:border-width 11)

(define hl (hline 1 5))
(define-values (x y) (lt-find hl hl))

(standard-fish 40 20 #:direction 'left #:color "olive")

