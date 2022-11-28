#lang typed/racket
(require racket/flonum)

(fl+)
(fl+ 1.)
(fl+ 3.1 1.)
(fl+ 1. 4.99 1.)

; bad: (fl-)
(fl- 1.)
(fl- 1. 2.)
(fl- 9.22 2. 3.)

(fl*)
(fl* 9.)
(fl* 9. 0.11)
(fl* 8.21 2. 8.)

; bad: (fl/)
(fl/ 2.) ; = (fl/ 1. 2.)
(fl/ 1. 2.)
(fl/ 1. 2. 3.)
(fl/ 1. 2. 3. 4.)
(fl/ 1. 2. 3. 4. 5.)
