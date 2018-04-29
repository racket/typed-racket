#lang typed/racket/shallow

(require typed/pict)

(pict-width (blank)) ;; yes shape-check

(let* ((p0 (rectangle 40 40))
       (p1 (circle 40)))
  (rtl-superimpose p0 p1)
  (lc-superimpose p0 p1)
  (cc-superimpose p0 p1))

(let* ((p0 (rectangle 40 40))
       (p1 (circle 40)))
  (vl-append 1 p0 p1)
  (hc-append 1 p0 p1)
  (hbl-append 1 p0 p1)
  (hb-append 1 p0 p1))

(let* ((p0 (rectangle 40 40))
       (p1 (circle 40)))
  (call-with-values (lambda () (lt-find p0 p0)) list)
  (void))

;; -Param
(let* ()
  (bitmap-draft-mode) ;; yes shape-check
  (bitmap-draft-mode #f)
  (bitmap-draft-mode)) ;; yes shape-check

(let* ((p0 : pict (rectangle 40 40))
       (p1 : pict (circle 40))
       (pp : pict (hc-append 100 p0 p1)))
 (pin-line pp p0 rc-find p1 lc-find)
 (pin-arrows-line 10 pp p0 rc-find p1 lc-find)
 (pin-arrow-line 30
                 pp
                 p0 rc-find
                 p1 lc-find
                 #:line-width 3
                 #:style 'long-dash
                 #:color "medium goldenrod"))

