#lang typed/racket/shallow
;; TODO why fail?

(require typed/framework typed/racket/draw)

;; bitmap% methods
(: bitmap-test (-> (Instance Bitmap%) Void))
(define (bitmap-test bm) ;; yes shape-check
  (send bm get-width)
  (send bm get-backing-scale)
  (send bm ok?)
  (void))

;; do-autosave
(: autosave (-> (Instance Autosave:Autosavable<%>) Void))
(define (autosave aa) ;; yes shape-check
  (send aa do-autosave)
  (void))

;; get-spell-check-strings
(: colortext (-> (Instance Color:Text<%>) Void))
(define (colortext cc) ;; yes shape-check
  (send cc get-spell-check-strings)
  (void))

(let ()
  color:misspelled-text-color-style-name
  (void))

(let ()
  (void (application:current-app-name)) ;; yes shape-check
  (application:current-app-name "test")
  (void))

(let ()
  (preferences:default-set? 'xxx)
  (void))

(let ()
  (get-current-gl-context)
  (get-face-list)
  (make-bitmap 4 4)
  (make-platform-bitmap 8 8)
  (recorded-datum->procedure '())
  (void))

