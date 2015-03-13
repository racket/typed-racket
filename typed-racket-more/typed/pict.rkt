#lang s-exp typed-racket/base-env/extra-env-lang

;; Typed base-env wrapper for the pict library

(require pict
         "racket/private/gui-types.rkt"
         (for-syntax (only-in typed-racket/rep/type-rep
                              make-Name
                              make-Union)
                     (submod "racket/private/gui-types.rkt" #%type-decl)))

(begin-for-syntax
 (define (-improper-listof t)
   (-mu -ilof
        (Un (-pair t -ilof) t -Null)))
 (define -dc
   (-inst (parse-type #'DC<%>)))
 (define -color
   (-inst (parse-type #'Color%)))
 (define -pict (-struct-name #'pict))
 (define -pict-path
   (make-Union (list (-val #f) -pict (-lst -pict))))
 (define -child (-struct-name #'child))
 (define -text-style
   (-mu -text-style
        (Un -Null (-inst (parse-type #'Font%)) (parse-type #'Font-Family) -String
            (-pair -String (parse-type #'Font-Family))
            (-pair (Un (-val 'bold) (-val 'italic) (-val 'subscript) (-val 'superscript) (-val 'caps)
                       (-val 'combine) (-val 'no-combine) (-val 'aligned) (-val 'unaligned)
                       -color)
                   -text-style))))
 (define -linestyle
   (one-of/c 'transparent 'solid 'xor 'hilite
             'dot 'long-dash 'short-dash 'dot-dash
             'xor-dot 'xor-long-dash 'xor-short-dash
             'xor-dot-dash))
 (define -pin-arrow-line
   (->key -Real
          -pict
          -pict-path
          (-> -pict -pict-path (-values (list -Real -Real)))
          -pict-path
          (-> -pict -pict-path (-values (list -Real -Real)))
          #:start-angle (-opt -Real) #f
          #:end-angle (-opt -Real) #f
          #:start-pull -Real #f
          #:end-pull -Real #f
          #:line-width (-opt -Real) #f
          #:color (-opt (Un -String -color)) #f
          #:alpha -Real #f
          #:style -linestyle #f
          #:under? Univ #f
          #:solid? Univ #f
          #:hide-arrowhead? Univ #f
          -pict))
 (define -pict-finder
   (-> -pict -pict-path (-values (list -Real -Real))))
 (define -append-type
   (cl->*
     (->* (list -Real -pict) -pict -pict)
     (->* (list -pict) -pict -pict)))
 (define -superimpose-type
   (->* (list -pict) -pict -pict)))

(type-environment
 ; 1 Pict Datatype
 [#:struct pict ([draw : Univ]
                 [width : -Real]
                 [height : -Real]
                 [ascent : -Real]
                 [children : (-lst -child)]
                 [panbox : Univ]
                 [last : -pict-path])
           #:extra-constructor-name make-pict]
 [#:struct child ([pict : -pict]
                  [dx : -Real]
                  [dy : -Real]
                  [sx : -Real]
                  [sy : -Real]
                  [sxy : -Real]
                  [syx : -Real])
           #:extra-constructor-name make-child]
 ; 2 Basic Pict Constructors
 [dc (->opt (-> -dc -Real -Real ManyUniv) -Real -Real [-Real -Real] -pict)]
 [blank (cl->* (-> -pict)
               (-> -Real -pict)
               (-> -Real -Real -pict)
               (-> -Real -Real -Real -pict)
               (-> -Real -Real -Real -Real -pict))]
 [text (->opt -String [-text-style -Index -Real] -pict)]
 [hline (->key -Real -Real #:segment (-opt -Real) #f -pict)]
 [vline (->key -Real -Real #:segment (-opt -Real) #f -pict)]
 [frame (->key -pict
               #:segment (-opt -Real) #f
               #:color (-opt (Un -String -color)) #f
               #:line-width (-opt -Real) #f
               -pict)]
 [ellipse (-> -Real -Real -pict)]
 [circle (-> -Real -pict)]
 [filled-ellipse
  (->key -Real -Real #:draw-border? Univ #f -pict)]
 [disk
  (->key -Real #:draw-border? Univ #f -pict)]
 [rectangle (-> -Real -Real -pict)]
 [filled-rectangle
  (->key -Real -Real #:draw-border? Univ #f -pict)]
 [rounded-rectangle
  (->optkey -Real -Real [-Real] #:angle -Real #f -pict)]
 [filled-rounded-rectangle
  (->optkey -Real -Real [-Real]
            #:angle -Real #f
            #:draw-border? Univ #f
            -pict)]
 ;; FIXME: add image-snip%
 [bitmap (-> (Un -Pathlike (-inst (parse-type #'Bitmap%))) -pict)]
 [arrow (-> -Real -Real -pict)]
 [arrowhead (-> -Real -Real -pict)]
 [pip-line (-> -Real -Real -Real -pict)]
 [pip-arrow-line (-> -Real -Real -Real -pict)]
 [pip-arrows-line (-> -Real -Real -Real -pict)]
 [pin-line
  (->key -pict
         -pict-path
         (-> -pict -pict-path (-values (list -Real -Real)))
         -pict-path
         (-> -pict -pict-path (-values (list -Real -Real)))
         #:start-angle (-opt -Real) #f
         #:end-angle (-opt -Real) #f
         #:start-pull -Real #f
         #:end-pull -Real #f
         #:line-width (-opt -Real) #f
         #:color (-opt (Un -String -color)) #f
         #:alpha -Real #f
         #:style -linestyle #f
         #:under? Univ #f
         -pict)]
 [pin-arrow-line -pin-arrow-line]
 [pin-arrows-line -pin-arrow-line]
 [bitmap-draft-mode (-Param Univ -Boolean)]

 ;; 3 Pict Combiners
 [vl-append -append-type]
 [vc-append -append-type]
 [vr-append -append-type]
 [ht-append -append-type]
 [htl-append -append-type]
 [hc-append -append-type]
 [hbl-append -append-type]
 [hb-append -append-type]

 [lt-superimpose -superimpose-type]
 [ltl-superimpose -superimpose-type]
 [lc-superimpose -superimpose-type]
 [lbl-superimpose -superimpose-type]
 [lb-superimpose -superimpose-type]
 [ct-superimpose -superimpose-type]
 [ctl-superimpose -superimpose-type]
 [cc-superimpose -superimpose-type]
 [cbl-superimpose -superimpose-type]
 [cb-superimpose -superimpose-type]
 [rt-superimpose -superimpose-type]
 [rtl-superimpose -superimpose-type]
 [rc-superimpose -superimpose-type]
 [rbl-superimpose -superimpose-type]
 [rb-superimpose -superimpose-type]

 [pin-over
  (cl->*
    (-> -pict -Real -Real -pict -pict)
    (-> -pict -pict-path
        (-> -pict -pict-path (-values (list -Real -Real)))
        -pict
        -pict))]
 [pin-under
  (cl->*
    (-> -pict -Real -Real -pict -pict)
    (-> -pict -pict
        (-> -pict -pict (-values (list -Real -Real)))
        -pict
        -pict))]

 [table
  (-> -PosInt
      (-pair -pict (-lst -pict))
      (-improper-listof (-> -pict -pict -pict))
      (-improper-listof (-> -pict -pict -pict))
      (-improper-listof -Real)
      (-improper-listof -Real)
      -pict)]

 ;; 4 Pict Drawing Adjusters
 [scale
  (cl->* (-> -pict -Real -pict)
         (-> -pict -Real -Real -pict))]
 [scale-to-fit
  (cl->* (-> -pict -Real -pict)
               (-> -pict -Real -Real -pict))]
 [rotate (-> -pict -Real -pict)]
 [ghost (-> -pict -pict)]
 [linewidth (-> (-opt -Real) -pict -pict)]
 [linestyle (-> -linestyle -pict -pict)]
 [colorize (-> -pict (Un -String (-lst* -Byte -Byte -Byte) -color) -pict)]
 [cellophane (-> -pict -Real -pict)]
 [clip (-> -pict -pict)]
 [inset/clip
  (cl->* (-> -pict -Real -pict)
         (-> -pict -Real -Real -pict)
         (-> -pict -Real -Real -Real -Real -pict))]
 [black-and-white (-Param Univ -Boolean)]

 ;; 5 Bounding Box Adjusters
 [inset
  (cl->* (-> -pict -Real -pict)
         (-> -pict -Real -Real -pict)
         (-> -pict -Real -Real -Real -Real -pict))]
 [clip-descent (-> -pict -pict)]
 [lift-above-baseline (-> -pict -Real -pict)]
 [drop-below-ascent (-> -pict -Real -pict)]
 [baseless (-> -pict -pict)]
 [refocus (-> -pict -pict -pict)]
 [panorama (-> -pict -pict)]
 [use-last (-> -pict -pict-path -pict)]
 [use-last* (-> -pict -pict-path -pict)]

 ;; 6 Pict Finders
 [lt-find -pict-finder]
 [ltl-find -pict-finder]
 [lc-find -pict-finder]
 [lbl-find -pict-finder]
 [lb-find -pict-finder]
 [ct-find -pict-finder]
 [ctl-find -pict-finder]
 [cbl-find -pict-finder]
 [cb-find -pict-finder]
 [rt-find -pict-finder]
 [rtl-find -pict-finder]
 [rc-find -pict-finder]
 [rbl-find -pict-finder]
 [rb-find -pict-finder]
 [pict-path? (make-pred-ty -pict-path)]
 [launder (-> -pict -pict)]

 ;; 7.1 Dingbats
 [cloud (->opt -Real -Real [(Un -String -color)] -pict)]
 [file-icon (->opt -Real -Real Univ [Univ] -pict)]
 [standard-fish
  (->key -Real -Real
         #:direction (one-of/c 'left 'right) #f
         #:color (Un -String -color) #f
         #:eye-color (-opt -String) #f
         #:open-mouth (Un -Boolean -Real) #f
         -pict)]
 [jack-o-lantern (->opt -Real [-String (Un -String -color)] -pict)]
 [angel-wing (-> -Real -Real Univ -pict)]
 [desktop-machine (->opt -Real [(-lst (one-of/c 'plt 'binary 'devil))] -pict)]
 ;; thermometer

 ;; 8 Animation Helpers
 [fade-pict (->key -Real -pict -pict
                   #:combine (-> -pict -pict -pict) #f
                   -pict)]
 [fade-around-pict (-> -Real -pict (-> -pict -pict) -pict)]
 [slide-pict (-> -pict -pict -pict -pict -Real -pict)]
 [sequence-animations (->* '() (-> -Real -pict) (-> -Real -pict))]
 [reverse-animations (->* '() (-> -Real -pict) (-> -Real -pict))]
 [fast-start (-> -Real -Real)]
 [fast-end (-> -Real -Real)]
 [fast-edges (-> -Real -Real)]
 [fast-middle (-> -Real -Real)]
 [split-phase (-> -Real (-values (list -Real -Real)))]

 ;; 10 Miscellaneous
 [hyperlinkize (-> -pict -pict)]
 [scale-color (-> -Real (Un -String -color) -color)]
 [color-series (-> -dc -Nat -PosRat
                   (Un -String -color) (Un -String -color)
                   (-> -Rat ManyUniv)
                   Univ Univ
                   -Void)]
 
 ;; 11 Rendering
 [dc-for-text-size (-Param (-opt -dc) (-opt -dc))]
 [convert-bounds-padding (-Param (-lst* -PosReal -PosReal -PosReal -PosReal)
                                 (-lst* -PosReal -PosReal -PosReal -PosReal))]
 [draw-pict (-> -pict -dc -Real -Real -Void)]
 [pict->bitmap (->opt -pict [(Un (-val 'unsmoothed) (-val 'smoothed) (-val 'aligned))]
                      (-inst (parse-type #'Bitmap%)))]
 [pict->argb-pixels (->opt -pict [(Un (-val 'unsmoothed) (-val 'smoothed) (-val 'aligned))]
                      -Bytes)]
 [argb-pixels->pict (-> -Bytes -Nat -pict)]
 [make-pict-drawer (-> -pict (-> -dc -Real -Real -Void))]
 [show-pict (->optkey -pict [(-opt -Nat) (-opt -Nat)]
                      #:frame-x -Integer #t #:frame-y -Integer #t
                      #:frame-style (-lst (Un (-val 'no-resize-border) (-val 'no-caption)
                                              (-val 'no-system-menu) (-val 'hide-menu-bar)
                                              (-val 'toolbar-button) (-val 'float)
                                              (-val 'metal))) #t
                      -Void)]
 [current-expected-text-scale (-Param (-lst* -Real -Real) (-lst* -Real -Real))]
 )
