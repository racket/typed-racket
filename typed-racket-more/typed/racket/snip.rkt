#lang s-exp typed-racket/base-env/extra-env-lang

;; This module provides a base type environment including
;; racket/snip bindings

(require racket/snip/private/snip
         racket/snip/private/snip-admin
         racket/snip/private/style
         "private/gui-types.rkt"
         (for-syntax (submod "private/gui-types.rkt" #%type-decl)))

(provide Image-Kind
         Add-Color<%>
         Image-Snip%
         Mult-Color<%>
         Readable-Snip<%>
         Snip%
         Snip-Admin%
         Snip-Class%
         Snip-Class-List<%>
         String-Snip%
         Style<%>
         Style-Delta%
         Style-List%
         Tab-Snip%)

#;(begin-for-syntax
    (define -Image-Kind (parse-type #'Image-Kind)))

(type-environment
 [add-color<%>       (parse-type #'Add-Color<%>)]
 [image-snip%        (parse-type #'Image-Snip%)]
 [mult-color<%>      (parse-type #'Mult-Color<%>)]
 [readable-snip<%>   (parse-type #'Readable-Snip<%>)]
 [snip%              (parse-type #'Snip%)]
 [snip-admin%        (parse-type #'Snip-Admin%)]
 [snip-class%        (parse-type #'Snip-Class%)]
 [snip-class-list<%> (parse-type #'Snip-Class-List<%>)]
 [string-snip%       (parse-type #'String-Snip%)]
 [style<%>           (parse-type #'Style<%>)]
 [style-delta%       (parse-type #'Style-Delta%)]
 [style-list%        (parse-type #'Style-List%)]
 [tab-snip%          (parse-type #'Tab-Snip%)])
