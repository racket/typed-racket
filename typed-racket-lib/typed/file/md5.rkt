#lang typed/racket/base
(require/typed file/md5
               [md5 (case-> [(U Input-Port Bytes String) -> Bytes]
                            [(U Input-Port Bytes String) Boolean -> Bytes])])
(provide md5)
