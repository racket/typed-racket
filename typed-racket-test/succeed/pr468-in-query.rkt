#lang typed/racket

;; https://github.com/racket/typed-racket/issues/460
;; https://github.com/racket/typed-racket/pull/468

(require typed/db)

(define sqlite : Connection (sqlite3-connect #:database 'memory))

(struct master ([id : Integer] [type : Symbol] [name : String] [sql : String])
  #:prefab #:type-name Master
  #:constructor-name make-master)

(define record : Master
  (make-master (current-milliseconds) 'table "manual_index"
               "CREATE TABLE manual_index(id, name, content);"))

(query-exec sqlite (master-sql record))
(query-exec sqlite "insert into manual_index values ($1, $2, $3)"
            (master-id record) (master-name record) (~s record))

(for ([s (in-query sqlite "SELECT content FROM manual_index WHERE name = $1;" (master-name record))])
  (define ?record (with-input-from-string (~a s) read))
  (if (not (master? ?record))
      (fprintf (current-error-port) "in-query: unexpected value: ~s"
               ?record)
      (fprintf (current-output-port) "[~a]~a: ~s~n"
               (master-type ?record)
               (master-name ?record)
               (master-sql ?record))))

(disconnect sqlite)
