#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(define-type Data-Source data-source)
(define-type Simple-Result simple-result)
(define-type Rows-Result rows-result)

(define-type Group-Mode (U 'preserve-null 'list))
(define-type Isolation-Level (U 'serializable 'repeatable-read 'read-committed 'read-uncommitted False))
(define-type Schema-Option (U 'search-or-current 'search 'current))

(define-type SQL-Datum (U Boolean String Real Char Bytes SQL-Null))
(define-type SQL-Type (List Boolean (Option Symbol) SQL-Datum))
(define-type Statement (U String Prepared-Statement Virtual-Statement Statement-Binding Other-Statement))

(define-type SQL-Field (U String Natural))
(define-type SQL-Grouping (U SQL-Field (Vectorof SQL-Field)))
(define-type SQL-Group (U SQL-Grouping (Listof SQL-Grouping)))
(define-type SQL-Dictionary (HashTable SQL-Field (U (Vectorof SQL-Datum) (Listof (Vectorof SQL-Datum)))))

(require/typed/provide
 db/base
 [#:opaque SQL-Null sql-null?]
 [sql-null SQL-Null]
 [sql-null->false (-> (U SQL-Null Any) Any)]
 [false->sql-null (-> Any (U SQL-Null Any))])

(require/typed/provide
 db/base
 [#:opaque Connection connection?]
 [#:opaque DBSystem dbsystem?]
 [#:opaque Connection-Pool connection-pool?]
 [#:struct data-source ([connector : Symbol]
                        [args : (Listof Any)]
                        [extensions : (Listof (List Symbol Any))])]
 [#:struct (exn:fail:sql exn:fail) ([sqlstate : (U String Symbol)]
                                    [info : (Listof (Pairof Symbol Any))])
  #:extra-constructor-name make-exn:fail:sql]
 [disconnect (-> Connection Void)]
 [connected? (-> Connection Boolean)]
 [connection-dbsystem (-> Connection DBSystem)]
 [dbsystem-name (-> DBSystem Symbol)]
 [dbsystem-supported-types (-> DBSystem (Listof Symbol))]
 [list-tables (-> Connection [#:schema Schema-Option] (Listof String))]
 [table-exists? (-> Connection String [#:schema Schema-Option] [#:case-sensitive? Any] Boolean)]
 [current-dsn-file (Parameterof Path-String)]
 [get-dsn (All (a) (->* (Symbol) ((U a (-> a)) #:dsn-file Path-String) (U Data-Source a)))]
 [put-dsn (-> Symbol (Option Data-Source) [#:dsn-file Path-String] Void)]
 [kill-safe-connection (-> Connection Connection)]
 [virtual-connection (-> (U (-> Connection) Connection-Pool) Connection)]
 [connection-pool-lease (->* (Connection-Pool) ((U (Evtof Any) Custodian)) Connection)]
 [connection-pool (-> (-> Connection)
                      [#:max-connections (U Positive-Index +inf.0)]
                      [#:max-idle-connections (U Positive-Index +inf.0)]
                      Connection-Pool)])

(require/typed/provide
 db/base
 [query-exec (-> Connection Statement SQL-Datum * Void)]
 [query-list (-> Connection Statement SQL-Datum * (Listof SQL-Datum))]
 [query-row (-> Connection Statement SQL-Datum * (Vectorof SQL-Datum))]
 [query-maybe-row (-> Connection Statement SQL-Datum * (Option (Vectorof SQL-Datum)))]
 [query-value (-> Connection Statement SQL-Datum * SQL-Datum)]
 [query-maybe-value (-> Connection Statement SQL-Datum * (Option SQL-Datum))]
 [query-rows (-> Connection Statement
                 [#:group SQL-Group]
                 [#:group-mode (Listof Group-Mode)]
                 SQL-Datum *
                 (Listof (Vectorof SQL-Datum)))]
 [in-query (-> Connection Statement
               [#:fetch (U Positive-Integer +inf.0)]
               [#:group SQL-Group]
               [#:group-mode (Listof Group-Mode)]
               SQL-Datum *
               (Sequenceof SQL-Datum))])

(require/typed/provide
 db/base
 [#:struct simple-result ([info : (Listof (Pairof Symbol SQL-Datum))])]
 [#:struct rows-result ([headers : (Listof Any)] [rows : (Listof (Vectorof SQL-Datum))])]
 [query (-> Connection Statement SQL-Datum * (U Simple-Result Rows-Result))]
 [group-rows (->* (Rows-Result #:group SQL-Group)
                  (#:group-mode (Listof Group-Mode))
                  Rows-Result)]
 [rows->dict (->* (Rows-Result
                   #:key SQL-Field
                   #:value SQL-Grouping)
                  (#:value-mode (Listof Group-Mode))
                  SQL-Dictionary)])

(require/typed/provide
 db/base
 [#:opaque Prepared-Statement prepared-statement?]
 [#:opaque Virtual-Statement virtual-statement?]
 [#:opaque Statement-Binding statement-binding?]
 [#:opaque Other-Statement prop:statement?]
 [prepare (-> Connection (U String Virtual-Statement) Prepared-Statement)]
 [prepared-statement-parameter-types (-> Prepared-Statement (Listof SQL-Type))]
 [prepared-statement-result-types (-> Prepared-Statement (Listof SQL-Type))]
 [bind-prepared-statement (-> Prepared-Statement (Listof SQL-Datum) Statement-Binding)]
 [virtual-statement (-> (U String (-> DBSystem String)) Virtual-Statement)])

(define statement? : (-> Any Boolean : Statement)
  (lambda [s]
    (or (string? s)
        (prepared-statement? s)
        (statement-binding? s)
        (virtual-statement? s)
        (prop:statement? s))))

(require/typed/provide
 db/base
 [start-transaction (-> Connection [#:isolation Isolation-Level] [#:option (Option Symbol)] Void)]
 [commit-transaction (-> Connection Void)]
 [rollback-transaction (-> Connection Void)]
 [in-transaction? (-> Connection Boolean)]
 [needs-rollback? (-> Connection Boolean)]
 [call-with-transaction (All (a) (-> Connection (-> a)
                                     [#:isolation Isolation-Level]
                                     [#:option (Option Symbol)]
                                     a))])

(define-syntax (require/db/provide stx)
  (syntax-case stx [->*]
    [(_ (->* (mandatory-dom ...) (optional-dom ...) connector))
     (with-syntax ([db/module (format-id #'connector "db/~a" (syntax-e #'connector))]
                   [db-connect (format-id #'connector "~a-connect" (syntax-e #'connector))]
                   [db-data-source (format-id #'connector "~a-data-source" (syntax-e #'connector))])
       #'(begin (require/typed/provide db/module
                                       [db-connect (->* (mandatory-dom ...)
                                                        (optional-dom ...)
                                                        Connection)])
                
                (require/typed/provide db/base
                                       [db-data-source (->* ()
                                                            (mandatory-dom ... optional-dom ...)
                                                            Data-Source)])))]))

(require/typed/provide
 db/util/testing
 [high-latency-connection (-> Connection Nonnegative-Real [#:sleep-atomic? Any] Connection)])
