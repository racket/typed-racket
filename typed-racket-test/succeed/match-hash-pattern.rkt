#lang typed/racket

(match (hash 1 "2" 3 "4")
  [(hash 1 #{x : String} 3 #{y : String})
   (list x y)])

(match (hash 1 "2" 3 "4")
  [(hash* [1 #{x : String}] [3 #{y : String}])
   (list x y)])

(match (hash 1 "2" 3 "4")
  [(hash* [1 #{x : (U String Symbol)} #:default 'a] [3 #{y : (U String Boolean)} #:default #t])
   (list x y)])

(match (hash 1 "2" 3 "4")
  [(hash* [1 #{x : (U String Symbol)} #:default 'a] [3 #{y : (U String Boolean)} #:default #t] #:closed)
   (list x y)])

(match (hash 1 "2" 3 "4")
  [(hash* [1 #{x : (U String False)} #:default #f] [3 #{y : String}])
   (list x y)])

(match (hash 1 "2" 3 "4")
  [(hash* [1 #{x : (U String False)} #:default #f]
          [3 #{y : String}] #:closed)
   (list x y)])

(match (hash 1 "2" 3 "4")
  [(hash* [1 #{x : (U String False)} #:default #f]
          [3 #{y : String}]
          #:rest #{ht : (HashTable Integer String)})
   (list x y ht)])
