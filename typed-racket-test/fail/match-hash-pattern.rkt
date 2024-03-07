#lang typed/racket

(match (hash 1 "2" 3 "4")
  [(hash* [1 #{x : String} #:default #f] [3 #{y : String}])
   (list x y)])
