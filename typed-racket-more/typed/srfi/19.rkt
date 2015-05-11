#lang typed/racket/base

(require/typed/provide
 srfi/19
 [#:opaque Time time?]
 [#:opaque Date date?])

(require/typed/provide
 srfi/19
 
 ; Constants
 [time-duration Symbol]
 [time-monotonic Symbol]
 [time-process Symbol]
 [time-tai Symbol]
 [time-thread Symbol]
 [time-utc Symbol]
 
 ; Current time and clock resolution
 [current-date ([] [Integer] . ->* . Date)]
 [current-julian-day (-> Real)]
 [current-modified-julian-day (-> Real)]
 [current-time ([] [Symbol] . ->* . Time)]
 [time-resolution ([] [Symbol] . ->* . Integer)]
 
 ; Time object and accessors
 [make-time (Symbol Index Byte -> Time)]
 [time-type (Time -> Symbol)]
 [time-nanosecond (Time -> Index)]
 [time-second (Time -> Byte)]
 [set-time-type! (Time Symbol -> Void)]
 [set-time-nanosecond! (Time Index -> Void)]
 [set-time-second! (Time Byte -> Void)]
 [copy-time (Time -> Time)]
 
 ; Time comparison procedures
 [time<=? (Time Time -> Boolean)]
 [time<? (Time Time -> Boolean)]
 [time=? (Time Time -> Boolean)]
 [time>=? (Time Time -> Boolean)]
 [time>? (Time Time -> Boolean)]
 
 ; Time arithmetic procedures
 [time-difference (Time Time -> Time)]
 [time-difference! (Time Time -> Time)]
 [add-duration (Time Time -> Time)]
 [add-duration! (Time Time -> Time)]
 [subtract-duration (Time Time -> Time)]
 [subtract-duration! (Time Time -> Time)]
 
 ; Date object and accessors
 [make-date (Index Byte Byte Byte Byte Positive-Byte Integer Integer -> Date)]
 [date-nanosecond (Date -> Index)]
 [date-second (Date -> Byte)]
 [date-minute (Date -> Byte)]
 [date-hour (Date -> Byte)]
 [date-day (Date -> Byte)]
 [date-month (Date -> Positive-Byte)]
 [date-year (Date -> Integer)]
 [date-zone-offset (Date -> Integer)]
 [date-year-day (Date -> Positive-Index)]
 [date-week-day (Date -> Byte)]
 [date-week-number (Date Byte -> Byte)]
 
 ; Time/Date/Julian Day/Modified Julian Day Converters
 [date->julian-day (Date -> Real)]
 [date->modified-julian-day (Date -> Real)]
 [date->time-monotonic (Date -> Time)]
 [date->time-tai (Date -> Time)]
 [date->time-utc (Date -> Time)]
 [julian-day->date ([Real] [Integer] . ->* . Date)]
 [julian-day->time-monotonic (Real -> Time)]
 [julian-day->time-tai (Real -> Time)]
 [julian-day->time-utc (Real -> Time)]
 [modified-julian-day->date ([Real] [Integer] . ->* . Date)]
 [modified-julian-day->time-monotonic (Real -> Time)]
 [modified-julian-day->time-tai (Real -> Time)]
 [modified-julian-day->time-utc (Real -> Time)]
 [time-monotonic->date ([Time] [Integer] . ->* . Date)]
 [time-monotonic->julian-day (Time -> Real)]
 [time-monotonic->modified-julian-day (Time -> Real)]
 [time-monotonic->time-tai (Time -> Time)]
 [time-monotonic->time-tai! (Time -> Time)]
 [time-monotonic->time-utc (Time -> Time)]
 [time-monotonic->time-utc! (Time -> Time)]
 [time-tai->date ([Time] [Integer] . ->* . Date)]
 [time-tai->julian-day (Time -> Real)]
 [time-tai->modified-julian-day (Time -> Real)]
 [time-tai->time-monotonic (Time -> Time)]
 [time-tai->time-monotonic! (Time -> Time)]
 [time-tai->time-utc (Time -> Time)]
 [time-tai->time-utc! (Time -> Time)]
 [time-utc->date ([Time] [Integer] . ->* . Date)]
 [time-utc->julian-day (Time -> Real)]
 [time-utc->modified-julian-day (Time -> Real)]
 [time-utc->time-monotonic (Time -> Time)]
 [time-utc->time-monotonic! (Time -> Time)]
 [time-utc->time-tai (Time -> Time)]
 [time-utc->time-tai! (Time -> Time)]
 
 ; Date to String/String to Date Converters
 [date->string ([Date] [String] . ->* . String)]
 [string->date (String String -> Date)]
 )
