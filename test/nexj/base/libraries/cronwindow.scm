; Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
; Simple get next start time implementation that takes
; window hour, minute, second, and duration.
; If hour, minute, or second is null, then it is like *
; - Code can be extended to cover day, month, and year.
; - Could extend to take lists for each parameter, like (0 30) for minute to mean
;   on the hour and half hour. (Metaclass would have a start, step, and stop tuple
;   of (0, 30, 0) to generate that list.)
; Another obvious way of implementing time windows would be with recurrence classes,
; but those are in CRM.

(import 'java.util.Calendar)
(import 'java.util.Locale)
(import 'java.sql.Timestamp)
(import 'nexj.core.util.TimeZoneUtil)

; getNextStartTime(time winHour winMinute winSecond)
(define (getNextStartTime time winHour winMinute winSecond)
   (let (
         (cal (java.util.Calendar'getInstance (nexj.core.util.TimeZoneUtil'UTC) (java.util.Locale'ENGLISH)))
         (tHour '())
         (tMinute '())
         (tSecond '())
         (nextStart '())
      )
      (cal'time time)
      (cal'set (java.util.Calendar'MILLISECOND) 0)

      ; Compute next start time
      (set! tHour (cal'get (java.util.Calendar'HOUR_OF_DAY)))
      (if (not (null? winHour))
         (begin
            (let (
                  (maxHour (cal'getMaximum (java.util.Calendar'HOUR_OF_DAY)))
               )
               (cal'add (java.util.Calendar'HOUR_OF_DAY)
                  (modulo (+ (- (+ maxHour 1) tHour) winHour) (+ maxHour 1))
               )
               (cal'set (java.util.Calendar'MINUTE) (ifnull winMinute 0))
               (cal'set (java.util.Calendar'SECOND) (ifnull winSecond 0))
               (if (< (cast timestamp (cal'timeInMillis)) time)
                  (cal'add (java.util.Calendar'DATE) 1)
               )
            )
         )
      )
      (set! tMinute (cal'get (java.util.Calendar'MINUTE)))
      (if (not (null? winMinute))
         (begin
            (let (
                  (maxMinute (cal'getMaximum (java.util.Calendar'MINUTE)))
               )
               (cal'add (java.util.Calendar'MINUTE)
                  (modulo (+ (- (+ maxMinute 1) tMinute) winMinute) (+ maxMinute 1))
               )
               (cal'set (java.util.Calendar'SECOND) (ifnull winSecond 0))
               (if (< (cast timestamp (cal'timeInMillis)) time)
                  (cal'add (java.util.Calendar'HOUR) 1)
               )
            )
         )
      )
      (set! tSecond (cal'get (java.util.Calendar'SECOND)))
      (if (not (null? winSecond))
         (begin
            (let (
                  (maxSecond (cal'getMaximum (java.util.Calendar'SECOND)))
               )
               (cal'add (java.util.Calendar'SECOND)
                  (modulo (+ (- (+ maxSecond 1) tSecond) winSecond) (+ maxSecond 1))
               )
            )
         )
      )
      (cast timestamp (cal'timeInMillis))
   )
)
(define (getPreviousStartTime time winHour winMinute winSecond)
   (let (
         (cal (java.util.Calendar'getInstance (nexj.core.util.TimeZoneUtil'UTC) (java.util.Locale'ENGLISH)))
         (tHour '())
         (tMinute '())
         (tSecond '())
         (nextStart '())
      )
      (cal'time time)
      (cal'set (java.util.Calendar'MILLISECOND) 0)
      (set! tHour (cal'get (java.util.Calendar'HOUR_OF_DAY)))

      ; Compute previous start time
      (if (not (null? winHour))
         (begin
            (cal'add (java.util.Calendar'HOUR_OF_DAY)
               (+ (- tHour) winHour)
            )
            (cal'set (java.util.Calendar'MINUTE) (ifnull winMinute 0))
            (cal'set (java.util.Calendar'SECOND) (ifnull winSecond 0))
            (if (> (cast timestamp (cal'timeInMillis)) time)
               (cal'add (java.util.Calendar'DATE) -1)
            )
         )
      )
      (set! tMinute (cal'get (java.util.Calendar'MINUTE)))
      (if (not (null? winMinute))
         (begin
            (cal'add (java.util.Calendar'MINUTE)
               (+ (- tMinute) winMinute)
            )
            (cal'set (java.util.Calendar'SECOND) (ifnull winSecond 0))
            (if (> (cast timestamp (cal'timeInMillis)) time)
               (cal'add (java.util.Calendar'HOUR) -1)
            )
         )
      )
      (set! tSecond (cal'get (java.util.Calendar'SECOND)))
      (if (not (null? winSecond))
         (begin
            (let (
                  (maxSecond (cal'getMaximum (java.util.Calendar'SECOND)))
               )
               (cal'add (java.util.Calendar'SECOND)
                  (+ (- tSecond) winSecond)
               )
            )
         )
      )
      (cast timestamp (cal'timeInMillis))
   )
)

; getWindowStartTime(time winHour winMinute winSecond winDuration)
(define (getWindowStartTime time winHour winMinute winSecond winDuration)
   (let (
         (endTime '())
         (prevStart (getPreviousStartTime time winHour winMinute winSecond))
      )
   
      ; Compute next start time
      (set! endTime (cast timestamp (+ (cast long prevStart) winDuration)))
      (if (and (<= prevStart time) (< time endTime))
         prevStart
         (getNextStartTime time winHour winMinute winSecond)
      )
   )
)





; ===== TESTS =====


;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:00:00.000 '() '() 0)
;   #m2008-07-24T18:00:00.000
;   ))
;   (error "Fail #1-1")
;   (display "OK #1-1")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:00:01.000 '() '() 0)
;   #m2008-07-24T18:01:00.000
;   ))
;   (error "Fail #1-2")
;   (display "OK #1-2")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:00:59.999 '() '() 0)
;   #m2008-07-24T18:01:00.000
;   ))
;   (error "Fail #1-3")
;   (display "OK #1-3")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:00:00.000 '() '() 30)
;   #m2008-07-24T18:00:30.000
;   ))
;   (error "Fail #1-4")
;   (display "OK #1-4")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:00:01.000 '() '() 30)
;   #m2008-07-24T18:00:30.000
;   ))
;   (error "Fail #1-5")
;   (display "OK #1-5")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:00:59.999 '() '() 30)
;   #m2008-07-24T18:01:30.000
;   ))
;   (error "Fail #1-6")
;   (display "OK #1-6")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:00:00.000 '() 10 '())
;   #m2008-07-24T18:10:00.000
;   ))
;   (error "Fail #1-7")
;   (display "OK #1-7")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:00:30.000 '() 10 '())
;   #m2008-07-24T18:10:00.000
;   ))
;   (error "Fail #1-8")
;   (display "OK #1-8")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:15:30.000 '() 10 '())
;   #m2008-07-24T19:10:00.000
;   ))
;   (error "Fail #1-9")
;   (display "OK #1-9")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:15:30.000 '() 10 15)
;   #m2008-07-24T19:10:15.000
;   ))
;   (error "Fail #1-10")
;   (display "OK #1-10")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:10:15.000 '() 10 15)
;   #m2008-07-24T18:10:15.000
;   ))
;   (error "Fail #1-11")
;   (display "OK #1-11")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:10:16.000 '() 10 15)
;   #m2008-07-24T19:10:15.000
;   ))
;   (error "Fail #1-12")
;   (display "OK #1-12")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:10:14.000 '() 10 15)
;   #m2008-07-24T18:10:15.000
;   ))
;   (error "Fail #1-13")
;   (display "OK #1-13")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:00:00.000 18 '() '())
;   #m2008-07-24T18:00:00.000
;   ))
;   (error "Fail #1-14")
;   (display "OK #1-14")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:00:01.000 18 '() '())
;   #m2008-07-25T18:00:00.000
;   ))
;   (error "Fail #1-15")
;   (display "OK #1-15")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T17:59:59.999 18 '() '())
;   #m2008-07-24T18:00:00.000
;   ))
;   (error "Fail #1-16")
;   (display "OK #1-16")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T17:59:59.999 18 30 '())
;   #m2008-07-24T18:30:00.000
;   ))
;   (error "Fail #1-17")
;   (display "OK #1-17")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:29:59.999 18 30 '())
;   #m2008-07-24T18:30:00.000
;   ))
;   (error "Fail #1-18")
;   (display "OK #1-18")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:30:01.000 18 30 '())
;   #m2008-07-25T18:30:00.000
;   ))
;   (error "Fail #1-19")
;   (display "OK #1-19")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:30:01.000 18 30 1)
;   #m2008-07-24T18:30:01.000
;   ))
;   (error "Fail #1-20")
;   (display "OK #1-20")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:30:02.000 18 30 1)
;   #m2008-07-25T18:30:01.000
;   ))
;   (error "Fail #1-21")
;   (display "OK #1-21")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:29:59.000 18 30 1)
;   #m2008-07-24T18:30:01.000
;   ))
;   (error "Fail #1-22")
;   (display "OK #1-22")
;)
;(if (not (equal?
;   (getNextStartTime #m2008-07-24T18:30:00.000 18 30 1)
;   #m2008-07-24T18:30:01.000
;   ))
;   (error "Fail #1-23")
;   (display "OK #1-23")
;)
;
;
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:00:00.000 '() '() 0)
;   #m2008-07-24T18:00:00.000
;   ))
;   (error "Fail #2-1")
;   (display "OK #2-1")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:00:01.000 '() '() 0)
;   #m2008-07-24T18:00:00.000
;   ))
;   (error "Fail #2-2")
;   (display "OK #2-2")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:00:59.999 '() '() 0)
;   #m2008-07-24T18:00:00.000
;   ))
;   (error "Fail #2-3")
;   (display "OK #2-3")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:00:00.000 '() 10 '())
;   #m2008-07-24T17:10:00.000
;   ))
;   (error "Fail #2-4")
;   (display "OK #2-4")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:00:30.000 '() 10 '())
;   #m2008-07-24T17:10:00.000
;   ))
;   (error "Fail #2-5")
;   (display "OK #2-5")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:15:30.000 '() 10 '())
;   #m2008-07-24T18:10:00.000
;   ))
;   (error "Fail #2-6")
;   (display "OK #2-6")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:15:30.000 '() 10 15)
;   #m2008-07-24T18:10:15.000
;   ))
;   (error "Fail #2-7")
;   (display "OK #2-7")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:10:15.000 '() 10 15)
;   #m2008-07-24T18:10:15.000
;   ))
;   (error "Fail #2-8")
;   (display "OK #2-8")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:10:16.000 '() 10 15)
;   #m2008-07-24T18:10:15.000
;   ))
;   (error "Fail #2-9")
;   (display "OK #2-9")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:10:14.000 '() 10 15)
;   #m2008-07-24T17:10:15.000
;   ))
;   (error "Fail #2-10")
;   (display "OK #2-10")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:00:00.000 18 '() '())
;   #m2008-07-24T18:00:00.000
;   ))
;   (error "Fail #2-11")
;   (display "OK #2-11")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T19:00:00.000 18 '() '())
;   #m2008-07-24T18:00:00.000
;   ))
;   (error "Fail #2-12")
;   (display "OK #2-12")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T17:59:59.999 18 '() '())
;   #m2008-07-23T18:00:00.000
;   ))
;   (error "Fail #2-13")
;   (display "OK #2-13")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T19:00:00.000 18 1 1)
;   #m2008-07-24T18:01:01.000
;   ))
;   (error "Fail #2-14")
;   (display "OK #2-14")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:01:00.000 18 1 1)
;   #m2008-07-23T18:01:01.000
;   ))
;   (error "Fail #2-15")
;   (display "OK #2-15")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:01:01.000 18 1 1)
;   #m2008-07-24T18:01:01.000
;   ))
;   (error "Fail #2-16")
;   (display "OK #2-16")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T18:01:02.000 18 1 1)
;   #m2008-07-24T18:01:01.000
;   ))
;   (error "Fail #2-17")
;   (display "OK #2-17")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T19:01:02.000 18 1 1)
;   #m2008-07-24T18:01:01.000
;   ))
;   (error "Fail #2-18")
;   (display "OK #2-18")
;)
;(if (not (equal?
;   (getPreviousStartTime #m2008-07-24T17:01:02.000 18 1 1)
;   #m2008-07-23T18:01:01.000
;   ))
;   (error "Fail #2-19")
;   (display "OK #2-19")
;)
;
;
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:00:00.000 '() '() 0 30000)
;   #m2008-07-24T18:00:00.000
;   ))
;   (error "Fail #1")
;   (display "OK #1")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:00:01.000 '() '() 0 30000)
;   #m2008-07-24T18:00:00.000
;   ))
;   (error "Fail #2")
;   (display "OK #2")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:00:29.999 '() '() 0 30000)
;   #m2008-07-24T18:00:00.000
;   ))
;   (error "Fail #3")
;   (display "OK #3")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:00:30.000 '() '() 0 30000)
;   #m2008-07-24T18:01:00.000
;   ))
;   (error "Fail #4")
;   (display "OK #4")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:00:59.000 '() '() 0 30000)
;   #m2008-07-24T18:01:00.000
;   ))
;   (error "Fail #5")
;   (display "OK #5")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:00:59.999 '() '() 0 30000)
;   #m2008-07-24T18:01:00.000
;   ))
;   (error "Fail #6")
;   (display "OK #6")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:00:00.000 '() 1 1 30000)
;   #m2008-07-24T18:01:01.000
;   ))
;   (error "Fail #7")
;   (display "OK #7")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:00:59.999 '() 1 1 30000)
;   #m2008-07-24T18:01:01.000
;   ))
;   (error "Fail #8")
;   (display "OK #8")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:01:00.999 '() 1 1 30000)
;   #m2008-07-24T18:01:01.000
;   ))
;   (error "Fail #9")
;   (display "OK #9")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:01:01.000 '() 1 1 120000)
;   #m2008-07-24T18:01:01.000
;   ))
;   (error "Fail #10")
;   (display "OK #10")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:02:01.000 '() 1 1 120000)
;   #m2008-07-24T18:01:01.000
;   ))
;   (error "Fail #11")
;   (display "OK #11")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:03:00.999 '() 1 1 120000)
;   #m2008-07-24T18:01:01.000
;   ))
;   (error "Fail #12")
;   (display "OK #12")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:03:01.000 '() 1 1 120000)
;   #m2008-07-24T19:01:01.000
;   ))
;   (error "Fail #13")
;   (display "OK #13")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:03:00.000 '() 1 '() 120000)
;   #m2008-07-24T19:01:00.000
;   ))
;   (error "Fail #14")
;   (display "OK #14")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:02:59.999 '() 1 '() 120000)
;   #m2008-07-24T18:01:00.000
;   ))
;   (error "Fail #15")
;   (display "OK #15")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:00:00.000 18 30 15 3600000)
;   #m2008-07-24T18:30:15.000
;   ))
;   (error "Fail #16")
;   (display "OK #16")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:30:15.000 18 30 15 3600000)
;   #m2008-07-24T18:30:15.000
;   ))
;   (error "Fail #17")
;   (display "OK #17")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:30:16.000 18 30 15 3600000)
;   #m2008-07-24T18:30:15.000
;   ))
;   (error "Fail #18")
;   (display "OK #18")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T19:30:14.999 18 30 15 3600000)
;   #m2008-07-24T18:30:15.000
;   ))
;   (error "Fail #19")
;   (display "OK #19")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T19:30:15.000 18 30 15 3600000)
;   #m2008-07-25T18:30:15.000
;   ))
;   (error "Fail #20")
;   (display "OK #20")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:00:00.000 '() '() '() 3600000)
;   #m2008-07-24T18:00:00.000
;   ))
;   (error "Fail #21")
;   (display "OK #21")
;)
;(if (not (equal?
;   (getWindowStartTime #m2008-07-24T18:12:34.000 '() '() '() 3600000)
;   #m2008-07-24T18:12:34.000
;   ))
;   (error "Fail #22")
;   (display "OK #22")
;)
;

