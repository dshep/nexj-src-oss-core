(import '(java.util.TimeZone))

;returns a timestamp ceiled to the nearest datepart (SECOND, MINUTE, HOUR, HOUR_OF_DAY, DAY, DAY_OF_YEAR)
;returns: timestamp
; e.g. (date-round-to-nearest (now) 'MINUTE)   ->   "2006-08-15 18:06:00.000"   
(define (date-ceil-to-nearest ts datepart . timeZone)
   (define c (apply date-get-calendar timeZone))
   (if (null? ts) 
      '()
      (begin
         (c'time ts)
         (case datepart
            ((SECOND)
               (c'set (c'SECOND) (+ 1 (c'get (c'SECOND))))
               (c'clear (c'MILLISECOND)))
            ((MINUTE)
               (c'set (c'MINUTE) (+ 1 (c'get (c'MINUTE))))
               (c'clear (c'MILLISECOND))
               (c'clear (c'SECOND)))
            ((HOUR HOUR_OF_DAY)
               (c'set (c'HOUR_OF_DAY) (+ 1 (c'get (c'HOUR_OF_DAY))))
               (c'clear (c'MILLISECOND))
               (c'clear (c'SECOND))
               (c'clear (c'MINUTE)))
            ((DAY DAY_OF_YEAR)
               (c'set (c'DAY_OF_YEAR) (+ 1 (c'get (c'DAY_OF_YEAR))))
               (c'clear (c'MILLISECOND))
               (c'clear (c'SECOND))
               (c'clear (c'MINUTE))
               (c'set (c'HOUR_OF_DAY) 0))
         )
         (cast timestamp ((c'time)'time)))
))

; adds days (integer) days to the given timestamp
; @arg time timestamp the time in which to add the days to
; @arg days integer the days to add (or negative to subtract) to the given time
; @arg timeZone string the timezone that the calculation should be done in
; @ret timestamp the timestamp with the time added
; @example (date-add-days (now) -1)  ->   "2006-08-14 18:06:45.501"   [subtract one day from now]
(define (date-add-days time days . timeZone)
   (if (or (null? time) (null? days))
      '()
      (let ((c (apply date-get-calendar timeZone)))
         (c'time time)
         (c'add (c'DAY_OF_YEAR) days)
         (cast timestamp ((c'time)'time))
      )
   )
)

; adds minutes (integer) minutes to the given timestamp
; @arg time timestamp the time in which to add the minutes to
; @arg minutes integer the minutes to add (or negative to subtract) to the given time
; @arg timeZone string the timezone that the calculation should be done in
; @ret timestamp the timestamp with the time added
; @example  (date-add-minutes (now) -60)  ->   "2006-08-15 17:06:45.501"   [subtract one hour from now]
(define (date-add-minutes time minutes . timeZone)
   (if (or (null? time) (null? minutes))
      '()
      (let ((c (apply date-get-calendar timeZone)))
         (c'time time)
         (c'add (c'MINUTE) minutes)
         (cast timestamp ((c'time)'time))
      ))
)

; Returns an instance of the java.util.Calendar object with the current invocation context's time zone.
; Mostly used by the other date functions.
; @arg timeZone string/TimeZone optional time zone to override the default (invocation-context's) time zone.
; @ret Calendar An instance of java.util.Calendar
(define (date-get-calendar . timeZone)
   (let ((c (java.util.Calendar'instance)))
      (if (null? timeZone)
         (c'timeZone ((invocation-context)'timeZone))
         ;else:
         (begin
            (set! timeZone (car timeZone))
            (when (string? timeZone)
               (set! timeZone (java.util.TimeZone'getTimeZone timeZone))
            )
            (c'timeZone timeZone)
         )
      )
      ;return:
      c
   )
)

;Subtracts two timestamps and returns the number of minutes between them.
;Positive return value if t1 is after t2, zero if they are equal, negative otherwise.
;returns: integer
; e.g.
;  (define sooner (now))
;  (define later (date-add-minutes sooner 55))
;  (date-diff-minutes later sooner )    ->    55
;  (date-diff-minutes sooner later )    ->    -55
(define (date-diff-minutes t1 t2)
   (if (or (null? t1) (null? t2))
      '()
      (let* ((c1 (java.util.Calendar'instance))
             (c2 (java.util.Calendar'instance))
             (epoch1 0)
             (epoch2 0))
         (c1'time t1)
         (c2'time t2)
         (set! epoch1 ((c1'time)'time)) ; get num of ms since epoch for t1
         (set! epoch2 ((c2'time)'time)) ; get num of ms since epoch for t2
         (/ (- epoch1 epoch2) 60000)    ; 60000ms in a minute
      )
   )
)
