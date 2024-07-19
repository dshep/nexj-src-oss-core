; Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0

; The following classes test for an attribute resolution bug.
; The bug caused (OverrideTest1B'x) and (OverrideTest1B'y) to return null, because
; the attribute overrides didn't work correctly. But for OverrideTest2B they did
; work.
(define-class OverrideTest1A () ""
   (class-attribute x)
   (class-attribute y)
)

(define-class OverrideTest1B (OverrideTest1A) ""
   (class-attribute x :init "hi")
   (class-attribute y :init "bye")
)

(define-class OverrideTest2A () ""
   (class-attribute x)
)

(define-class OverrideTest2B (OverrideTest2A) ""
   (class-attribute x :init "hi")
   (class-attribute y :init "bye")
)
