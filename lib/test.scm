(define b0 (_boundry '(0 0) '(1 1)))
(define b1 (_boundry '(0.5 0.5) '(2 2)))
(define b2 (new-boundry '(0 0) '(1 1)))

(pp (extent-collision b0 b1))
