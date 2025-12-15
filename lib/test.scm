(define b0 (new-boundry '(0 0) '(1 1)))
(define b1 (new-boundry '(0 0) 1))
(define b2 (new-boundry '(1 1) '(2 2)))
(define b3 (new-boundry '(2 2) 0.75))

(pp (collision? b0 b1))
(pp (collision? b2 b1))
(pp (collision? b2 b0))
(pp (collision? b1 b3))
