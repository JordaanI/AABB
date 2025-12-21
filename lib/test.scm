(define b0 (new-boundry '(2 2) '(1 1)))
(define b1 (new-boundry '(2 -2) '(1 1)))
(define b2 (new-boundry '(-2 2) '(1 1)))
(define b3 (new-boundry '(-2 -2) '(1 1)))

(define b4 (new-boundry '(0 0) 0.5))
(define b5 (new-boundry '(2 2) '(1 3)))

(define boundries (list b0 b1 b2 b3 #;b4 #;b5))

(pp boundries)

(define tree (apply build-tree boundries))

(pp tree)

(define collisions (boundry-collisions tree b4))

(pp collisions)
