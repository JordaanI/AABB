;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                        ;;;
;;;     __  .______     ______   .__   __.    .______    __    _______.                    ;;;
;;;    |  | |   _  \   /  __  \  |  \ |  |    |   _  \  |  |  /  _____|        _____       ;;;
;;;    |  | |  |_)  | |  |  |  | |   \|  |    |  |_)  | |  | |  |  __      ^..^     \9     ;;;
;;;    |  | |      /  |  |  |  | |  . `  |    |   ___/  |  | |  | |_ |     (oo)_____/      ;;;
;;;    |  | |  |\  \  |  `--'  | |  |\   |    |  |      |  | |  |__| |        WW  WW       ;;;
;;;    |__| | _| `._|  \______/  |__| \__|    | _|      |__|  \______|                     ;;;
;;;                                                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Author: Ivan Jordaan
;; Date: 2025-12-05
;; email: ivan@axoinvent.com
;; Project: Main file for AABBTree implmented in Gambit Scheme
;;

(define-type boundry id: 4B48A7EC-7E41-42E9-9AAB-DDF5DDB9DE47 type data)

(define (_boundry start end)
  (make-boundry
   'extent
   (list->table `((0 ,@start)
                  (1 ,@end)))))

(define (new-boundry center radius)
  (make-boundry
   'radius
   (list->table `((0 . ,center)
                  (1 . ,radius)))))

(define (boundry-0 boundry #!optional value)
  (let ((data (table-copy (boundry-data boundry))))
    (if value (begin
                (table-set! data 0 value)
                (boundry-data-set! boundry data))
        (table-ref data 0))))

(define (boundry-1 boundry #!optional value)
  (let ((data (table-copy (boundry-data boundry))))
    (if value (begin
                (table-set! data 1 value)
                (boundry-data-set! boundry data))
        (table-ref data 1))))

(define (radius->extent boundry)
  (let ((b0 (boundry-0 boundry))
        (b1 (boundry-1 boundry)))
    (_boundry
     (map + b0 b1)
     (map - b0 b1))))

(define (extent->radius boundry)
  (let* ((b0 (boundry-0 boundry))
	 (b1 (boundry-1 boundry))
	 (p (map (lambda (x y) (/ (+ x y) 2)) b1 b0)))
    (new-boundry p
		 (map
		  (lambda (e) (/ e 2.0))
		  p))))

(define (extent-collision b1 b2)
  (let ((b1-0 (boundry-0 b1))
        (b1-1 (boundry-1 b1))
        (b2-0 (boundry-0 b2))
        (b2-1 (boundry-1 b2)))
    (apply andf
           (append
            (map <= b1-0 b2-1)
            (map >= b1-1 b2-0)))))

(define (radius-collision b1 b2)
  (let ((b1-0 (boundry-0 b1))
        (b1-1 (boundry-1 b1))
        (b2-0 (boundry-0 b2))
        (b2-1 (boundry-1 b2)))
    (<= (apply + (map (lambda (term)
                        (* term term))
                      (map - b1-0 b2-0)))
        (expt (+ b1-1 b2-1) 2))))

(define (extent-radius-collision e r)
  (let ((b1 (extent->radius e))
	(b1-0 (boundry-0 b1))
        (b1-1 (boundry-1 b1))
        (b2-0 (boundry-0 r))
        (b2-1 (boundry-1 r))
	(dist (map abs (map - b2-0 b1-0))))
    (apply orf
	   (cons
	    (<= (apply + (map (lambda (e0 e1)
				(expt (- e0 e1) 2))
			      b2 b1)))
	    (map <= dist b1-1)))))

(define (check-collision b1 b2)
  #t)
