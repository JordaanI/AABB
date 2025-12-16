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

(define (square-radius? boundry)
  (and
   (eq? (boundry-type boundry) 'radius)
   (list? (table-ref (boundry-data boundry) 1))))

(define (circle-radius? boundry)
  (and
   (eq? (boundry-type boundry) 'radius)
   (number? (table-ref (boundry-data boundry) 1))))

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
  (let* ((b0 (boundry-0 boundry))
         (b1 (boundry-1 boundry))
	 (factor (if (list? b1) b1
		     (make-list (length b0) b1))))
    (_boundry
     (map - b0 factor)
     (map + b0 factor))))

(define (extent->radius boundry)
  (let ((b0 (boundry-0 boundry))
	(b1 (boundry-1 boundry))
	(by2 (lambda (e) (/ e 2.0))))
    (new-boundry
     (map by2 (map + b1 b0))
     (map by2 (map - b1 b0)))))

(define (_resolve_boundry b1 b2)
  (lambda (ss sc cs cc)
    (or
     (and
      (square-radius? b1)
      (or
       (and
	(square-radius? b2)
	(ss (radius->extent b1) (radius->extent b2)))
       (and
	(circle-radius? b2)
	(cs b1 b2))))
     (and
      (circle-radius? b1)
      (or
       (and
	(square-radius? b2)
	(cs b2 b1))
       (and
	(circle-radius? b2)
	(cc b1 b2)))))))

(define (extent-collision? b1 b2)
  (let ((b1-0 (boundry-0 b1))
        (b1-1 (boundry-1 b1))
        (b2-0 (boundry-0 b2))
        (b2-1 (boundry-1 b2)))
    (apply andf
           (append
            (map <= b1-0 b2-1)
            (map >= b1-1 b2-0)))))

(define (radius-collision? b1 b2)
  (let ((b1-0 (boundry-0 b1))
        (b1-1 (boundry-1 b1))
        (b2-0 (boundry-0 b2))
        (b2-1 (boundry-1 b2)))
    (<= (apply + (map (lambda (term)
                        (* term term))
                      (map - b1-0 b2-0)))
        (expt (+ b1-1 b2-1) 2))))

(define (extent-radius-collision? b1 b2)
  (let* ((b1-0 (boundry-0 b1))
         (b1-1 (boundry-1 b1))
         (b2-0 (boundry-0 b2))
         (b2-1 (boundry-1 b2))
	 (cx (first b2-0))
	 (cy (second b2-0))
	 (x0 (first b1-0))
	 (y0 (second b1-0))
	 (x1 (first b1-1))
	 (y1 (second b1-1))
	 (dx (- cx (cond
		    ((< cx x0) x0)
		    ((> cx x1) x1)
		    (#t cx))))
	 (dy (- cy (cond
		    ((< cy y0) y0)
		    ((> cy y1) y1)
		    (#t cy)))))
    (<= (+ (expt dx 2) (expt dy 2)) (expt b2-1 2))))

(define (collision? b1 b2)
  ((_resolve_boundry b1 b2)
   extent-collision?
   extent-radius-collision?
   extent-radius-collision?
   radius-collision?))

(define (extent-merge b1 b2)
  (let ((b1-0 (boundry-0 b1))
        (b1-1 (boundry-1 b1))
        (b2-0 (boundry-0 b2))
        (b2-1 (boundry-1 b2)))
    (extent->radius
     (_boundry
      (list (min (first b1-0) (first b2-0)) (min (second b1-0) (second b2-0)))
      (list (max (first b1-1) (first b2-1)) (max (second b1-1) (second b2-1)))))))

(define (merge b1 b2)
  (cond
   ((and (square-radius? b1) (square-radius? b2))
    (extent-merge b1 b2))
   ((square-radius? b1)
    (extent-merge b1 (radius->extent b2)))
   ((square-radius? b2)
    (extent-merge (radius->extent b1) b2))
   (#t (extent-merge (radius->extent b1) (radius->extent b2)))))
