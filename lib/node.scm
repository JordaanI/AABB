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
;; Date: 2025-12-16
;; email: ivan@axoinvent.com
;; Project:
;;

(define-type node id: 3F469D30-9A3A-4CD9-AA7D-D9BBA0C900FF boundry left right dirty?)

(define (boundry->node boundry)
  (make-node
   boundry
   #f
   #f
   #f))

(define (update-branch node)
  (let* ((left (node-left node))
	 (right (node-right node))
	 (clean-left? (and left (node-dirty? left)))
	 (clean-right? (and right (node-dirty? right))))
    (cond
     ((and left (node-dirty? left))
      (let ((new-left (update-branch left)))
	(make-node
	 (merge (node-boundry new-left) (node-boundry right))
	 new-left
	 right
	 #f)))
     ((and right (node-dirty? right))
      (let ((new-right (update-branch right)))
	(make-node
	 (merge (node-boundry new-left) (node-boundry right))
	 new-left
	 right
	 #f)))
     (left
      (make-node
       (merge (node-boundry left) (node-boundry right))
       left
       right
       #f))
     (#t
      (make-node
       (node-boundry node)
       left
       right
       #f)))))

(define (tree-boundry-add tree boundry)
  (update-branch
   (let ((node (boundry->node boundry))
	 (left (node-left tree))
	 (right (node-right tree)))
     (if (and
	  (not left)
	  (not right))
	 (make-node
	  (merge (node-boundry tree) boundry)
	  tree
	  node
	  #f)
	 (let ((ml (merge (node-boundry left) boundry))
	       (mr (merge boundry (node-boundry right))))
	   (if (<= (boundry-volume ml) (boundry-volume mr))
	       (make-node
		#f
		(tree-boundry-add left boundry)
		right       
		#t)
	       (make-node
		#f
		left
		(tree-boundry-add right boundry)
		#t)))))))

(define (tree-node-remove tree boundry)
  (update-branch
  (let ((left (node-left tree))
	(right (node-right tree)))
    (cond
     ((equal? boundry (node-boundry left))
	(make-node
	 #f
	 tree
	 right
	 #t))
     ((equal? boundry (node-boundry right))
	(make-node
	 #f
	 left
	 tree
	 #t))
     (left
      (make-node
       #f
       (tree-node-remove left boundry)
       (tree-node-remove right boundry)
       #t))
     (#t
      tree)))))

(define (build-tree . boundries)
  (letrec ((build (lambda (tree boundries)
		    (if (null? boundries) tree
			(build
			 (tree-boundry-add tree (car boundries))
			 (cdr boundries))))))
    (build (boundry->node (car boundries)) (cdr boundries))))

(define (boundry-collisions tree boundry)
  (let ((ref (node-boundry tree))
	(left (node-left tree))
	(right (node-right tree)))
    (if (collision? ref boundry)
	(if (and (not left) (not right))
	    (list ref)
	    (append
	     (boundry-collisions left boundry)
	     (boundry-collisions right boundry)))
	(list))))
