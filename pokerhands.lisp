;;; Poker hands kata

(defparameter *color*
  (loop for x across "CDHS"
     and r from 1 nconc
       (loop for y across "23456789TJQKA" collect
	    (cons
	     (intern (format nil "~A~A" y x))
	     r))))

(defparameter *rank*
  (loop for x across "23456789TJQKA"
     and r from 2 nconc
       (loop for y across "CDHS" collect
	    (cons
	     (intern (format nil "~A~A" x y))
	     r))))

(defparameter *black* '())
(defparameter *white* '())

(defun black ()
  (setq *black* (loop repeat 5 collect (read))))

(defun white ()
  (setq *white* (loop repeat 5 collect (read))))

(defun parse-input ()
  (funcall (read))
  (funcall (read)))

(defun rank (card)
  (cdr (assoc card *rank*)))

(defun sort-hand (hand)
  (sort (mapcar #'rank hand) #'>))

(defun color-hand (hand)
  (mapcar #'color hand))

(defun straight-flush (hand)
  (and (straight hand) (flush hand)))

(defun four-of-a-kind (hand)
  (when (member 4 (identical hand))
    (cons 7 hand)))

(defun full-house (hand)
  (when (and (three-of-a-kind hand) (two-pairs hand))
    (cons 6 hand)))

(defun flush (hand rank)
  (when (member 5 (identical (color-hand hand)))))

(defun straight (hand))

(defun three-of-a-kind (hand)
  (when (member 3 (identical hand))
    (cons 3 hand)))

(defun two-pairs (hand)
  (when (eq 2 (count 2 (identical hand)))
    (cons 3 hand)))

(defun pair (hand rank)
  (let ((n (position 2 (identical hand))))
    (when n
      (list* 2 n rank))))

(defun identical (hand)
  (list* 0 0 (loop for r from 2 to 14 collect (count r (sort-hand hand)))))

(defun high-card (hand rank)
  (declare (ignore hand))
  (cons 1 rank))

(defun rank-hand (hand)
  (loop with rank = (sort-hand hand)
     for fn in '(pair high-card)
     for r = (funcall fn hand rank)
     when r do (return r)))

(defun compare-hands (hand1 hand2)
  (loop for r1 in (rank-hand hand1)
        and r2 in (rank-hand hand2)
        unless (= r1 r2)
        do (return (- r1 r2))))
