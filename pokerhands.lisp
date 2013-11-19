;;; Poker hands kata

(defparameter *black* nil)
(defparameter *white* nil)

(defun read-card ()
  (let ((string (symbol-name (read))))
    (cons (parse-integer string :start 0 :end 1)
	  (char string 1))))

(defun read-cards ()
  (loop repeat 5 collect (read-card)))

(defun black ()
  (setq *black* (read-cards)))

(defun white ()
  (setq *white* (read-cards)))

(defun parse-input ()
  (funcall (read))
  (funcall (read)))

(defun rank (card)
  (car card))

(defun suite (card)
  (cdr card))

(defun sorted (hand)
  (sort hand (lambda (c1 c2) (> (rank c1) (rank c2)))))

(defun straight-flush (hand ranks)
  (when (and (straight hand ranks) (flush hand ranks))
    (cons 9 ranks)))

(defun four-of-a-kind (hand ranks)
  (declare (ignore hand))
  (when (member 4 (identical ranks))
    (cons 8 ranks)))

(defun full-house (hand ranks)
  (when (and (three-of-a-kind hand ranks) (pair hand ranks))
    (cons 7 ranks)))

(defun flush (hand ranks)
  (when (every (lambda (c) (eql (suite c) (suite (first hand))))
	       (rest hand))
    (cons 6 ranks)))

(defun straight (hand ranks)
  (declare (ignore hand))
  (when (loop for (r1 r2) on ranks when r2 unless (= (- r1 r2) 1)
	      return nil finally (return t))
    (cons 5 ranks)))

(defun three-of-a-kind (hand ranks)
  (declare (ignore hand))
  (when (member 3 (identical ranks))
    (list* 4 (position 3 (identical ranks)) ranks)))

(defun two-pairs (hand ranks)
  (declare (ignore hand))
  (when (eq 2 (count 2 (identical ranks)))
    (list* 3 (position 2 (identical ranks) :from-end t)
	   (position 2 (identical ranks)) ranks)))

(defun pair (hand ranks)
  (declare (ignore hand))
  (let ((n (position 2 (identical ranks))))
    (when n
      (list* 2 n ranks))))

(defun identical (ranks)
  (list* 0 0 (loop for r from 2 to 14 collect (count r ranks))))

(defun high-card (hand ranks)
  (declare (ignore hand))
  (cons 1 ranks))

(defun classify (hand)
  (loop with sorted = (sorted hand) with ranks = (mapcar #'rank sorted)
     for fn in '(straight-flush four-of-a-kind full-house flush straight
		 three-of-a-kind two-pairs pair high-card)
     for c = (funcall fn sorted ranks)
     when c do (return c)))

(defun compare-hands (hand1 hand2)
  (loop for c1 in (classify hand1)
        and c2 in (classify hand2)
        unless (= c1 c2)
        do (return (- c1 c2))))
