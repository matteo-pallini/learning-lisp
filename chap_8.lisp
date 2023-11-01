
(defun add-up (numbers) (
   cond ((null numbers) 0)
	 (t (+ (first numbers) (add-up (rest numbers))))
	 ))


(defun nth_custom (values idx) (
   cond ((eql 0 idx) (first values))
	 (t (nth_custom (rest values) (- idx 1)))
			 ))


(defun every_other (vals &optional (keep t)) (
  cond ((null vals) '())
  ((eql keep t) (cons (car vals) (every_other (cdr vals) (not keep))))
  (t (every_other (cdr vals) (not keep)))
  ))
