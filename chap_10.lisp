
(defvar *friends* nil)
(defvar *counter* 0)

(defun meet (person)
  (cond ((equal person (first *friends*))
	 'we-just-met)
	((member person *friends*)
	 'we-know-each-other)
	(t (push person *friends*) (incf *counter*)
	   'pleased-to-meet-you)))


(meet 'freddy)
(meet 'cindy)
(meet 'fred)
(format t "counter ~2d ~%" *counter*)
