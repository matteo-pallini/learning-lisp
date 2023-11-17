
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


(defun left_hand (vals &optional (n '())) (
			  cond ((null n) (left_hand vals (length vals)))
				((<= (length vals) (/ n 2)) '())
			       (t (cons (car vals) (left_hand (rest vals) n)))
))


(defun merge_lists (vals_1 vals_2) (
   cond ((null vals_1) vals_2)
  ((null vals_2) vals_1)
  ((< (car vals_1) (car vals_2)) (cons (car vals_1) (merge_lists (rest vals_1) vals_2)))
  (t (cons (car vals_2) (merge_lists vals_1 (rest vals_2))))
				    ))
(defvar family
'((colin nil nil)
(deirdre nil nil)
(arthur nil nil)
(kate nil nil)
(frank nil nil)
(linda nil nil)
(suzanne colin deirdre)
(bruce arthur kate)
(charles arthur kate)
(david arthur kate)
(ellen arthur kate)
(george frank linda)
(hillary frank linda)
(andre nil nil)
(tamara bruce suzanne)
(vincent bruce suzanne)
(wanda nil nil)
(ivan george ellen)
(julie george ellen)
(marie george ellen)
(nigel andre hillary)
(frederick nil tamara)
(zelda vincent wanda)
(joshua ivan wanda)
(quentin nil nil)
(robert quentin julie)
(olivia nigel marie)
(peter nigel marie)
(erica nil nil)
(yvette robert zelda)
(diane peter erica)))


(defun father (name) (
   second (assoc name family)
))

(defun mother (name) (
   third (assoc name family)
))

(defun parents (name) (
   rest (find-if #'(lambda (x) (eql (first x) name)) family)))


(defun children (name) (
			mapcar #'first (remove-if-not #'(lambda (x) (or (eql (second x) name) (eql (third x) name))) family)
			))

(defun siblings (name) (
   remove-if #'(lambda (x) (eql x name)) (remove-duplicates (reduce #'append (mapcar #'children (parents name))))
))

(defun mapunion (function vals) (
    reduce #'union (mapcar function vals)
))

(parents 'suzanne)
(children 'arthur)
(siblings 'bruce)
(mapunion #'REST '((1 A B C) (2 E C J) (3 F A B C D)))

(defun tail-rec-count-up (number &optional (result '())) (
   cond ((eql number 0) (cons 0 result))
        (t (tail-rec-count-up (- number 1) (cons number result)))
	))

(tail-rec-count-up 5)

(defun tail-rec-fact (number &optional (result 1)) (
   cond ((zerop number) result)
	(t (tail-rec-fact (- number 1) (* result number)))
))

(tail-rec-fact 7)
