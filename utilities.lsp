(defun startState ()
(let (start)

	(setf start (make-array '(8 8) 
		:initial-contents '(( - - - - - - - - ) (- - - - - - - - ) (- - - - - - - - ) (- - - W B - - -) (- - - B W - - -) (- - - - - - - -) (- - - - - - - -) (- - - - - - - -))
		)
	)
)
)

(defun printState (state)
(let (count)

	(setf count 1)
	(format t "~%  1 2 3 4 5 6 7 8~%")
	(dotimes (x 8)
		(format t "~s " count)
		(dotimes (y 8)
			(format t "~s " (aref state x y))
		)
		(format t "~%")
		(incf count)
	)

)
)