(defun startState ()
(let (start)

	(setf start '( - - - - - - - - - - - - - - - - - - - - - - - - - - - W B - - - - - - B W - - - - - - - - - - - - - - - - - - - - - - - - - - -) )
	
)
)


(defun printState (state)
(let (row count)

	(setf row 0)
	(setf count 0)
	(format t "~%  1 2 3 4 5 6 7 8")
	;(format t " ~s " row)
	(dolist (index state)
		(when (equal (mod count 8) 0)
			(incf row)
			(format t "~% ~s " row)
		)
		(format t "~s " index)
		(incf count)
	
	)

)
)

(defun move-generator (position player)
(let ( moves currentPlayer next currentSpace foundSpace viableFlag row col)
	(setf moves ())
	
	(when (equal player "White")
		(setf currentPlayer 'W)
		(setf next 'B)	
	)
	(when (equal player "Black")
		(setf currentPlayer 'B)
		(setf next 'W)
	)
	
	(setf currentSpace -1)
	(dolist (index position)
		(incf currentSpace)
	
		(when (equal index next)
			(format t "~s index ~s ~s ~%" currentSpace index (nth (- currentSpace 1) position ))
			(when (and (>= (- currentSpace 1) 0) (equal (nth (- currentSpace 1) position ) '-) ) ; left
				(setf viableFlag 0)
				(do ((foundSpace currentSpace (incf foundSpace) ) (row (floor currentSpace 8) (incf row) ) (col (mod currentSpace 8) (incf col) ) )
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
					(when (> col 7)
						(setf viableFlag -1)
					)					
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (- currentSpace 1) ) ) )
				)
				
			
			)
			(format t "broke right ~%")
			(when (and (>= (+ currentSpace 1) 0) (equal (nth (+ currentSpace 1) position ) '-) ); right
				(setf viableFlag 0)
				(do ((foundSpace currentSpace (1- foundSpace) ) (row (floor currentSpace 8) (1- row) ) (col (mod currentSpace 8) (1- col) ) )
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (< col 0)
						(setf viableFlag -1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (+ currentSpace 1) ) ) )
				)
			
			)
			
			(when (and (>= (- currentSpace 8) 0) (equal (nth ( - currentSpace 8) position) '-) ) ; up
				(setf viableFlag 0)
				(do ((foundSpace currentSpace (+ foundSpace 8) ) (row (floor currentSpace 8) (incf row) ) (col (mod currentSpace 8) (incf col) ) ) 
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (> row 7)
						(setf viableFlag -1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					)
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (- currentSpace 8) ) ) )
				)
				
			
			)
			
			(when (and (>= (+ currentSpace 8) 0) (equal (nth (+ currentSpace 8) position) '-) ) ; down
				(setf viableFlag 0)
				(do ((foundSpace currentSpace (- foundSpace 8) ) (row (floor currentSpace 8) (1- row) ) (col (mod currentSpace 8) (1- col) ) ) 
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (< row 0)
						(setf viableFlag -1)
					)
					(format t "broke down ~s ~s ~%" foundSpace row)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (+ currentSpace 8) ) ) )
				)
				
			
			)
			
			(when (and (>= (- currentSpace 9) 0) (equal (nth (- currentSpace 9) position) '-) ) ; up and left
				(setf viableFlag 0)
				(do ((foundSpace currentSpace (+ foundSpace 9) ) (row (floor currentSpace 8) (incf row) ) (col (mod currentSpace 8) (incf col) ) ) 
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (or (> row 7) (> col 7) )
						(setf viableFlag -1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (- currentSpace 9) ) ) )
				)
			
			)
			
			(when (and (>= (- currentSpace 7) 0) (equal (nth (- currentSpace 7) position) '-) ) ; up and right
				(setf viableFlag 0)
				(do ((foundSpace currentSpace (+ foundSpace 7) ) (row (floor currentSpace 8) (incf row) ) (col (mod currentSpace 8) (1- col) ) ) 
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (or (> row 7) (< col 0) )
						(setf viableFlag -1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (- currentSpace 7) ) ) )
				)
			
			)
			
			(when (and (>= (+ currentSpace 7) 0) (equal (nth (+ currentSpace 7) position) '-) ) ; down and left
				(setf viableFlag 0)
				(do ((foundSpace currentSpace (- foundSpace 7) ) (row (floor currentSpace 8) (1- row) ) (col (mod currentSpace 8) (incf col) ) ) 
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (or (< row 0) (> col 7) )
						(setf viableFlag -1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (+ currentSpace 7) ) ) )
				)
				
			)
			
			(when (and (>= (+ currentSpace 9) 0) (equal (nth ( + currentSpace 9) position) '-) ) ; down and right
				(setf viableFlag 0)
				(do ((foundSpace currentSpace (- foundSpace 9) ) (row (floor currentSpace 8) (1- row) ) (col (mod currentSpace 8) (1- col) ) ) 
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (or (< row 0) (< col 0) )
						(setf viableFlag -1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (+ currentSpace 9) ) ) )
				)
				
			
			)
		
		)
	
	)
)
)