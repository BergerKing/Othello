(defun startState ()
(let (start)

	(setf start '( - - - - - - - - - - - - - - - - - - - - - - - - - - - W B - - - - - - B W - - - - - - - - - - - - - - - - - - - - - - - - - - -) )
	;(setf start '( - - - - - - - - - - - - - B - - - - - B - W - B - - - - W W W - - - - B W - W B - - - - W W W - - - - B - B - B - - - - - - - -) )
	;(setf start '( - - - - - W - - - - - - - W - - - - - B - W - B - - - - W W W - - - - B W - W B - - - - W W W - - - - B - B - B - - - - - - - -) )
	
)
)


(defun printState (state)
(let (row count)

	(setf row 0)
	(setf count 0)
	(format t "~%   1 2 3 4 5 6 7 8")
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





(defun flipTiles (position player move)
	(let ( flips totalFlips currentPlayer next currentSpace foundSpace viableFlag row col)
		(setf flips '())
		(setf totalFlips '())
		
		(when (equal player "White")
			(setf currentPlayer 'W)
			(setf next 'B)
		)
		(when (equal player "Black")
			(setf currentPlayer 'B)
			(setf next 'W)
		)
		
		(setf currentSpace move)
		(format t "current ~s " (nth (- currentSpace 2) position ))
		(when (and (>= (+ currentSpace 1) 0) (equal (nth (+ currentSpace 1) position ) next) ) ; right
			(setf viableFlag 0)
			(setf flips '())
			(do ((foundSpace (+ currentSpace 1) (incf foundSpace) ) (row (floor (+ currentSpace 1) 8) (incf row) ) (col (mod (+ currentSpace 1) 8) (incf col) ) )
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
				(format t "in loop  ~s~%" (nth foundSpace position ))
				(format t "when check  ~s ~s~%" (nth foundSpace position) currentPlayer)
				(when (> col 7)
					(setf viableFlag -1)
					(setf flips '())
					(format t "bad1 ~s ~%" flips)
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(format t "complete ~s ~s" flips currentPlayer)
					(setf totalFlips (append totalFlips flips) ); check append call !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
					(format t "bad2 ~s ~%" flips)
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
					(format t "flip ~s ~%" flips)
				
				)
			)
			
			#|(when (equal viableFlag 1)
				(setf moves (nconc moves (list (- currentSpace 1) ) ) )
			)|#
			
		
		)
		
		(when (and (>= (- currentSpace 1) 0) (equal (nth (- currentSpace 1) position ) next) ); left
			(setf viableFlag 0)
			(setf flips '())
			(format t "broke left ~%")
			(do ((foundSpace (- currentSpace 1) (1- foundSpace) ) (row (floor (- currentSpace 1) 8) (1- row) ) (col (mod (- currentSpace 1) 8) (1- col) ) )
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (< col 0)
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(format t "complete ~s ~s" flips currentPlayer)
					(setf totalFlips (append totalFlips flips) ); check append call !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
					(format t "flip ~s ~%" flips)
				)
			)
		
		)
		
		(when (and (>= (- currentSpace 8) 0) (equal (nth (- currentSpace 8) position) next) ) ; up
			(setf viableFlag 0)
			(setf flips ())
			(format t "~% broke up ~%")
			(do ((foundSpace (- currentSpace 8) (- foundSpace 8) ) (row (floor (- currentSpace 8) 8) (1- row) ) (col (mod (- currentSpace 8) 8) (incf col) ) ) 
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (< row 0)
					(setf viableFlag -1)
					(format t "top reached ~%")
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(format t "complete ~s ~s" flips currentPlayer)
					(setf totalFlips (append totalFlips flips) ); check append call !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(format t "wrong up")
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
					(format t "flip ~s ~%" flips)
				
				
				)
			)

		)
		
		(when (and (>= (+ currentSpace 8) 0) (equal (nth (+ currentSpace 8) position) next) ) ; down
			(setf viableFlag 0)
			(setf flips '())
			(do ((foundSpace (+ currentSpace 8) (+ foundSpace 8) ) (row (floor (+ currentSpace 8) 8) (incf row) ) (col (mod (+ currentSpace 8) 8) (incf col) ) ) 
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (> row 7)
					(setf viableFlag -1)
					(setf flips '())
				)
				(format t "broke down ~s ~s ~%" foundSpace row)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(format t "complete ~s ~s" flips currentPlayer)
					(setf totalFlips (append totalFlips flips) ); check append call !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				) 
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
					(format t "flip ~s ~%" flips)
				
				
				)
			)
		)
		
		(when (and (>= (- currentSpace 9) 0) (equal (nth (- currentSpace 9) position) next) ) ; up and left
			(setf viableFlag 0)
			(setf flips '())
			(do ((foundSpace (- currentSpace 9) (- foundSpace 9) ) (row (floor (- currentSpace 9) 8) (1- row) ) (col (mod (- currentSpace 9) 8) (1- col) ) ) 
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (or (< row 0) (< col 0) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(format t "complete ~s ~s" flips currentPlayer)
					(setf totalFlips (append totalFlips flips) ); check append call !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
					(format t "flip ~s ~%" flips)
				
				)
			)
		
		)
		
		(when (and (>= (- currentSpace 7) 0) (equal (nth (- currentSpace 7) position) next) ) ; up and right
			(setf viableFlag 0)
			(setf flips '())
			(do ((foundSpace (- currentSpace 7) (- foundSpace 7) ) (row (floor (- currentSpace 7) 8) (1- row) ) (col (mod (- currentSpace 7) 8) (incf col) ) ) 
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (or (< row 0) (> col 7) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(format t "complete ~s ~s" flips currentPlayer)
					(setf totalFlips (append totalFlips flips) ); check append call !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
					(format t "flip ~s ~%" flips)
				)
			)
		)
		
		(when (and (>= (+ currentSpace 7) 0) (equal (nth (+ currentSpace 7) position) next) ) ; down and left
			(setf viableFlag 0)
			(setf flips '())
			(do ((foundSpace (+ currentSpace 7) (+ foundSpace 7) ) (row (floor (+ currentSpace 7) 8) (incf row) ) (col (mod (+ currentSpace 7) 8) (1- col) ) ) 
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (or (> row 7) (< col 0) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(format t "complete ~s ~s" flips currentPlayer)
					(setf totalFlips (append totalFlips flips) ); check append call !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
					(format t "flip ~s ~%" flips)
				
				
				)
			)
		)
		
		(when (and (>= (+ currentSpace 9) 0) (equal (nth ( + currentSpace 9) position) next) ) ; down and right
			(setf viableFlag 0)
			(setf flips '())
			(do ((foundSpace (+ currentSpace 9) (+ foundSpace 9) ) (row (floor (+ currentSpace 9) 8) (1- row) ) (col (mod (+ currentSpace 9) 8) (incf col) ) ) 
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (or (> row 7) (> col 7) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(format t "complete ~s ~s" flips currentPlayer)
					(setf totalFlips (append totalFlips flips) ); check append call !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
					(format t "flip ~s ~%" flips)
				)					
			)
			
		)
		
		(dolist (index totalFlips)
		
			(setf (nth index position) currentPlayer)
		)
		
		(return-from flipTiles position)
	)
)


