
;this needs to be fleshed out
(defun make-move (position player ply)
	(let (x validMoves move)
		(setf validMoves (move-generator position player) )
		(when (equal validMoves nil)
			(format t "Computer has no moves currently")
			(when (equal player 'W)
				(setf *WCanMove* 1)	
			)
			(when (equal player 'B)
				(setf *BCanMove* 1)
			)
			(return-from make-move position)
			
		)
		
		(setf move (checkCorners validMoves) )
		(when (not (equal move nil) )
			(flipTiles position player move)
			(setf move (reverseConvert move) )
			(incf *MovesMade*)
			(format t "Here is my move: ~s ~%" move)
			(return-from make-move position)
		
		)

		(setf x (minimax position ply -10000000000 1000000000 player t))
		(incf *MovesMade*)
		(format t "Here is my move: ~s ~%" (reverseConvert *comMove*) )
		(cadr x)
	)
)

(defun humanmove(validMoves player state)
	(let ((tempcoord -1) (coords '(-1 -1)))
		(do () ( (not (equal (validatePlayerChoice coords validMoves) nil) ) )
			(cond 
				((equal validMoves nil) 
					(format t "You have no moves currently")
					(when (equal player 'W)
					    (setf *WCanMove* 1)	
					)
					(when (equal player 'B)
						(setf *BCanMove* 1)
					)
					(return-from humanmove state)
				)
				(t 
					(format t "Valid Moves: ")
					
					(dolist (index validMoves)
					
						(format t "~s " (reverseConvert index))
					)
					
					(format t "~%")
					
					(format t "Enter a move: ")
					(setf coords nil) 
					(dotimes (x 2)
						(setf tempcoord (read))
						(cond
							((not (numberp tempcoord)) (setf x -1) (setf coords nil) (format t "Not a number! ~% Try again: ~%") )
							(t (setf coords (append coords (list tempcoord) ) ) )
						)
					)
					(format t "coor ~s" coords)
					(when (or (< (first coords) 0) (< (second coords) 0) )
						(setf coords '(-1 -1) )
					)
				)
			)
	
		)
		(setf coords (coordinateConversion coords))
		(flipTiles state player coords)
		(incf *MovesMade*)
		state
	)
)

(defun checkCorners (moves)
	(let ( (flag nil) )
	
		(dolist (index moves)
			(cond
				( (equal index 0)
					(setf flag 0)
				)
				
				( (equal index 7)
					(setf flag 7)
				)
				
				( (equal index 56)
					(setf flag 56)
				)
				
				( (equal index 63)
					(setf flag 63)
				)
			)
		)
		flag
	)
)


