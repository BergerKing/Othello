(load 'alphabeta.lsp)
(load 'placement.lsp)
;this needs to be fleshed out
(defun make-move (position player ply)
	(let (x)
		(setf x (minimax position ply -10000000000 1000000000 player t))
		(cadr x)
	)
)


(defun humanmove(validMoves player state)
	(let ((coords '(0 0)))
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
					(format t "Enter a move: ")
					(setf coords nil) 
					(dotimes (x 2)
						(setf coords (append coords (list (read))))
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


