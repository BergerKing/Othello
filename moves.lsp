(load 'minimax.lsp)
(load 'placement.lsp)
;this needs 
(defun make-move (position player ply)
	(let (x)
		(setf x (minimax position ply))
		(cadr x)
	)
)


(defun humanmove(validMoves player state)
	(let ((coords '(0 0)))
		(do () ( (not (equal (validatePlayerChoice coords validMoves) nil) ) )
			(cond 
				((equal validMoves nil) 
					(format t "You have no moves currently")
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
	)
)


