(defun placeStone (coords state player &optional computer)
	(let (tempPos)
		(cond
			((equal nil computer) (validatePlayerChoice coords state player) )
			(t (setf (nth coords state) player) (setf tempPos (coords))
		)
		(flipTiles position player tempPos)
	)
)

(defun coordinateConversion (coords)
	(8 * (1- (car coords)) + (1- (cadr coords)))
)

(defun validatePlayerChoice (coords state player)
	(let (validSuccessors tempPos)
		;will call generate successors
		(setf validSuccessors (move-generator state player))
		;convert coords
		(setf tempPos (coordinateConversion coords))
		;find converted coords in successor list
		(when (equal  (member validSuccesors tempPos) nil) (format t "Invalid move. Please try again") (humanMove))
	)
)
