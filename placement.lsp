
(load 'moves.lsp)

(defun placeStone (coords state player &optional computer)
	(let (tempPos)
		(cond
			;this not computer move is gonna need to change
			((equal nil computer) (validatePlayerChoice coords state player) )
			(t (setf (nth coords state) player) (setf tempPos (coords)))
		)
		(flipTiles position player tempPos)
	)
)

(defun coordinateConversion (coords)
	(+ (* 8 (1- (car coords))) (1- (cadr coords)))
)

(defun reverseConvert(index)
	(let (coords '())
		(setf coords (push (1+ (floor index 8)) coords))
		(setf coords (push (1+ (- index (* 8 (floor index 8)))) coords))
		(reverse coords)
	)
)

#|
(defun validatePlayerChoice (coords state player)
	(let (validSuccessors tempPos)
		;will call generate successors
		(setf validSuccessors (move-generator state player))
		;convert coords
		(setf tempPos (coordinateConversion coords))
		;find converted coords in successor list
		(when (equal  (member validSuccesors tempPos) nil) (format t "Invalid move. Please try again") (humanMove))
	)
)|#
