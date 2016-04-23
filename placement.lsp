
#| Name: placeStone
	Author: Benjamin Kaiser
	Description: This function takes a list of coords, the state trying to be
		modified, the current player that is placing and whether or not this is a
		computer player.  If the move is a human move, the move is validated before
		being passed to the flip tiles function.  
	Parameters:
		coords: the coords in a list of length 2
		state: the state trying to be placed into
		player: B or W depending on what player is playing
		computer: whether or not this is a computer player
	Returns:
		The modified state.  
|#
(defun placeStone (coords state player &optional computer)
	(let (tempPos)
		(cond
			((equal nil computer) (validatePlayerChoice coords state player) )
			(t (setf (nth coords state) player) (setf tempPos (coords)))
		)
		(flipTiles position player tempPos)
	)
)

#|	Name: coordinateConversion
	Author: Benjamin Kaiser
	Description: This function takes a list of length 2 which contains the
		row and column coordinates of a board position and converts it into a
		single value from 0 to 63 for a board position.
	Parameters: 
		coords: a list of length 2 to be converted
	Returns: a scalar value from 0 to 63
|#
(defun coordinateConversion (coords)
	(+ (* 8 (1- (car coords))) (1- (cadr coords)))
)

#|	Name: reverseConvert
	Author: Benjamin Kaiser
	Description: This function takes a scalar between 0 and 63 and converts
	it into a list containing the row and column coordinates of a board position.
	Parameters:
		index: the scalar value to be converted into (row col) format
	Returns:
		A list of length 2 which has the row-col format
|#
(defun reverseConvert(index)
	(let (coords '())
		(setf coords (push (1+ (floor index 8) ) coords) )
		(setf coords (push (1+ (- index (* 8 (floor index 8))) ) coords) )
		(reverse coords)
	)
)

#|	Name: validatePlayerChoice
	Author: Benjamin Kaiser
	Description: This function takes a list of coordinates and a list of valid successors.
		It then does validation on the move that the player entered and checks to see if the move was valid.
		If it is, then it returns the coords.  if not, then it prints that we need to try again.  
	Parameters:
		coords: the move in row-col format.  
		validSuccessors: the list of valid moves in row-col format
	Returns:	
|#
(defun validatePlayerChoice (coords validSuccessors)
	(let (tempPos)
		;convert coords
		(setf tempPos (coordinateConversion coords))
		;find converted coords in successor list
		(cond
			( (< tempPos 0)
				nil
			)
			((equal  (member tempPos validSuccessors) nil) (format t "Invalid move. Please try again ~%"))
			(t 
				coords
			)
		)
	)
)
