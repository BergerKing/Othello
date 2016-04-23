#|
	Name: make-move
	Description: This is the function that performs the computer moves.
		It checks to see if the move currently has any moves and if it does,
		it then proceeds to evaluate each one of those moves.  It then picks
		one using minimax (with alpha-beta pruning), executes it, and tells
		the human which one it picked.  
	Parameters:
		position: the current state of the board
		player: the current player (computer-player)
		ply: how deep we want to search on this move
	Returns: the row-col representation of the chosen move.
|#
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

#|
	Name: humanmove
	Author: Benjamin Kaiser
	Description: This function is the function that handles human input.  
		It first checks to see if the player has any moves.  If it doesn't
		then it informs the user.  If it does, then it displayers a list
		of the valid moves.  It then prompts for the user to input a coordinate point.
		It does data validation to make sure the input are numbers so that the program
		does not crash.  
	Parameters:
		validMoves: a list of the valid moves for the given player given this state.
		player:  This is the player that the humman is currently playing as.
		state:  The current board state.  
	Returns:
		A modified board state after the player has moved.
|#
(defun humanmove(validMoves player state)
	(let (empty coord(tempcoord -1) (coords '(-1 -1)))
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
							((not (numberp tempcoord)) (setf x -1) (setf coords nil) (format t "Not a number! ~%") )
							(t (setf coords (append coords (list tempcoord) ) ) )
						)
					)
					(format t "coor ~s~%" coords)
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
#|
	Function: checkCorners
	Description: This function is used to see if a corner has been taken or
		could soon be taken.  It then figures into the heuristic to decide
		if it should take the corner if possible or if it can't to keep the
		opponent from taking the corner.  
|#
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


