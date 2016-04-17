(load 'moves.lsp)
; Node structure: stores state, parent, moveLocation and depth.
(defstruct node state parent moveLocation minMaxVal)

(defparameter *WCanMove* 0) ; can white make a move
(defparameter *BCanMove* 0) ; can black move flag
(defparameter *MovesMade* 4) ; counter for the amount of tiles on the board 

(defun othello ( &optional ( player nil ) )
(let (firstplayer state vaildMoves)
	( load 'utilities.lsp)

	(cond
		; just typed (othello)
		((equal player nil)
			(setf player 'B)
			(format t "Would you like to move first [y/n]? ")
			
			(setf firstplayer (read-line)) ; get the user input
			;(format t "~s ~%" firstplayer)
			(when (equal firstplayer "y")
				(format t "OK! You will be playing Black. When asked for your move, please enter the row 
				and column in which you would like to place a Black stone. Remember, you must 
				outflank at least one White stone, or forfeit your move.")
			)
			(when (equal firstplayer "n")
				(format t "OK! You will be playing White. When asked for your move, please enter the row 
				and column in which you would like to place a White stone. Remember, you must 
				outflank at least one Black stone, or forfeit your move.")
			)
		)
		
		;typed (othello [player]) or cmdline
		(t
			(checkPlayer player)
			(format t "Would you like to move first [y/n]? ")
			
			(setf firstplayer (read-line)) ; get the user input
			(when (equal firstplayer "y")
				(format t "OK! You will be playing ~s. When asked for your move, please enter the row 
				and column in which you would like to place a ~s stone. Remember, you must 
				outflank at least one of the other color stone, or forfeit your move." player player)
				(when (equal player "White") 
					(setf player 'W)
				
				)
				(when (equal player "Black") 
					(setf player 'B)
				)
			)
			(when (equal firstplayer "n")
				(format t "OK! You will be playing ~s. When asked for your move, please enter the row 
				and column in which you would like to place a ~s stone. Remember, you must 
				outflank at least one of the other color stone, or forfeit your move." player player)
				(when (equal player "White") 
					(setf player 'B)
				
				)
				(when (equal player "Black") 
					(setf player 'W)
				)
				
			)
		)
	)
	
	(setf state (startState) ) ; get start state
	; game loop
	(loop while (and (< *MovesMade* 63) (or (equal *WCanMove* 0) (equal *BCanMove* 0) ) )  do
	
		(printState state) ; print after each move
		(setf validMoves (move-generator state player) )
		(setf state (humanMove validMoves player state) )
		(setf player (switchPlayer player))
		(printState state)
		(setf state (make-move state player 4))
		(setf player (switchPlayer player))
		
	)
	(format t "~%GAME OVER ~%")
	(printState state)
	(findWinner state)
	
)
)

;script for command line run
( load 'readStart.lsp )
(cond
	((= (length *ARGS*) 1)
		( othello ( getPlayer *ARGS* ) )
	)
	(t
		(othello)
	)
)
