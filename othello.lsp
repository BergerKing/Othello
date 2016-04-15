; Node structure: stores state, parent, moveLocation and depth.
(defstruct node state parent moveLocation minMaxVal)

(defvar *WCanMove* 0) ; can white make a move
(defvar *BCanMove* 0) ; can black move flag
(defvar *MovesMade* 4) ; counter for the amount of tiles on the board 

(defun othello ( &optional ( player nil ) )
(let (first state vaildMoves)
	( load 'utilities.lsp)

	(cond
		; just typed (othello)
		((equal player nil)
			(setf player 'B)
			(format t "Would you like to move first [y/n]? ")
			
			(setf first (read-line)) ; get the user input
			(format t "~s ~%" first)
			(when (equal first "y")
				(format t "OK! You will be playing Black. When asked for your move, please enter the row 
				and column in which you would like to place a Black stone. Remember, you must 
				outflank at least one White stone, or forfeit your move.")
			)
			(when (equal first "n")
				(format t "OK! You will be playing White. When asked for your move, please enter the row 
				and column in which you would like to place a White stone. Remember, you must 
				outflank at least one Black stone, or forfeit your move.")
			)
		)
		
		;typed (othello [player]) or cmdline
		(t
			(checkPlayer player)
			(format t "Would you like to move first [y/n]? ")
			
			(setf first (read-line)) ; get the user input
			(when (equal first "y")
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
			(when (equal first "n")
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
	(do ( (index 0 (incf foundSpace) )
			( (or (and (= *WCanMove* 1) (= *BCanMove* 1) ) (equal *MovesMade* 63 ) ) *MovesMade*) )
	
		(printState state) ; print after each move
		(setf validMoves (move-generator state player) )
		(setf state (humanMove vailidMoves player state) )
	)
	(format t "~%GAME OVER ~%")
	(printState state)
	(findWinner state)
	
)
)

;script for command line run
;( load 'readStart.lsp )
;(cond
;	((= (length *ARGS*) 1)
;		( othello ( getPlayer *ARGS* ) )
;	)
;	(t
;		(othello)
;	)
;)