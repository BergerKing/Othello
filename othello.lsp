; Node structure: stores state, parent, and depth.
(defstruct node state parent minMaxVal)

( defun checkPlayer ( player )
    ( cond 
        ;If exactly one clisp argument given
        ( ( equal player "Black" )
            
        )
		( ( equal player "White" )
            
        )
        
        ;else
        ( t
            ;Required command line argument missing, print usage statement
            (format t "Please Enter Black or White as player")
        )
    )
)

(defun othello ( &optional ( player nil ) )
(let (first)
	( load 'utilities)

	(cond
		; just typed (othello)
		((equal player nil)
			(format t "Would you like to move first [y/n]? ")
			
			(setf first (read-line)) ; get the user input
			(format t "~s ~%" first)
			(when (equal first "y")
				(format t "OK! You will be playing Black. When asked for your move, please enter the row 
				and column in which you would like to place a Black stone. Remember, you must 
				outflank at least one White stone, or forfeit your move." player player)
			)
			(when (equal first "n")
				(format t "OK! You will be playing White. When asked for your move, please enter the row 
				and column in which you would like to place a White stone. Remember, you must 
				outflank at least one Black stone, or forfeit your move." player player)
			)
		)
		
		;typed (othello [player]) or cmdline
		(t
			(checkPlayer player)
			(format t "Would you like to move first [y/n]? ")
			
			(setf first (read-line)) ; get the user input
			(format t "~s ~%" first)
			(when (equal first "y")
				(format t "OK! You will be playing ~s. When asked for your move, please enter the row 
				and column in which you would like to place a ~s stone. Remember, you must 
				outflank at least one of the other color stone, or forfeit your move." player player)
			)
			(when (equal first "n")
				(format t "OK! You will be playing ~s. When asked for your move, please enter the row 
				and column in which you would like to place a ~s stone. Remember, you must 
				outflank at least one of the other color stone, or forfeit your move." player player)
			)
		)
	)


)
)

;script for command line run
( load 'readStart )
(cond
	((= (length *ARGS*) 1)
		( othello ( getPlayer *ARGS* ) )
	)
	(t
		(othello)
	)
)