 #|
 | Function: othello-init
 |
 | Description:
 | Prepares the othello program for tournament play
 |
 | Parameters:
 | None
 |
 |#
 (defun othello-init () 
 
 )

#|
 | Function: getPlayer
 |
 | Description:
 | Gets the selected player from the command line
 | and checks to make sure that the player is valid
 |
 | Parameters:
 | args - command line arguments
 |
 |#
( defun getPlayer ( args )
    ( cond 
        ;If exactly one command line argument given
        ( ( = 1 ( length args ) )
            ;get player and turn
            (cond 
				((equal (car args) "Black") 
					(return-from getPlayer "Black")
				)
				((equal (car args) "White")
					(return-from getPlayer "White")
				
				)
				(t
					(format t "Usage: clisp othello.lsp player (Player must be Black or White)~%")
					(format t "Please Enter Black or White as the player~%")
				)
			)
        )
        
        ;If not exactly one command line argument given
        ( t
            ;Required command line argument missing, print usage statement
            (format t "Usage: clisp othello.lsp player (Player must be Black or White)~%")
        )
    )
)

#|
 | Function: startState
 |
 | Description:
 | This function returns the start state of the othello board
 | 
 |
 | Parameters:
 | None
 |
 |#
(defun startState ()
	(let (start)

	(setf start (copy-list '( - - - - - - - - - - - - - - - - - - - - - - - - - - - W B - - - - - - B W - - - - - - - - - - - - - - - - - - - - - - - - - - -) ) )
	;(setf start '( - - - - - - - - - - - - - B - - - - - B - W - B - - - - W W W - - - - B W - W B - - - - W W W - - - - B - B - B - - - - - - - -) )
	;(setf start '( - - - - - W - - - - - - - W - - - - - B - W - B - - - - W W W - - - - B W - W B - - - - W W W - - - - B - B - B - - - - - - - -) )
	
	)
)