 (defun othello-init () 
 
 )

#|
 | Function: getPlayer
 |
 | Description:
 | Gets the selected player from the command line
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