#|
 | Function: checkPlayer
 |
 | Description:
 | This function checks to make sure the player color was entered in the correct
 | way.
 | 
 |
 | Parameters:
 | player - the string the user entered as the player
 |
 |#
(defun checkPlayer ( player )
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


#|
 | Function: printState
 |
 | Description:
 | This function prints out the current othello board state to the terminal 
 | 
 |
 | Parameters:
 | state - the current othello board state
 | 
 |
 |#
(defun printState (state)
(let (row count)

	(setf row 0)
	(setf count 0)
	(format t "~%   1 2 3 4 5 6 7 8")
	(dolist (index state)
		(when (equal (mod count 8) 0)
			(incf row)
			(format t "~% ~s " row)
		)
		(format t "~s " index)
		(incf count)
	
	)
	(format t "~%")

)
)


#|
 | Function: findWinner
 |
 | Description:
 | This function goes through the final board and counts
 | the number of tiles of each color to determine the
 | winner of the game.
 |
 | Parameters:
 | state - the current othello board state
 |
 |#
(defun findWinner (state)
	(let ( (white 0) (black 0) )
		
		; step through the list and count tiles
		(dolist (index state)
			(when (equal index 'B)
				(incf black)
			)
			(when (equal index 'W)
				(incf white)
			)
		)
		(format t "Black: ~s White: ~s ~%" black white)
		
		(cond
			((> black white)
				(format t "Black Wins!!!")
			)
			((> white black)
				(format t "White Wins!!!")
			)
			((= white black)
				(format t "Tie Game")
			)
		)
		
	)
)

#|
 | Function: move-generator
 |
 | Description:
 | This function goes through the board and determine a
 | list of possible moves that can be made for the given player
 | which is then returned
 |
 | Parameters:
 | position - the current othello board state
 | player - the current player looking for moves
 |
 |#
(defun move-generator (position player)
(let ( moves currentPlayer next currentSpace foundSpace viableFlag row col
		;(top '(0 1 2 3 4 5 6 7) )
		(right '(7 15 23 31 39 47 55 63) )
		;(bottom '(56 57 58 59 60 61 62 63) )
		(left '(0 8 16 24 32 40 48 56) )
		
	)
	(setf moves ())
	
	; retrive the current and next player to move
	(when (equal player "White")
		(setf currentPlayer 'W)
		(setf next 'B)	
	)
	(when (equal player "Black")
		(setf currentPlayer 'B)
		(setf next 'W)
	)
	(when (equal player 'W)
		(setf currentPlayer 'W)
		(setf next 'B)	
	)
	(when (equal player 'B)
		(setf currentPlayer 'B)
		(setf next 'W)
	)
	
	;set space being looked at
	(setf currentSpace -1)
	
	; go through the list and look for an occurence of the next players tile
	; when a tile is found check the tiles surrounding that tile looking for
	; an occurence of the current players tile. If one if found backtrack through
	; the board in that direction looking for another player tile. If one is found place
	; it in the vaild move list. A move is not vaild if a blank tile is found or if 
	; the backtrace reaches the end of the board state.
	(dolist (index position)
		(incf currentSpace)
	
		(when (equal index next)
			(when (and (>= (- currentSpace 1) 0) (equal (nth (- currentSpace 1) position ) '-) ) ; left side
				(setf viableFlag 0) ; flag to end do loop
				; backtrace right by adding one
				(do ((foundSpace currentSpace (incf foundSpace) ) (row (floor currentSpace 8) (incf row) ) (col (mod currentSpace 8) (incf col) ) )
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
					;check if off board
					(when (> col 7)
						(setf viableFlag -1)
					)
					; check for current player tile 					
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					;check if blank
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				; if move was found add to viable move lisr
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (- currentSpace 1) ) ) )
				)
				
			
			)
			; check right space and backtrace left
			(when (and (>= (+ currentSpace 1) 0) (equal (nth (+ currentSpace 1) position ) '-) ); right
				(setf viableFlag 0)
				; backtrace left by subtracting one each time
				(do ((foundSpace currentSpace (1- foundSpace) ) (row (floor currentSpace 8) (1- row) ) (col (mod currentSpace 8) (1- col) ) )
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (< col 0)
						(setf viableFlag -1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (+ currentSpace 1) ) ) )
				)
			
			)
			; check up space and backtrace down
			(when (and (>= (- currentSpace 8) 0) (equal (nth ( - currentSpace 8) position) '-) ) ; up
				(setf viableFlag 0)
				; move down by adding 8
				(do ((foundSpace currentSpace (+ foundSpace 8) ) (row (floor currentSpace 8) (incf row) ) (col (mod currentSpace 8) (incf col) ) ) 
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (> row 7)
						(setf viableFlag -1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					)
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (- currentSpace 8) ) ) )
				)
				
			
			)
			; check down and backtrace up
			(when (and (>= (+ currentSpace 8) 0) (equal (nth (+ currentSpace 8) position) '-) ) ; down
				(setf viableFlag 0)
				; move up by subtracting 8
				(do ((foundSpace currentSpace (- foundSpace 8) ) (row (floor currentSpace 8) (1- row) ) (col (mod currentSpace 8) (1- col) ) ) 
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (< row 0)
						(setf viableFlag -1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (+ currentSpace 8) ) ) )
				)
				
			
			)
			; check upper left corner but backtrace down and right
			(when (and (>= (- currentSpace 9) 0) (equal (nth (- currentSpace 9) position) '-) ) ; up and left
				(setf viableFlag 0)
				; in the list the down and right spaces are always + 9
				(do ((foundSpace currentSpace (+ foundSpace 9) ) (row (floor currentSpace 8) (incf row) ) (col (mod currentSpace 8) (incf col) ) ) 
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (member currentSpace left) 
						(return)
					)
						
					(when (or (> row 7) (> col 7) )
						(setf viableFlag -1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (- currentSpace 9) ) ) )
				)
			
			)
			; check upper right corner but backtrace down and left
			(when (and (>= (- currentSpace 7) 0) (equal (nth (- currentSpace 7) position) '-) ) ; up and right
				(setf viableFlag 0)
				; backtrace done by adding 7
				(do ((foundSpace currentSpace (+ foundSpace 7) ) (row (floor currentSpace 8) (incf row) ) (col (mod currentSpace 8) (1- col) ) ) 
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (member currentSpace right) 
						(return)
					)
					
					; make sure to check the row and column for off board in all diaganol backtraces 					
					(when (or (> row 7) (< col 0) )
						(setf viableFlag -1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (- currentSpace 7) ) ) )
				)
			
			)
			; check lower left corner but backtrace up and right
			(when (and (>= (+ currentSpace 7) 0) (equal (nth (+ currentSpace 7) position) '-) ) ; down and left
				(setf viableFlag 0)
				; subtracts seven each backtrace
				(do ((foundSpace currentSpace (- foundSpace 7) ) (row (floor currentSpace 8) (1- row) ) (col (mod currentSpace 8) (incf col) ) ) 
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (member currentSpace left) 
						(return)
					)
					
					(when (or (< row 0) (> col 7) )
						(setf viableFlag -1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (+ currentSpace 7) ) ) )
				)
				
			)
			; check lower right corner but backtrace up and left
			(when (and (>= (+ currentSpace 9) 0) (equal (nth ( + currentSpace 9) position) '-) ) ; down and right
				(setf viableFlag 0)
				(do ((foundSpace currentSpace (- foundSpace 9) ) (row (floor currentSpace 8) (1- row) ) (col (mod currentSpace 8) (1- col) ) ) 
						( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
						
					(when (member currentSpace right) 
						(return)
					)
						
					(when (or (< row 0) (< col 0) )
						(setf viableFlag -1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
						(setf viableFlag 1)
					)
					(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
						(setf viableFlag -1)
					) 
				)
				
				(when (equal viableFlag 1)
					(setf moves (nconc moves (list (+ currentSpace 9) ) ) )
				)
				
			
			)
		
		)
	
	)
	; one all moves have been checked return the list of possible moves
	(setf moves (remove-duplicates moves))
	(return-from move-generator moves)
	
)
)




#|
 | Function: flipTiles
 |
 | Description:
 | This function takes the move made by the current player and
 | flips all the tiles that the player would capture
 |
 | Parameters:
 | position - the current othello board state
 | player - the current player looking for moves
 | move - the spot on the board the player intends to go
 |
 |#
(defun flipTiles (position player move)
	(let ( flips totalFlips currentPlayer next currentSpace foundSpace viableFlag row col)
		(setf flips '())
		(setf totalFlips '())
		
		;check player
		(when (equal player "White")
			(setf currentPlayer 'W)
			(setf next 'B)
		)
		(when (equal player "Black")
			(setf currentPlayer 'B)
			(setf next 'W)
		)
		(when (equal player 'W)
			(setf currentPlayer 'W)
			(setf next 'B)	
		)
		(when (equal player 'B)
			(setf currentPlayer 'B)
			(setf next 'W)
		)
		
		(setf currentSpace move)
		; look right for flipable tiles
		(when (and (>= (+ currentSpace 1) 0) (equal (nth (+ currentSpace 1) position ) next) ) ; right
			(setf viableFlag 0)
			(setf flips '())
			(do ((foundSpace (+ currentSpace 1) (incf foundSpace) ) (row (floor (+ currentSpace 1) 8) (incf row) ) (col (mod (+ currentSpace 1) 8) (incf col) ) )
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
				; check if off board
				(when (> col 7)
					(setf viableFlag -1)
					(setf flips '())
				)
				; check for current player tile to end flips
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					; append directional flips to total flips
					(setf totalFlips (append totalFlips flips) )
				)
				; check for blank if found dump flips list
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				)
				; if a flip is good add to flips list
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
					
				
				)
			)
			
		
		)
		
		;check left
		(when (and (>= (- currentSpace 1) 0) (equal (nth (- currentSpace 1) position ) next) ); left
			(setf viableFlag 0)
			(setf flips '())
			(do ((foundSpace (- currentSpace 1) (1- foundSpace) ) (row (floor (- currentSpace 1) 8) (1- row) ) (col (mod (- currentSpace 1) 8) (1- col) ) )
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (< col 0)
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(setf totalFlips (append totalFlips flips) )
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
				)
			)
		
		)
		;check up
		(when (and (>= (- currentSpace 8) 0) (equal (nth (- currentSpace 8) position) next) ) ; up
			(setf viableFlag 0)
			(setf flips ())

			(do ((foundSpace (- currentSpace 8) (- foundSpace 8) ) (row (floor (- currentSpace 8) 8) (1- row) ) (col (mod (- currentSpace 8) 8) (incf col) ) ) 
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (< row 0)
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(setf totalFlips (append totalFlips flips) )
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
						
				)
			)

		)
		; check down
		(when (and (>= (+ currentSpace 8) 0) (equal (nth (+ currentSpace 8) position) next) ) ; down
			(setf viableFlag 0)
			(setf flips '())
			(do ((foundSpace (+ currentSpace 8) (+ foundSpace 8) ) (row (floor (+ currentSpace 8) 8) (incf row) ) (col (mod (+ currentSpace 8) 8) (incf col) ) ) 
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (> row 7)
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(setf totalFlips (append totalFlips flips) )
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				) 
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
				
				
				)
			)
		)
		; check upper left
		(when (and (>= (- currentSpace 9) 0) (equal (nth (- currentSpace 9) position) next) ) ; up and left
			(setf viableFlag 0)
			(setf flips '())
			(do ((foundSpace (- currentSpace 9) (- foundSpace 9) ) (row (floor (- currentSpace 9) 8) (1- row) ) (col (mod (- currentSpace 9) 8) (1- col) ) ) 
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (or (< row 0) (< col 0) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(setf totalFlips (append totalFlips flips) )
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
				
				)
			)
		
		)
		; check uppper right
		(when (and (>= (- currentSpace 7) 0) (equal (nth (- currentSpace 7) position) next) ) ; up and right
			(setf viableFlag 0)
			(setf flips '())
			(do ((foundSpace (- currentSpace 7) (- foundSpace 7) ) (row (floor (- currentSpace 7) 8) (1- row) ) (col (mod (- currentSpace 7) 8) (incf col) ) ) 
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (or (< row 0) (> col 7) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(setf totalFlips (append totalFlips flips) )
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
				)
			)
		)
		; check lower left
		(when (and (>= (+ currentSpace 7) 0) (equal (nth (+ currentSpace 7) position) next) ) ; down and left
			(setf viableFlag 0)
			(setf flips '())
			(do ((foundSpace (+ currentSpace 7) (+ foundSpace 7) ) (row (floor (+ currentSpace 7) 8) (incf row) ) (col (mod (+ currentSpace 7) 8) (1- col) ) ) 
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (or (> row 7) (< col 0) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(setf totalFlips (append totalFlips flips) )
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )			
					
				)
			)
		)
		; check lower right
		(when (and (>= (+ currentSpace 9) 0) (equal (nth ( + currentSpace 9) position) next) ) ; down and right
			(setf viableFlag 0)
			(setf flips '())
			(do ((foundSpace (+ currentSpace 9) (+ foundSpace 9) ) (row (floor (+ currentSpace 9) 8) (1- row) ) (col (mod (+ currentSpace 9) 8) (incf col) ) ) 
					( (or (= viableFlag 1) (= viableFlag -1) ) viableFlag)
					
				(when (or (> row 7) (> col 7) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) currentPlayer) )
					(setf viableFlag 1)
					(setf totalFlips (append totalFlips flips) )
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) '-) )
					(setf viableFlag -1)
					(setf flips '())
				)
				(when (and (equal viableFlag 0) (equal (nth foundSpace position) next) )
				
					(setf flips (append flips (list foundSpace) ) )
				)					
			)
			
		)
		
		; ones all directions are checked flip the tiles in
		; total flips to the current players color
		(dolist (index totalFlips)
		
			(setf (nth index position) currentPlayer)
		)
		
		(setf (nth move position) currentPlayer) ; add the stone that cause flips
												; to the game board
		
		; and return the new board state
		(return-from flipTiles position)
	)
)

(defun switchplayer (current)
	(let (newPlayer)
		(when (equal current 'W)
			(setf newPlayer 'B)	
		)
		(when (equal current 'B)
			(setf newPlayer 'W)
		)
		newPlayer
	)
)


