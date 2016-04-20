#|
 | Function: hueristics
 |
 | Description:
 | This function collects the various hueristics values
 | and adds them together. This then returns the value to the minimax function
 |
 |
 | Parameters:
 | state - the current othello board state
 | maxPlayer - The minimax's max player for calculations
 |
 |
 |#
(defun hueristics (state maxPlayer)
	(let (coin maxCoins coinWeight mobile corn near final)
		
		; get coin hueristic values in a two value list
		(setf coin (coinParity state maxPlayer) )
		; get maxs number of coins
		(setf maxCoins (* (car coin) 10) )
		; coin weights are returned in the cadr of the list
		(setf coinWeight (* (cadr coin) 10) )
		; find mobility - the number of possible moves
		(setf mobile (* (mobility state maxPlayer) 79) )
		; check the corners
		(setf corn (* (corners state maxPlayer) 802) )
		; check the spaces near the corners to prevent bad moves
		(setf near (* (nearCorners state maxPlayer) 382) )
		
		; add all the weighted values together to get the total hueristic value
		(setf final (+ (+ (+ (+ maxCoins mobile) corn) near) coinWeight) )
	)
)

#|
 | Function: coinParity
 |
 | Description:
 | This function counts the number of tiles each player has. For each of these tiles 
 | a weight value is also calculated based on board weights. These two values are then
 | returned in a list to the hueristic function.
 |
 | Parameters:
 | state - the current othello board state
 | maxPlayer - the minimax maxPlayer
 |
 |#
(defun coinParity (state maxPlayer)
	(let ( (maxCoins 0) (minCoins 0) minPlayer boardWeights (diskWeight 0) (counter -1) )
	
	     ; the set of board weights
		(setf boardWeights '(20 -3 11 8 8 11 -3 20 -3 -7 -4 1 1 -4 -7 -3 11 -4 2 2 2 2 -4 11 8 1 2 -3 -3 2 1 8 8 1 2 -3 -3 2 1 8 11 -4 2 2 2 2 -4 11 -3 -7 -4 1 1 -4 -7 -3 20 -3 11 8 8 11 -3 20) )
		
		(when (equal maxPlayer 'W)
			(setf minPlayer 'B)
		)
		(when (equal maxPlayer 'B)
			(setf minPlayer 'W)
		)
		; step throuhg the list and count the number of coins for each player
		(dolist (index state)
			(incf counter)
			(when (equal index maxPlayer)
				(setf diskWeight (+ diskWeight (nth counter boardWeights) ) )
				(incf maxCoins)
			)
			(when (equal index minPlayer)
				(incf minCoins)
				(setf diskWeight (- diskWeight (nth counter boardWeights) ) )
			)
		)
		
		(when (> maxCoins minCoins)
			(return-from coinParity (list (/ (* maxCoins 100) (+ maxCoins minCoins) ) diskWeight ) )	
		)
		(when (< maxCoins minCoins)
			(return-from coinParity (list (/ (* minCoins -100) (+ maxCoins minCoins) ) diskWeight ) )
		)
		; if the two players have the same number of coins
		(return-from coinParity (list 0 diskWeight) )
	)
)


#|
 | Function: mobility
 |
 | Description:
 | This function counts the number of possible moves each player has open to them 
 | on the current board. This value is then returned to the hueristic function.
 |
 | Parameters:
 | state - the current othello board state
 | maxPlayer - the minimax maxPlayer
 |
 |#
(defun mobility (state maxPlayer)
	(let (minPlayer maxMoves minMoves corner)
		(when (equal maxPlayer 'W)
			(setf minPlayer 'B)
		)
		(when (equal maxPlayer 'B)
			(setf minPlayer 'W)
		)
		; generate a list of possible moves for each player
		(setf maxMoves (move-generator state maxPlayer) )
		
		(setf minMoves (move-generator state minPlayer) )
		
		; and decide return statement based on which set of possible moves is longer
		(when (> (length maxMoves) (length minMoves) )

			(return-from mobility (/ (* (length maxMoves) 100) (+ (length maxMoves) (length minMoves) ) ) )	
		)
		(when (< (length maxMoves) (length minMoves) )
			(return-from mobility (/ (* (length minMoves) -100) (+ (length maxMoves) (length minMoves) ) ) )
		)
		(return-from mobility 0)	
		
	)
)


#|
 | Function: corners
 |
 | Description:
 | This function counts the number of corners each player has open to them 
 | on the current board. This value is then returned to the hueristic function.
 |
 | Parameters:
 | state - the current othello board state
 | maxPlayer - the minimax maxPlayer
 |
 |#
(defun corners (state maxPlayer)
	(let (minPlayer (maxCorners 0) (minCorners 0) )
		(when (equal maxPlayer 'W)
			(setf minPlayer 'B)
		)
		(when (equal maxPlayer 'B)
			(setf minPlayer 'W)
		)
		
		; check to see if a player holds each corner
		(cond
			( (equal (nth 0 state) maxPlayer)
				(incf maxCorners)
			)
			( (equal (nth 0 state) minPlayer)
				(incf minCorners)
			)
		)
		(cond
			( (equal (nth 7 state) maxPlayer)
				(incf maxCorners)
			)
			( (equal (nth 7 state) minPlayer)
				(incf minCorners)
			)
		)
		(cond
			( (equal (nth 56 state) maxPlayer)
				(incf maxCorners)
			)
			( (equal (nth 56 state) minPlayer)
				(incf minCorners)
			)
		)
		(cond
			( (equal (nth 63 state) maxPlayer)
				(incf maxCorners)
			)
			( (equal (nth 63 state) minPlayer)
				(incf minCorners)
			)
		)
		
		(when (equal (+ maxCorners minCorners) 0)
			(return-from corners 0)
		)
		
		; return the max players weighted corner value
		(return-from corners (* (- maxCorners minCorners) 25) )	
		
	)
)

#|
 | Function: nearCorners
 |
 | Description:
 | This function checks to see if the possible move for the max player's
 | move results in taking one of three spaces around each corner. These
 | space usually result in corners for the min player and are therefore
 | negitivly weight in the hueristic calculations.
 |
 | Parameters:
 | state - the current othello board state
 | maxPlayer - the minimax maxPlayer
 |
 |#
(defun nearCorners (state maxPlayer)
	(let (minPlayer (maxCorners 0) (minCorners 0) )
		(when (equal maxPlayer 'W)
			(setf minPlayer 'B)
		)
		(when (equal maxPlayer 'B)
			(setf minPlayer 'W)
		)
		
		(when (equal (nth 0 state) '-)
			(cond
				( (equal (nth 1 state) maxPlayer)
					(incf maxCorners)
				)
				( (equal (nth 1 state) minPlayer)
					(incf minCorners)
				)
			)
			(cond
				( (equal (nth 8 state) maxPlayer)
					(incf maxCorners)
				)
				( (equal (nth 8 state) minPlayer)
					(incf minCorners)
				)
			)
			(cond
				( (equal (nth 9 state) maxPlayer)
					(incf maxCorners)
				)
				( (equal (nth 9 state) minPlayer)
					(incf minCorners)
				)
			)
		)
		(when (equal (nth 7 state) '-)
			(cond
				( (equal (nth 6 state) maxPlayer)
					(incf maxCorners)
				)
				( (equal (nth 6 state) minPlayer)
					(incf minCorners)
				)
			)
			(cond
				( (equal (nth 15 state) maxPlayer)
					(incf maxCorners)
				)
				( (equal (nth 15 state) minPlayer)
					(incf minCorners)
				)
			)
			(cond
				( (equal (nth 14 state) maxPlayer)
					(incf maxCorners)
				)
				( (equal (nth 14 state) minPlayer)
					(incf minCorners)
				)
			)
		)
		(when (equal (nth 56 state) '-)
			(cond
				( (equal (nth 57 state) maxPlayer)
					(incf maxCorners)
				)
				( (equal (nth 57 state) minPlayer)
					(incf minCorners)
				)
			)
			(cond
				( (equal (nth 48 state) maxPlayer)
					(incf maxCorners)
				)
				( (equal (nth 48 state) minPlayer)
					(incf minCorners)
				)
			)
			(cond
				( (equal (nth 49 state) maxPlayer)
					(incf maxCorners)
				)
				( (equal (nth 49 state) minPlayer)
					(incf minCorners)
				)
			)
		)
		(when (equal (nth 63 state) '-)
			(cond
				( (equal (nth 62 state) maxPlayer)
					(incf maxCorners)
				)
				( (equal (nth 62 state) minPlayer)
					(incf minCorners)
				)
			)
			(cond
				( (equal (nth 55 state) maxPlayer)
					(incf maxCorners)
				)
				( (equal (nth 55 state) minPlayer)
					(incf minCorners)
				)
			)
			(cond
				( (equal (nth 54 state) maxPlayer)
					(incf maxCorners)
				)
				( (equal (nth 54 state) minPlayer)
					(incf minCorners)
				)
			)
		)
		
		
		(when (equal (+ maxCorners minCorners) 0)
			(return-from nearCorners 0)
		)
		
		(return-from nearCorners (* (- maxCorners minCorners) -12.5) )	
		
	)
	
)
