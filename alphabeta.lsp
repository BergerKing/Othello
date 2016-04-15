(load 'utilities.lsp)
(load 'readStart.lsp)
(load 'hueristics.lsp)
#|
                  ***** MINIMAX.LSP *****

Generalized recursive minimax routine.

Author: Dr. John M. Weiss
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (minimax position depth)
          where position is the position to be evaluated,
          and depth is the search depth (number of plys).

Returns:  (value path)
          where value is the backed-up value from evaluation of leaf nodes,
          and path is the path to the desired leaf node.

Functions called:

          (deepenough depth) -
              predicate that returns T if the current position has reached
              the desired search depth, NIL otherwise.

          (move-generator position) -
              generates successors to the position.

          (static position) -
              applies the static evaluation function to the position.

          Note: these functions may need additional arguments.
|#

(defun minimax (position depth alpha beta player maxFlag)

    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
    (if (or (deepenough depth) (null (move-generator position player)))
	;identified that this returns (nil nil)
        (list (hueristics position player) nil)

        ; otherwise, generate successors and run minimax recursively
        (let
            (
			
				;generate list of possible move position
				(moves (move-generator position player))
				
				
                ; generate list of sucessor positions
                (successors '() )
				
				;passed in list
				(start (copy-list position) )

                ; initialize current best path to nil
                (best-path nil)

                ; initialize current best score to negative infinity
                (best-score -100000000)

                ; other local variables
                succ-value
                succ-state
            )
			
			(dolist (index moves)
				(format t "this is where i die ~%")
				;(setf successors (append successors (list (flipTiles position player index) ) ) )
				(setf successors (append successors (list (make-node  :state (flipTiles position player index) 
                                    :parent start
									:moveLocation index
                                    :minMaxVal (hueristics position player)
										) )
								)
				)
				(format t "the parent ~s ~%" (node-parent (car (last successors) ) ) )
				(setf position (copy-list start) )
				(format t "successors: ~s index ~s ~%" successors index)
			)
			
			(when (equal maxFlag t)
					(format t " MAX ~%~%")
					(setf best-score -100000000)
					(dolist (successor successors)

						(when (equal player 'W)
							(setf next 'B)	
						)
						(when (equal player 'B)
							(setf next 'W)
						)
						; perform recursive DFS exploration of game tree
						(format t "alpha in ~s beta in ~s~%" alpha beta)
						(setf succ-value (minimax (node-state successor) (1- depth) alpha beta next nil) )
						(setf succ-state (second succ-value ) )
						(setf succ-value (first succ-value ) )
						(format t "secc alpha ~s ~s~%" succ-value alpha )
						(when (setf alpha (max succ-value alpha) ) 
							(format t "alpha changed ~s ~s ~s ~%" alpha successor succ-state)
							
						
						)
						(format t "beta alpha ~s ~s~%" beta alpha)
						(when (<= beta alpha)
							(format t "max pruned~%")
							(return) ;my question being what do we return from this?
						)
						(format t "alpha ~s  BS: ~s~%" alpha best-score)
						(when (> alpha best-score)
							;(setf best-path (append best-path (list (node-state successor) ) ) ) ;consing does things - bad things
							(setf best-path (node-parent succ-state) )
							(format t "best path updated: ~s  BP: ~s~%" successor best-path)
						
							(setf best-score alpha)
							(format t "max best score set: ~s ~%" best-score)
						)

					)
			)
			
			(when (equal maxFlag nil)
				(format t " MIN ~%~%")
				(setf best-score 100000000)
				(dolist (successor successors)

						(when (equal player 'W)
							(setf next 'B)	
						)
						(when (equal player 'B)
							(setf next 'W)
						)						
						; perform recursive DFS exploration of game tree
						(format t "beta in ~s  alpha in ~s~%" beta alpha)
						;(setf succ-value (first (minimax (node-state successor) (1- depth) alpha beta next t) ) )
						(setf succ-value (minimax (node-state successor) (1- depth) alpha beta next t) )
						(setf succ-state (second succ-value ) )
						(setf succ-value (first succ-value ) )
						(format t "secc beta ~s ~s~%" succ-value beta)
						(when (setf beta (min succ-value beta) )
						
							(format t "beta changed ~s ~s~%" beta successor)
						)
						(format t "alpha beta ~s ~s~%" alpha beta )
						(when (<= beta alpha)
							(format t "min pruned~%")
							(return) ;my question being what do we return from this?
						)
						(format t "beta ~s  BS: ~s~%" beta best-score)
						(when (< beta best-score)
							;(setf best-path (append best-path (list (node-state successor) ) ) ) ;consing does things - bad things
							(setf best-path successor )
							(format t "best path updated: ~s  BP: ~s~%" successor best-path)
						
							(setf best-score beta)
							(format t "min best score set: ~s ~%" best-score)
						)
				)
				
			)
            ; return (value path) list when done
			(format t "here3 ~s best: ~s ~%" best-score best-path )
            (list best-score best-path ) 
        )
    )
)

(defun deepenough (depth)
	(let ()
		(cond
			( (equal depth 0) 
				t
			)
			(t nil)
		)
	)
)
